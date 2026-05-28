#!/usr/bin/env bash
# analyze.sh — Corre ferramentas de análise de segurança em contratos Solidity
# Guarda outputs em results/<ferramenta>/
#
# Ferramentas suportadas (flag --tool):
#   Estáticas  : slither, aderyn, semgrep, solhint
#   Simbólicas : mythril, halmos
# Removidas por incompatibilidade com stacks modernas:
#   Manticore  — pysha3 não compila em Python 3.12+ (abandonado desde 2022)
#   SmartCheck — JAR requer Java 8, falha em Java 11+ (abandonado)
#
# Fuzzing (Echidna, Medusa) e LLM são feitos fora deste script.
# Ver: docs/setup.md
#
# Uso:
#   ./analyze.sh                                         # tudo em tudo
#   ./analyze.sh --tool slither                          # só Slither
#   ./analyze.sh --tool slither,aderyn,semgrep           # várias ferramentas
#   ./analyze.sh --category OWASP_08                     # só OWASP_08
#   ./analyze.sh --category OWASP_01,OWASP_02,OWASP_03  # várias categorias
#   ./analyze.sh --contract contracts/OWASP_08/MVE_O8_Vuln.sol
#   ./analyze.sh --tool mythril --category OWASP_08      # combinação

set -euo pipefail

# Fix: VIRTUAL_ENV=/usr quebra o solc-select (tenta escrever em /usr/.solc-select)
unset VIRTUAL_ENV

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

# Garantir PATH completo (forge necessário para halmos)
export PATH="$HOME/.foundry/bin:$HOME/.local/bin:$PATH"

CONTRACTS_DIR="$ROOT_DIR/contracts"
RESULTS_DIR="$ROOT_DIR/results"

# ============================================================
# Parser de argumentos
# ============================================================
SELECTED_TOOLS=""      # vazio = todas
SELECTED_CATEGORIES="" # vazio = todas
SELECTED_CONTRACTS=""  # vazio = todas

while [[ $# -gt 0 ]]; do
    case "$1" in
        --tool)
            SELECTED_TOOLS="$2"; shift 2 ;;
        --category)
            SELECTED_CATEGORIES="$2"; shift 2 ;;
        --contract)
            SELECTED_CONTRACTS="$2"; shift 2 ;;
        # Retrocompatibilidade com flags antigas
        --slither)  SELECTED_TOOLS="${SELECTED_TOOLS:+$SELECTED_TOOLS,}slither"; shift ;;
        --mythril)  SELECTED_TOOLS="${SELECTED_TOOLS:+$SELECTED_TOOLS,}mythril"; shift ;;
        --solhint)  SELECTED_TOOLS="${SELECTED_TOOLS:+$SELECTED_TOOLS,}solhint"; shift ;;
        *)
            echo "Argumento desconhecido: $1"
            echo ""
            echo "Uso: ./analyze.sh [--tool TOOL[,TOOL...]] [--category OWASP_XX[,...]] [--contract FILE[,...]]"
            echo ""
            echo "Ferramentas disponíveis: slither, aderyn, semgrep, solhint, mythril, halmos"
            exit 1
            ;;
    esac
done

# Funções de filtro
# O truque ",valores," evita matches parciais (ex: OWASP_01 dentro de OWASP_010).
tool_selected()     { [[ -z "$SELECTED_TOOLS"      || ",$SELECTED_TOOLS,"      == *",$1,"* ]]; }
category_selected() { [[ -z "$SELECTED_CATEGORIES" || ",$SELECTED_CATEGORIES," == *",$1,"* ]]; }
contract_selected() { [[ -z "$SELECTED_CONTRACTS"  || ",$SELECTED_CONTRACTS,"  == *",$1,"* ]]; }

# ============================================================
# Verificar ferramentas disponíveis
# ============================================================
SLITHER_OK=false;    command -v slither    >/dev/null 2>&1 && SLITHER_OK=true    || echo "[AVISO] slither não encontrado    — pipx install slither-analyzer"
ADERYN_OK=false;     command -v aderyn     >/dev/null 2>&1 && ADERYN_OK=true     || echo "[AVISO] aderyn não encontrado     — ver setup.sh (binário em ~/.local/bin)"
SEMGREP_OK=false;    command -v semgrep    >/dev/null 2>&1 && SEMGREP_OK=true    || echo "[AVISO] semgrep não encontrado    — pip install semgrep"
SOLHINT_OK=false;    command -v solhint    >/dev/null 2>&1 && SOLHINT_OK=true    || echo "[AVISO] solhint não encontrado    — npm install -g solhint"
MYTHRIL_OK=false;    command -v myth       >/dev/null 2>&1 && MYTHRIL_OK=true    || echo "[AVISO] myth não encontrado       — pipx install mythril"
HALMOS_OK=false;     command -v halmos     >/dev/null 2>&1 && HALMOS_OK=true     || echo "[AVISO] halmos não encontrado     — pip install halmos"

echo ""
echo "=== Ferramentas disponíveis ==="
printf "  %-12s %s\n" "Slither:"    "$SLITHER_OK"
printf "  %-12s %s\n" "Aderyn:"     "$ADERYN_OK"
printf "  %-12s %s\n" "Semgrep:"    "$SEMGREP_OK"
printf "  %-12s %s\n" "Solhint:"    "$SOLHINT_OK"
printf "  %-12s %s\n" "Mythril:"    "$MYTHRIL_OK"
printf "  %-12s %s\n" "Halmos:"     "$HALMOS_OK"
echo ""

if [[ -n "$SELECTED_TOOLS" ]];      then echo "  Filtro ferramentas  : $SELECTED_TOOLS";      fi
if [[ -n "$SELECTED_CATEGORIES" ]]; then echo "  Filtro categorias   : $SELECTED_CATEGORIES"; fi
if [[ -n "$SELECTED_CONTRACTS" ]];  then echo "  Filtro contratos    : $SELECTED_CONTRACTS";  fi
echo ""

# ============================================================
# Criar estrutura de resultados
# ============================================================
mkdir -p "$RESULTS_DIR"/{slither,aderyn,semgrep,solhint,mythril,halmos,echidna,medusa,llm,summary}

# ============================================================
# Ficheiro de sumário
# ============================================================
SUMMARY_FILE="$RESULTS_DIR/summary/summary.md"
cat > "$SUMMARY_FILE" <<EOF
# Sumário de Análise Estática/Simbólica — $(date '+%Y-%m-%d %H:%M')
# Cobre: Slither, Aderyn, Semgrep, Solhint, Mythril, Halmos
# Para relatório unificado (inclui LLM + Fuzzing): python3 scripts/build_report.py

| Contrato | Variante | Slither | Aderyn | Semgrep | Solhint | Mythril | Halmos |
|----------|----------|---------|--------|---------|---------|---------|--------|
EOF

# ============================================================
# Loop por contratos
# ============================================================
for owasp_dir in "$CONTRACTS_DIR"/OWASP_*/; do
    owasp=$(basename "$owasp_dir")

    # Filtro de categoria
    category_selected "$owasp" || continue

    echo "========================================"
    echo "  Categoria: $owasp"
    echo "========================================"

    for contract in "$owasp_dir"*_Vuln*.sol "$owasp_dir"*_Fixed*.sol; do
        [[ -f "$contract" ]] || continue
        name=$(basename "$contract" .sol)

        # Filtro de contrato
        contract_selected "$name" || continue

        echo ""
        echo "  >> $name"

        # N/A significa "não executado" (filtro ou ferramenta indisponível).
        SLITHER_STATUS="N/A"
        ADERYN_STATUS="N/A"
        SEMGREP_STATUS="N/A"
        SOLHINT_STATUS="N/A"
        MYTHRIL_STATUS="N/A"
        HALMOS_STATUS="N/A"

        # --- Slither ---
        if tool_selected "slither" && $SLITHER_OK; then
            OUT="$RESULTS_DIR/slither/${owasp}_${name}.txt"
            echo "     [Slither] a analisar..."
            slither "$contract" \
                --disable-color \
                --json "$RESULTS_DIR/slither/${owasp}_${name}.json" \
                > "$OUT" 2>&1 \
                && SLITHER_STATUS="OK" || SLITHER_STATUS="ISSUES"
            echo "     [Slither] → $OUT"
        fi

        # --- Aderyn ---
        # Aderyn exige um diretório como ROOT (não aceita ficheiro individual).
        # Usar -i para filtrar ao contrato específico; -o escreve para .md (obrigatório).
        if tool_selected "aderyn" && $ADERYN_OK; then
            OUT="$RESULTS_DIR/aderyn/${owasp}_${name}.md"
            OUT_LOG="$RESULTS_DIR/aderyn/${owasp}_${name}.log"
            echo "     [Aderyn] a analisar..."
            aderyn "$(dirname "$contract")" \
                -i "${name}.sol" \
                -o "$OUT" \
                > "$OUT_LOG" 2>&1 \
                && ADERYN_STATUS="OK" || ADERYN_STATUS="ISSUES"
            echo "     [Aderyn] → $OUT"
        fi

        # --- Semgrep ---
        if tool_selected "semgrep" && $SEMGREP_OK; then
            OUT="$RESULTS_DIR/semgrep/${owasp}_${name}.txt"
            echo "     [Semgrep] a analisar..."
            semgrep --config=p/smart-contracts "$contract" \
                > "$OUT" 2>&1 \
                && SEMGREP_STATUS="OK" || SEMGREP_STATUS="ISSUES"
            echo "     [Semgrep] → $OUT"
        fi

        # --- Solhint ---
        if tool_selected "solhint" && $SOLHINT_OK; then
            OUT="$RESULTS_DIR/solhint/${owasp}_${name}.txt"
            echo "     [Solhint] a analisar..."
            solhint "$contract" \
                --config .solhint.json \
                > "$OUT" 2>&1 \
                && SOLHINT_STATUS="OK" || SOLHINT_STATUS="ISSUES"
            echo "     [Solhint] → $OUT"
        fi

        # --- Mythril ---
        if tool_selected "mythril" && $MYTHRIL_OK; then
            OUT="$RESULTS_DIR/mythril/${owasp}_${name}.txt"
            echo "     [Mythril] a analisar (pode demorar até 60s)..."
            myth analyze "$contract" \
                --solv 0.8.20 \
                --execution-timeout 60 \
                > "$OUT" 2>&1 \
                && MYTHRIL_STATUS="OK" || MYTHRIL_STATUS="ISSUES"
            echo "     [Mythril] → $OUT"
        fi

        # --- Halmos ---
        if tool_selected "halmos" && $HALMOS_OK; then
            OUT="$RESULTS_DIR/halmos/${owasp}_${name}.txt"
            echo "     [Halmos] a analisar..."
            # Halmos recebe o nome do contrato (não o caminho do ficheiro).
            halmos --contract "$name" \
                > "$OUT" 2>&1 \
                && HALMOS_STATUS="OK" || HALMOS_STATUS="ISSUES"
            echo "     [Halmos] → $OUT"
        fi

        # Adicionar linha ao sumário
        VARIANT="Vuln"; [[ "$name" == *Fixed* ]] && VARIANT="Fixed"
        echo "| $name | $VARIANT | $SLITHER_STATUS | $ADERYN_STATUS | $SEMGREP_STATUS | $SOLHINT_STATUS | $MYTHRIL_STATUS | $HALMOS_STATUS |" >> "$SUMMARY_FILE"
    done
done

# ============================================================
# Halmos — check_* contracts (test/OWASP_01_Halmos + OWASP_09_Halmos)
# Estes contratos têm funções check_* para verificação formal.
# Correm separadamente do loop principal (que analisa os MVE originais).
# ============================================================
if tool_selected "halmos" && $HALMOS_OK; then
    echo ""
    echo "  [Halmos] A correr verificação formal (check_* contracts)..."
    declare -a HALMOS_CHECKS=(
        "OWASP_01:OWASP_01_VulnCheck"
        "OWASP_01:OWASP_01_FixedCheck"
        "OWASP_09:OWASP_09_VulnCheck"
        "OWASP_09:OWASP_09_FixedCheck"
    )
    for entry in "${HALMOS_CHECKS[@]}"; do
        owasp="${entry%%:*}"
        cname="${entry##*:}"
        OUT="$RESULTS_DIR/halmos/${owasp}_${cname}.txt"
        echo "     [Halmos] $cname..."
        halmos --contract "$cname" \
            > "$OUT" 2>&1 \
            && echo "     [Halmos] PASS → $OUT" \
            || echo "     [Halmos] FAIL/COUNTEREXAMPLE → $OUT"
    done
fi

echo ""
echo "========================================"
echo "  Análise concluída!"
echo "  Resultados em: $RESULTS_DIR/"
echo "  Sumário em:    $SUMMARY_FILE"
echo "========================================"
echo ""
echo "NOTA — Outros scripts de análise:"
echo "  LLM     : python3 scripts/llm_analyze.py --all"
echo "            → results/llm/OWASP_XX/MVE_OX_Variant_<modelo>.txt"
echo ""
echo "  Fuzzing : ./fuzz.sh"
echo "            → results/echidna/ + results/medusa/ + results/summary/fuzz_summary.md"
echo ""
echo "  Relatório unificado: python3 scripts/build_report.py"
echo "            → results/summary/report.md + report.csv"
echo ""
echo "  Ver docs/setup.md para instruções completas."
