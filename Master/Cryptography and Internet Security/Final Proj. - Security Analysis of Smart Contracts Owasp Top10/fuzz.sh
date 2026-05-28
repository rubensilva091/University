#!/usr/bin/env bash
# fuzz.sh — Run Echidna and Medusa on all OWASP fuzzing contracts.
# Usage:
#   ./fuzz.sh                  # all tools, all contracts
#   ./fuzz.sh --tool echidna   # echidna only
#   ./fuzz.sh --tool medusa    # medusa only
#   ./fuzz.sh --category OWASP_01  # specific category

set -euo pipefail
ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT_DIR"

export PATH="$HOME/.foundry/bin:$HOME/.local/bin:$PATH"
# VIRTUAL_ENV=/usr (set by conda) breaks crytic-compile's solc-select path resolution
unset VIRTUAL_ENV

RESULTS_DIR="$ROOT_DIR/results"
ECHIDNA_DIR="$RESULTS_DIR/echidna"
MEDUSA_DIR="$RESULTS_DIR/medusa"
SUMMARY_FILE="$RESULTS_DIR/summary/fuzz_summary.md"

mkdir -p "$ECHIDNA_DIR" "$MEDUSA_DIR" "$RESULTS_DIR/summary" "$ROOT_DIR/corpus/echidna" "$ROOT_DIR/corpus/medusa"

# ── Argument parsing ─────────────────────────────────────────────────────────
TOOL="all"
FILTER_CAT=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --tool) TOOL="$2"; shift 2 ;;
        --category) FILTER_CAT="$2"; shift 2 ;;
        *) echo "Unknown argument: $1"; exit 1 ;;
    esac
done

# ── Tool availability ─────────────────────────────────────────────────────────
ECHIDNA_OK=false
MEDUSA_OK=false

if command -v echidna &>/dev/null; then ECHIDNA_OK=true; fi
if command -v medusa  &>/dev/null; then MEDUSA_OK=true; fi

tool_selected() {
    [[ "$TOOL" == "all" || "$TOOL" == "$1" ]]
}

echo "============================================================"
echo " Fuzzing — $(date '+%Y-%m-%d %H:%M')"
echo " Echidna: $ECHIDNA_OK | Medusa: $MEDUSA_OK"
echo "============================================================"

# ── Contract list ─────────────────────────────────────────────────────────────
# Lista explícita para manter ordem estável entre execuções e relatórios.
# Formato: OWASP:ContratoDeFuzz:FicheiroSolidity
declare -a FUZZ_CONTRACTS=(
    "OWASP_01:OWASP_01_VulnFuzz:test/OWASP_01_Fuzz.sol"
    "OWASP_01:OWASP_01_FixedFuzz:test/OWASP_01_Fuzz.sol"
    "OWASP_01:OWASP_01_Vuln2Fuzz:test/OWASP_01_Fuzz.sol"
    "OWASP_01:OWASP_01_Fixed2Fuzz:test/OWASP_01_Fuzz.sol"
    "OWASP_02:OWASP_02_VulnFuzz:test/OWASP_02_Fuzz.sol"
    "OWASP_02:OWASP_02_FixedFuzz:test/OWASP_02_Fuzz.sol"
    "OWASP_03:OWASP_03_VulnFuzz:test/OWASP_03_Fuzz.sol"
    "OWASP_03:OWASP_03_FixedFuzz:test/OWASP_03_Fuzz.sol"
    "OWASP_04:OWASP_04_VulnFuzz:test/OWASP_04_Fuzz.sol"
    "OWASP_04:OWASP_04_FixedFuzz:test/OWASP_04_Fuzz.sol"
    "OWASP_05:OWASP_05_VulnFuzz:test/OWASP_05_Fuzz.sol"
    "OWASP_05:OWASP_05_FixedFuzz:test/OWASP_05_Fuzz.sol"
    "OWASP_05:OWASP_05_Vuln2Fuzz:test/OWASP_05_Fuzz.sol"
    "OWASP_05:OWASP_05_Fixed2Fuzz:test/OWASP_05_Fuzz.sol"
    "OWASP_06:OWASP_06_VulnFuzz:test/OWASP_06_Fuzz.sol"
    "OWASP_06:OWASP_06_FixedFuzz:test/OWASP_06_Fuzz.sol"
    "OWASP_06:OWASP_06_Vuln2Fuzz:test/OWASP_06_Fuzz.sol"
    "OWASP_06:OWASP_06_Fixed2Fuzz:test/OWASP_06_Fuzz.sol"
    "OWASP_07:OWASP_07_VulnFuzz:test/OWASP_07_Fuzz.sol"
    "OWASP_07:OWASP_07_FixedFuzz:test/OWASP_07_Fuzz.sol"
    "OWASP_08:OWASP_08_VulnFuzz:test/OWASP_08_Fuzz.sol"
    "OWASP_08:OWASP_08_FixedFuzz:test/OWASP_08_Fuzz.sol"
    "OWASP_08:OWASP_08_Vuln2Fuzz:test/OWASP_08_Fuzz.sol"
    "OWASP_08:OWASP_08_Fixed2Fuzz:test/OWASP_08_Fuzz.sol"
    "OWASP_09:OWASP_09_VulnFuzz:test/OWASP_09_Fuzz.sol"
    "OWASP_09:OWASP_09_FixedFuzz:test/OWASP_09_Fuzz.sol"
    "OWASP_09:OWASP_09_Vuln2Fuzz:test/OWASP_09_Fuzz.sol"
    "OWASP_09:OWASP_09_Fixed2Fuzz:test/OWASP_09_Fuzz.sol"
    "OWASP_10:OWASP_10_VulnFuzz:test/OWASP_10_Fuzz.sol"
    "OWASP_10:OWASP_10_FixedFuzz:test/OWASP_10_Fuzz.sol"
)

# ── Summary header ────────────────────────────────────────────────────────────
cat > "$SUMMARY_FILE" <<EOF
# Sumário de Fuzzing — $(date '+%Y-%m-%d %H:%M')
# Cobre: Echidna, Medusa
# Para relatório unificado: python3 scripts/build_report.py

| Contrato | Variante | Echidna | Medusa |
|----------|----------|---------|--------|
EOF

# ── Main loop ─────────────────────────────────────────────────────────────────
for entry in "${FUZZ_CONTRACTS[@]}"; do
    IFS=: read -r owasp contract_name sol_file <<< "$entry"

    [[ -n "$FILTER_CAT" && "$owasp" != "$FILTER_CAT" ]] && continue

    # Fixed2/Fixed continuam a ser classificados como "Fixed" para o sumário.
    VARIANT="Vuln"; [[ "$contract_name" == *Fixed* ]] && VARIANT="Fixed"
    echo ""
    echo "  [$owasp] $contract_name ($VARIANT)"

    ECHIDNA_STATUS="—"
    MEDUSA_STATUS="—"

    # ── Echidna ──────────────────────────────────────────────────────────────
    if tool_selected "echidna" && $ECHIDNA_OK; then
        OUT_LOG="$ECHIDNA_DIR/${owasp}_${contract_name}.log"
        echo "     [Echidna] a correr..."
        _t0=$SECONDS
        if echidna "$sol_file" \
               --contract "$contract_name" \
               --config config/echidna.yaml \
               > "$OUT_LOG" 2>&1; then
            ECHIDNA_STATUS="OK"
        else
            # Check if property was falsified (expected for Vuln contracts).
            # Match "failed!" (property result) not corpus-replay "failed." warnings.
            if grep -qE "^echidna_.*failed!|: failed!|violated" "$OUT_LOG" 2>/dev/null; then
                ECHIDNA_STATUS="VULN"
            else
                ECHIDNA_STATUS="ERR"
            fi
        fi
        _elapsed=$(( SECONDS - _t0 ))
        echo "     [Echidna] → $OUT_LOG (${_elapsed}s, status=$ECHIDNA_STATUS)"
    fi

    # ── Medusa ───────────────────────────────────────────────────────────────
    if tool_selected "medusa" && $MEDUSA_OK; then
        OUT_LOG="$MEDUSA_DIR/${owasp}_${contract_name}.log"
        echo "     [Medusa] a correr..."
        _t0=$SECONDS

        # Medusa não expõe targetContractsBalances por CLI, então ajustamos via JSON temporário.
        # Isto permite manter uma config base comum e apenas customizar o que muda por contrato.
        MEDUSA_TMP="/tmp/medusa_${contract_name}.json"
        python3 - <<PYEOF
import json
with open("config/medusa.json") as f:
    cfg = json.load(f)
cfg["fuzzing"]["targetContractsBalances"] = ["100000000000000000000"]
cfg["fuzzing"]["corpusDirectory"] = "$ROOT_DIR/corpus/medusa/$contract_name"
with open("$MEDUSA_TMP", "w") as f:
    json.dump(cfg, f, indent="\t")
PYEOF

        ABS_SOL="$ROOT_DIR/$sol_file"
        if medusa fuzz \
               --config "$MEDUSA_TMP" \
               --compilation-target "$ABS_SOL" \
               --target-contracts "$contract_name" \
               > "$OUT_LOG" 2>&1; then
            MEDUSA_STATUS="OK"
        else
            # "FAILED" significa propriedade quebrada (esperado em contratos Vuln), não crash.
            if grep -q "\[FAILED\]" "$OUT_LOG" 2>/dev/null; then
                MEDUSA_STATUS="VULN"
            elif grep -q "Failed to initialize\|Failed to compile\|failed to run" "$OUT_LOG" 2>/dev/null; then
                MEDUSA_STATUS="ERR"
            else
                MEDUSA_STATUS="OK"
            fi
        fi
        _elapsed=$(( SECONDS - _t0 ))
        echo "     [Medusa]  → $OUT_LOG (${_elapsed}s, status=$MEDUSA_STATUS)"
    fi

    echo "| $contract_name | $VARIANT | $ECHIDNA_STATUS | $MEDUSA_STATUS |" >> "$SUMMARY_FILE"
done

echo ""
echo "============================================================"
echo " Fuzzing concluído. Sumário: $SUMMARY_FILE"
echo " Para relatório completo: python3 scripts/build_report.py"
echo "============================================================"
