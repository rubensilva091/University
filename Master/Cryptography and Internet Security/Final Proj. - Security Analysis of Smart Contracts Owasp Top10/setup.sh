#!/usr/bin/env bash
# setup.sh — Instala todas as ferramentas de análise de segurança
# Correr em qualquer máquina para obter um ambiente idêntico.
#
# Estratégia de isolamento:
#   Slither e Mythril partilham crytic-compile mas precisam de versões incompatíveis.
#   Usamos pipx para os instalar em ambientes virtuais isolados — cada ferramenta
#   tem o seu próprio venv mas continua acessível como comando normal no PATH.
#
#   Ferramentas instaladas via pipx : slither, mythril
#   Ferramentas instaladas via pip  : semgrep, halmos, solc-select
#   Ferramentas instaladas via npm  : solhint
#   Ferramentas instaladas via curl : aderyn, echidna, medusa (binários)
#   Ferramentas instaladas via curl : foundry (forge)
#
# Pré-requisitos:
#   - Python 3.8+ e pip
#   - Node.js e npm
#   - curl
#
# Uso:
#   ./setup.sh              # instala tudo
#   ./setup.sh --check      # apenas verifica versões instaladas

set -euo pipefail

# Fix: VIRTUAL_ENV=/usr quebra o solc-select (tenta escrever em /usr/.solc-select)
unset VIRTUAL_ENV

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

# Carregar versões
source "$ROOT_DIR/config/versions.env"

# Garantir PATH completo em ambos os modos (--check e instalação)
export PATH="$HOME/.local/bin:$HOME/.foundry/bin:$PATH"

# Modo de verificação apenas
CHECK_ONLY=false
if [[ "${1:-}" == "--check" ]]; then
    CHECK_ONLY=true
fi

# --- Funções auxiliares ---
check_version() {
    local name="$1"
    local cmd="$2"
    local version
    if version=$(eval "$cmd" 2>&1 | head -1); then
        printf "  %-14s %s\n" "$name:" "$version"
    else
        printf "  %-14s [NÃO ENCONTRADO]\n" "$name:"
    fi
}

if $CHECK_ONLY; then
    echo "=== Versões instaladas ==="
    check_version "slither"    "slither --version"
    check_version "myth"       "myth version"
    check_version "semgrep"    "semgrep --version 2>/dev/null"
    check_version "halmos"     "halmos --version"
    check_version "solc"       "solc --version"
    check_version "solhint"    "solhint --version"
    check_version "aderyn"     "aderyn --version"
    check_version "echidna"    "echidna --version"
    check_version "medusa"     "medusa --version"
    check_version "forge"      "forge --version"
    exit 0
fi

echo "=== Setup CSI — ambiente nativo de análise ==="
echo "    Versões definidas em: config/versions.env"
echo ""

# ============================================================
# 1. pipx (gestor de ambientes isolados para CLI tools)
# ============================================================
echo "[1/7] A instalar pipx..."
pip install pipx
pipx ensurepath
echo "      ✓ pipx instalado"

# ============================================================
# 2. Ferramentas com conflito crytic-compile → instalar via pipx
# ============================================================
echo ""
echo "[2/7] A instalar Slither e Mythril via pipx (ambientes isolados)..."
echo "      AVISO: Mythril compila z3 de source — pode demorar 10-30 min."
echo ""

pipx install "slither-analyzer==$SLITHER_VERSION"
echo "      ✓ slither $SLITHER_VERSION"

pipx install "mythril==$MYTHRIL_VERSION"
# Fix: mythril depende de pkg_resources que o pipx venv não expõe por default em Python 3.12+.
# Solução: symlink do pkg_resources do conda/sistema para dentro do venv isolado do mythril.
MYTH_SITE="$HOME/.local/share/pipx/venvs/mythril/lib/python3.12/site-packages"
CONDA_PKG="$(python3 -c 'import pkg_resources, os; print(os.path.dirname(pkg_resources.__file__))')"
if [[ ! -e "$MYTH_SITE/pkg_resources" && -d "$CONDA_PKG" ]]; then
    ln -sf "$CONDA_PKG" "$MYTH_SITE/pkg_resources"
fi
echo "      ✓ mythril $MYTHRIL_VERSION"

# Manticore removido: pysha3 (dependência) não compila em Python 3.12+.
# A ferramenta está abandonada desde 2022. Substituída por Mythril + Halmos.

# ============================================================
# 3. Ferramentas Python sem conflito → instalar via pip normal
# ============================================================
echo ""
echo "[3/7] A instalar Semgrep, Halmos e solc-select via pip..."
pip install \
    "semgrep==$SEMGREP_VERSION" \
    "halmos==$HALMOS_VERSION" \
    solc-select
echo "      ✓ semgrep, halmos, solc-select instalados"

# ============================================================
# 4. solc 0.8.20
# ============================================================
echo ""
echo "[4/7] A instalar solc $SOLC_VERSION..."
solc-select install "$SOLC_VERSION"
solc-select use "$SOLC_VERSION"
echo "      ✓ solc $SOLC_VERSION activo"

# ============================================================
# 5. npm tools (Solhint)
# ============================================================
echo ""
echo "[5/7] A instalar ferramentas npm..."

if ! command -v npm &>/dev/null; then
    echo "      [AVISO] npm não encontrado."
    echo "      Instalar: sudo apt-get install nodejs npm"
    echo "      Solhint será saltado."
else
    npm install -g "solhint@$SOLHINT_VERSION"
    echo "      ✓ solhint $SOLHINT_VERSION instalado"

    # SmartCheck removido: JAR requer Java 8, falha em Java 11+ (javax.xml.bind removido).
fi

# Diretório de binários locais (usado nas secções 6 e 7)
INSTALL_DIR="$HOME/.local/bin"
mkdir -p "$INSTALL_DIR"

# ============================================================
# 6. Aderyn (binário Linux)
# ============================================================
echo ""
echo "[6/7] A instalar Aderyn..."

echo "      A instalar Aderyn v$ADERYN_VERSION..."
if curl -fsSL "$ADERYN_URL" | tar -xJ -C /tmp/ 2>/dev/null && \
   cp /tmp/aderyn-x86_64-unknown-linux-gnu/aderyn "$INSTALL_DIR/aderyn" 2>/dev/null; then
    chmod +x "$INSTALL_DIR/aderyn"
    echo "      ✓ Aderyn v$ADERYN_VERSION"
else
    echo "      [ERRO] Aderyn falhou. Verificar URL em versions.env:"
    echo "        $ADERYN_URL"
fi

# ============================================================
# 7. Echidna + Medusa (binários Linux) + Foundry
# ============================================================
echo ""
echo "[7/7] A instalar Echidna, Medusa e Foundry..."

if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
    echo "      [AVISO] $INSTALL_DIR não está no PATH."
    echo "      Adicionar ao ~/.bashrc: export PATH=\"\$HOME/.local/bin:\$PATH\""
    echo "      Depois: source ~/.bashrc"
fi

echo "      A instalar Echidna v$ECHIDNA_VERSION..."
if curl -fsSL "$ECHIDNA_URL" | tar -xz -C "$INSTALL_DIR" 2>/dev/null; then
    chmod +x "$INSTALL_DIR/echidna"
    echo "      ✓ echidna v$ECHIDNA_VERSION"
else
    echo "      [ERRO] Echidna falhou. Verificar URL em versions.env:"
    echo "        $ECHIDNA_URL"
    echo "      Downloads manuais: https://github.com/crytic/echidna/releases"
fi

echo "      A instalar Medusa v$MEDUSA_VERSION..."
if curl -fsSL "$MEDUSA_URL" | tar -xz -C "$INSTALL_DIR" 2>/dev/null; then
    chmod +x "$INSTALL_DIR/medusa"
    echo "      ✓ medusa v$MEDUSA_VERSION"
else
    echo "      [ERRO] Medusa falhou. Verificar URL em versions.env:"
    echo "        $MEDUSA_URL"
    echo "      Downloads manuais: https://github.com/crytic/medusa/releases"
fi

if command -v forge &>/dev/null; then
    echo "      Foundry já instalado ($(forge --version 2>&1 | head -1))"
elif command -v foundryup &>/dev/null || [[ -x "$HOME/.foundry/bin/foundryup" ]]; then
    echo "      foundryup encontrado — a instalar forge..."
    FOUNDRYUP_BIN="$(command -v foundryup 2>/dev/null || echo "$HOME/.foundry/bin/foundryup")"
    "$FOUNDRYUP_BIN"
    export PATH="$PATH:$HOME/.foundry/bin"
else
    curl -L https://foundry.paradigm.xyz | bash
    echo ""
    echo "      [IMPORTANTE] Reiniciar o terminal e correr: foundryup"
fi

# ============================================================
# Sumário de versões
# ============================================================
echo ""
echo "=== Setup concluído — verificação de versões ==="
check_version "slither"    "slither --version"
check_version "myth"       "myth version"
check_version "semgrep"    "semgrep --version 2>/dev/null"
check_version "halmos"     "halmos --version"
check_version "solc"       "solc --version"
check_version "solhint"    "solhint --version"
check_version "aderyn"     "aderyn --version"
check_version "echidna"    "echidna --version"
check_version "medusa"     "medusa --version"
check_version "forge"      "forge --version"
echo ""
echo "Para verificar versões mais tarde: ./setup.sh --check"
echo ""
echo "NOTA — LLM (Groq/Gemini/Qwen):"
echo "  Usar: python3 scripts/llm_analyze.py --all"
echo ""
echo "NOTA — Fuzzing (Echidna/Medusa):"
echo "  Usar: ./fuzz.sh"
