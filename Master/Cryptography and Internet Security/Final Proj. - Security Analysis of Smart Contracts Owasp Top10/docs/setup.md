# Setup e Utilização — CSI Project

## Objetivo

Este documento descreve como preparar o ambiente e executar o pipeline completo do projeto sem precisar de abrir scripts.

## Pré-requisitos

Confirmar:

| Requisito | Verificar |
|-----------|-----------|
| Python 3.8+ | `python3 --version` |
| pipx | `pipx --version` |
| Node + npm | `node --version && npm --version` |
| curl | `curl --version` |
| git | `git --version` |

Nota importante:
- se existir `VIRTUAL_ENV=/usr`, o `solc-select` pode falhar por permissões
- os scripts principais já fazem `unset VIRTUAL_ENV`

## Comandos

### 1) Instalação recomendada

```bash
./setup.sh
```

### 2) Verificação do ambiente

```bash
./setup.sh --check
```

### 3) Execução canónica

```bash
./analyze.sh
./fuzz.sh
.venv/bin/python scripts/llm_analyze.py --all
.venv/bin/python scripts/build_report.py
```

Tempos aproximados (30 contratos):

| Etapa | Tempo típico |
|------|---------------|
| `./setup.sh --check` | ~1-2 min |
| `./analyze.sh` | ~25-45 min (Mythril domina) |
| `./fuzz.sh` | ~20-40 min (depende de limites/config) |
| `llm_analyze.py --all` | ~5-20 min (depende de API/quota) |
| `build_report.py` | <1 min |

### 4) Execução por partes (opcional)

```bash
# Estática/simbólica por ferramenta ou categoria
./analyze.sh --tool slither,mythril --category OWASP_08

# Fuzzing por ferramenta ou categoria
./fuzz.sh --tool echidna --category OWASP_08

# LLM por modelo/categoria
.venv/bin/python scripts/llm_analyze.py --model gemini --category OWASP_08
```

## Output esperado

Após execução:

```text
results/slither/      results/mythril/     results/halmos/
results/aderyn/       results/semgrep/     results/solhint/
results/echidna/      results/medusa/
results/llm/OWASP_XX/
results/summary/summary.md
results/summary/fuzz_summary.md
results/summary/report.md
results/summary/report.csv
```

## Erros comuns e correção

| Problema | Correção |
|----------|----------|
| `GEMINI_API_KEY` em falta | criar `.env` a partir de `.env.example` e preencher chave |
| Erro `pkg_resources` no Mythril | correr `./setup.sh` novamente (aplica fix automático) |
| `solc-select` permission denied | garantir `unset VIRTUAL_ENV` no shell |
| Ferramenta não encontrada (`aderyn`, `echidna`, `medusa`) | garantir `~/.local/bin` no `PATH` |
| Dependências Python não encontradas | usar `.venv/bin/python ...` nos scripts Python |

## Notas de setup manual

Se for necessário instalar manualmente, usar o documento de versões em `config/versions.env` e manter:
- Slither e Mythril em `pipx` (isolados)
- Semgrep/Halmos/solc-select no ambiente do projeto
- Foundry instalado para Halmos

## Próximo passo

Depois de confirmar que o pipeline corre, seguir para [docs/tabela.md](docs/tabela.md) para interpretar resultados e para `results/summary/report.md` para fechar pendências antes do relatório.
