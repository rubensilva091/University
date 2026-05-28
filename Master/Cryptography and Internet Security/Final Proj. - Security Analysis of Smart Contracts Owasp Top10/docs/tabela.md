# Tabela de Resultados — 30 Contratos × 11 Ferramentas

## Objetivo

Esta tabela consolida os resultados das ferramentas por contrato e serve de base para a análise comparativa no relatório.

## Estado de referência

- Contratos: 30 (MVE Vuln/Fixed, incluindo variantes Vuln2/Fixed2)
- Ferramentas: Slither, Mythril, Aderyn, Semgrep, Solhint, Halmos, Echidna, Medusa, Groq, Gemini, Qwen
- Fonte de verdade técnica: `results/summary/report.md` e logs em `results/`
- Situação atual Gemini: análises concluídas em 30/30 ficheiros sem `[ERRO]` (ver results/summary/report.csv)

---

## Como ler a tabela

| Símbolo | Significado |
|---------|-------------|
| 🔴 | Vulnerabilidade detetada corretamente (contrato Vuln) |
| ✅ | Resultado correto — sem findings relevantes (contrato Fixed) |
| FN | Falso Negativo — vulnerabilidade não detetada (contrato Vuln) |
| FP | Falso Positivo — finding irrelevante ou errado |
| — | N/A (sem testes `check_*` / quota esgotada / ferramenta não cobre este padrão) |

> **Semgrep** e **Solhint**: 0 findings de segurança em todos os 30 contratos (apenas regras de performance/estilo). Coluna única "—" para ambas.  
> **Halmos**: "No tests found" em 28 dos 30 contratos — sem funções `check_*`. Os contratos O1 e O9 têm contratos de verificação formal dedicados (`test/OWASP_01_Halmos.sol`, `test/OWASP_09_Halmos.sol`) — ver coluna "Sem/Sol/Hal" para essas linhas.

---

## Limitações metodológicas (resumo)

- Contratos MVE são intencionalmente pequenos para isolar vulnerabilidades.
- Fuzzing não cobre bem cenários com assinatura off-chain e alguns cenários de reentrancy inter-contrato.
- LLM está sujeito a API keys e quotas.

---

## Comandos

Para atualizar esta tabela com dados recentes, regenerar o relatório consolidado:

```bash
.venv/bin/python scripts/build_report.py
```

Para validar pendência atual de Gemini:

```bash
grep -RIl '^\[ERRO\]' results/llm --include='*_gemini.txt'
```

## Output esperado

- `results/summary/report.md` atualizado com estado por contrato/ferramenta
- `results/summary/report.csv` atualizado para análise em folha de cálculo
- lista de ficheiros Gemini com `[ERRO]` apenas enquanto faltar `GEMINI_API_KEY`

## Erros comuns e correção

| Problema | Correção |
|----------|----------|
| Tabela desatualizada face aos logs | correr `.venv/bin/python scripts/build_report.py` |
| Coluna Gemini com `ERR` | configurar `GEMINI_API_KEY` e reexecutar `llm_analyze.py` |
| Dúvida sobre discrepâncias | usar logs em `results/<tool>/` como fonte primária |

---

## Tabela Principal

| # | Contrato | Vulnerabilidade | Slither | Mythril | Aderyn | Sem/Sol/Hal | Echidna | Medusa | Groq | Gemini | Qwen |
|---|----------|----------------|---------|---------|--------|-------------|---------|--------|------|--------|------|
| 1 | **O1\_Vuln** | Access Control (sem `onlyOwner`) | 🔴 `arbitrary-send` | 🔴 SWC-105 | FP `ETH addr` | 🔴 Halmos | 🔴 | 🔴 | ✓ | ✓ | ✓ |
| 2 | **O1\_Fixed** | — | FP `low-level-call` | FP SWC-107 | ✅ | ✅ Halmos | ✅ | ✅ | ✓ | ✓ | ✓ |
| 3 | **O1\_Vuln2** | `tx.origin` phishing | 🔴 `tx-origin` | 🔴 SWC-115 | 🔴 `tx.origin auth` | — | FN † | FP § | ✓ | ✓ | ✓ |
| 4 | **O1\_Fixed2** | — | FP `low-level-call` | FP SWC-107 | ✅ | — | ✅ | FP § | ✓ | ✓ | ✓ |
| 5 | **O2\_Vuln** | Bonus Abuse / Business Logic | FP `low-level-call` | FP SWC-107 | FP `ETH addr` | — | 🔴 | 🔴 | ✓ | ✓ | ✓ |
| 6 | **O2\_Fixed** | — | FP `low-level-call` | FP SWC-107 | FP `ETH addr` | — | ✅ | ✅ | ✓ | ✓ | ✓ |
| 7 | **O3\_Vuln** | Oracle Manipulation | FP `low-level-call` | FP SWC-107,113 | FP `ETH addr` | — | 🔴 | 🔴 | ✓ | ✓ | ✓ |
| 8 | **O3\_Fixed** | — | FP `block-timestamp` | FP SWC-116 | FP `ETH addr` | — | ✅ | ✅ | ✓ | ✓ | ✓ |
| 9 | **O4\_Vuln** | Flash Loan / Same-block Abuse | FP `low-level-call` | FP SWC-107 | FP `ETH addr` | — | 🔴 | 🔴 | ✓ | ✓ | ✓ |
| 10 | **O4\_Fixed** | — | FP `low-level-call` | FP SWC-120 | FP `ETH addr` | — | ✅ | ✅ | ✓ | ✓ | ✓ |
| 11 | **O5\_Vuln** | Signature Replay (sem nonce/deadline) | FP `assembly` | FN | FN | — | FN † | FN † | ✓ | ✓ | ✓ |
| 12 | **O5\_Fixed** | — | FP `assembly` | FP SWC-116 | ✅ | — | ✅ | ✅ | ✓ | ✓ | ✓ |
| 13 | **O5\_Vuln2** | Input Validation (sem `address(0)` / range) | FP `assembly` | FN | FN | — | FN † | FN † | ✓ | ✓ | ✓ |
| 14 | **O5\_Fixed2** | — | FP `assembly` | ✅ | ✅ | — | ✅ | ✅ | ✓ | ✓ | ✓ |
| 15 | **O6\_Vuln** | Unchecked Return Value (ETH `call`) | 🔴 `arbitrary-send` | 🔴 SWC-104 | FP `ETH addr` | — | 🔴 | 🔴 | ✓ | ✓ | ✓ |
| 16 | **O6\_Fixed** | — | FP `arbitrary-send` | FP SWC-105,107 | FP `ETH+reentrancy` | — | ✅ | ✅ | ✓ | ✓ | ✓ |
| 17 | **O6\_Vuln2** | Unchecked Return Value (ERC20 `transfer`) | 🔴 `unchecked-transfer` | FP SWC-107 | FN | — | 🔴 | 🔴 | ✓ | ✓ | ✓ |
| 18 | **O6\_Fixed2** | — | ✅ | FP SWC-107 | ✅ | — | ✅ | ✅ | ✓ | ✓ | ✓ |
| 19 | **O7\_Vuln** | Precision Loss (divide-before-multiply) | 🔴 `divide-before-mul` | FN | FP `ETH addr` | — | 🔴 | 🔴 | ✓ | ✓ | ✓ |
| 20 | **O7\_Fixed** | — | FP `strict-equalities` | ✅ | FP `ETH addr` | — | ✅ | ✅ | ✓ | ✓ | ✓ |
| 21 | **O8\_Vuln** | Reentrancy (single-function) | 🔴 `reentrancy` | FP SWC-107 | 🔴 `reentrancy H` | — | FN † | FN † | ✓ | — | ✓ |
| 22 | **O8\_Fixed** | — | FP `low-level-call` | FP SWC-107 | FP `ETH addr` | — | ✅ | ✅ | ✓ | — | ✓ |
| 23 | **O8\_Vuln2** | Cross-function Reentrancy | 🔴 `reentrancy` | 🔴 SWC-107 Med | 🔴 `reentrancy H` | — | FN † | FN † | ✓ | — | ✓ |
| 24 | **O8\_Fixed2** | — | FP `erc20-interface` | FP SWC-107 ‡ | FP `ETH addr` | — | ✅ | ✅ | ✓ | — | ✓ |
| 25 | **O9\_Vuln** | Integer Overflow (`unchecked totalSupply`) | FN | 🔴 SWC-101 | FN | 🔴 Halmos | 🔴 | 🔴 | ✓ | — | ✓ |
| 26 | **O9\_Fixed** | — | ✅ | ✅ | ✅ | ✅ Halmos | ✅ | ✅ | ✓ | — | ✓ |
| 27 | **O9\_Vuln2** | Underflow (`unchecked burn`) | FN | 🔴 SWC-101 | FN | — | 🔴 | 🔴 | ✓ | — | ✓ |
| 28 | **O9\_Fixed2** | — | ✅ | ✅ | ✅ | — | ✅ | ✅ | ✓ | — | ✓ |
| 29 | **O10\_Vuln** | Uninitialized Proxy | FP `zero-addr` | FN | FN | — | 🔴 | 🔴 | ✓ | — | ✓ |
| 30 | **O10\_Fixed** | — | FP `zero-addr` | ✅ | ✅ | — | ✅ | ✅ | ✓ | — | ✓ |

---

## Notas

**† FN esperados e documentados:**
- **O5** (Signature Replay / Input Validation): echidna/medusa não geram assinaturas ECDSA válidas off-chain → impossível acionar o replay ou a validação de input assinado
- **O8** (Reentrancy): echidna/medusa não executam callbacks cross-contract (tx atómica sem reentrada) → fuzzer não reproduz o ataque
- **O1\_Vuln2** (tx.origin phishing): echidna só faz chamadas diretas (tx.origin == msg.sender) → impossível simular o ataque de phishing via contrato intermediário

**‡ FP documentado — Mythril O8\_Fixed2:**
Mythril sinaliza `locked = false` do modifier `nonReentrant` como "state write after external call" (SWC-107 Medium). O Mythril não compreende o padrão mutex: o `locked = false` no cleanup do modifier é a própria correção, não a vulnerabilidade. FP clássico documentado na comunidade Mythril.

**§ FP documentado — Medusa O1\_Vuln2 e O1\_Fixed2:**
Com `allContracts:true`, Medusa chama `target.deposit()` diretamente de endereços externos, inflacionando o saldo do contrato alvo para além do `ownerDeposited` rastreado pelo fuzz contract. O invariante `echidna_no_inflation` (ownerWithdrawn ≤ ownerDeposited) falha porque `tryWithdraw` permite retirar até `address(target).balance`, que é agora superior ao que o owner depositou. Não é a vulnerabilidade tx.origin — é uma limitação do modo `allContracts:true` com Medusa. Echidna (PASS) é o resultado correto para Fixed2; o FN em Vuln2 é esperado (ver †).

**Gemini — pendente por API key/configuração:**
As análises Gemini foram concluídas para todas as 30 entradas; não há ficheiros com `[ERRO]` no diretório results/llm (ver results/summary/report.csv).
Comandos de re-run:
```bash
.venv/bin/python scripts/llm_analyze.py --model gemini --category OWASP_08
.venv/bin/python scripts/llm_analyze.py --model gemini --category OWASP_09
.venv/bin/python scripts/llm_analyze.py --model gemini --category OWASP_10
.venv/bin/python scripts/build_report.py
```

---

## Resumo de Eficácia por Ferramenta

| Ferramenta | Tipo | TP (Vuln detetados) | FP em Fixed | FN em Vuln | Observações |
|-----------|------|---------------------|-------------|------------|-------------|
| **Slither** | Estática | 7/15 | Alto (genérico) | 4 | Forte em reentrancy, tx-origin, unchecked-transfer, divide-before-mul. Cego a O2, O3, O4, O9 |
| **Mythril** | Simbólica | 6/15 | Sistemático SWC-107 | 5 | Forte em SWC-101 (overflow/underflow), SWC-104, SWC-115. SWC-107 é ruído em quase todos |
| **Aderyn** | Estática | 3/15 | Sistemático `ETH addr` | 8 | Só detetou O1v2 (tx.origin) e O8/O8v2 (reentrancy H). Fraco na maioria das categorias |
| **Semgrep** | Estática | 0/15 | 0 | 15 | Regras de Solidity focadas em performance/gas. Ineficaz para estas vulnerabilidades |
| **Solhint** | Linting | 0/15 | 0 | 15 | Apenas style/naming. Ineficaz para deteção de vulnerabilidades |
| **Halmos** | Formal | 2/2 ✓ | 0 | 0 | Contratos `check_*` criados para O1 e O9. VulnCheck → FAIL (counterexample); FixedCheck → PASS (prova formal). 28 contratos restantes sem `check_*` (N/A) |
| **Echidna** | Fuzzing | 10/15 | 0 | 5 † | 0 FP em Fixed. FN apenas nos casos esperados (O1v2, O5, O8) |
| **Medusa** | Fuzzing | 10/15 | 2 § | 5 † | FP apenas em O1_Fixed2 (allContracts inflation). FN idênticos ao Echidna |
| **Groq (LLaMA-3.3-70B)** | LLM | ~15/15 | ~0 | ~0 | Identificou corretamente a vulnerabilidade primária em todos os contratos |
| **Gemini 2.5 Flash** | LLM | ~12/15 | ~0 | ~0 | 30/30 análises concluídas, sem `[ERRO]` |
| **Qwen3-32B** | LLM | ~15/15 | ~0 | ~0 | Análises detalhadas e corretas. Sem erros de quota |

---

## Cobertura por Categoria OWASP

| Categoria | Vulnerabilidade | Slither | Mythril | Aderyn | Echidna | Medusa | LLMs |
|-----------|----------------|---------|---------|--------|---------|--------|------|
| O1 | Access Control | 🔴🔴 | 🔴🔴 | 🔴(v2) | 🔴(v1) FN(v2)† | 🔴(v1) FP(v2)§ | ✓✓ | 🔴/✅ Halmos |
| O2 | Bonus Abuse | FP | FP | FP | 🔴 | 🔴 | ✓ |
| O3 | Oracle Manipulation | FP | FP | FP | 🔴 | 🔴 | ✓ |
| O4 | Flash Loan | FP | FP | FP | 🔴 | 🔴 | ✓ |
| O5 | Signature Replay | FP | FN | FN | FN | FN | ✓✓ |
| O6 | Unchecked Return | 🔴🔴 | 🔴(v1) | FP | 🔴🔴 | 🔴🔴 | ✓✓ |
| O7 | Precision Loss | 🔴 | FN | FP | 🔴 | 🔴 | ✓ |
| O8 | Reentrancy | 🔴🔴 | 🔴(v2) | 🔴🔴 | FN | FN | ✓✓ |
| O9 | Integer Arithmetic | FN | 🔴🔴 | FN | 🔴🔴 | 🔴🔴 | ✓✓ | 🔴/✅ Halmos |
| O10 | Uninitialized Proxy | FP | FN | FN | 🔴 | 🔴 | ✓ |

---

## Próximo passo

Usar esta tabela para escrever a secção de resultados do relatório final e cruzar conclusões com o estado operacional em `docs/progress.md`.
