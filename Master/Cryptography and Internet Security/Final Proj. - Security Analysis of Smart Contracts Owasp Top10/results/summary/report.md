# Relatório Unificado — 2026-05-18 20:39

Gerado por `scripts/build_report.py`.
Legenda: ✅ sem findings / ⚠️ N findings / 🔴🟡🟢 score LLM / — não executado ou sem score estruturado / ERR erro

## Análise Estática + Simbólica

| Contrato | Variante | Slither | Aderyn | Semgrep | Solhint | Mythril | Halmos |
|----------|----------|---------|--------|---------|---------|---------|--------|
| MVE_O1_Fixed | Fixed | ⚠️ 4 | ✅ | ⚠️ 2 | ✅ | ⚠️ 1 | ✅ PASS |
| MVE_O1_Fixed2 | Fixed | ⚠️ 5 | ✅ | ⚠️ 3 | ✅ | ⚠️ 1 | — |
| MVE_O1_Vuln | Vuln | ⚠️ 6 | ✅ | ⚠️ 2 | ✅ | ⚠️ 2 | 🔴 FAIL |
| MVE_O1_Vuln2 | Vuln | ⚠️ 7 | ✅ | ⚠️ 4 | ✅ | ⚠️ 2 | — |
| MVE_O2_Fixed | Fixed | ⚠️ 3 | ✅ | ⚠️ 4 | ✅ | ⚠️ 1 | — |
| MVE_O2_Vuln | Vuln | ⚠️ 3 | ✅ | ⚠️ 3 | ✅ | ⚠️ 1 | — |
| MVE_O3_Fixed | Fixed | ⚠️ 6 | ✅ | ⚠️ 4 | ✅ | ⚠️ 1 | — |
| MVE_O3_Vuln | Vuln | ⚠️ 3 | ✅ | ⚠️ 5 | ✅ | ⚠️ 2 | — |
| MVE_O4_Fixed | Fixed | ⚠️ 3 | ✅ | ⚠️ 4 | ✅ | ⚠️ 1 | — |
| MVE_O4_Vuln | Vuln | ⚠️ 3 | ✅ | ⚠️ 3 | ✅ | ⚠️ 1 | — |
| MVE_O5_Fixed | Fixed | ⚠️ 5 | ✅ | ⚠️ 6 | ✅ | ⚠️ 1 | — |
| MVE_O5_Fixed2 | Fixed | ⚠️ 4 | ✅ | ⚠️ 4 | ✅ | ✅ 0 | — |
| MVE_O5_Vuln | Vuln | ⚠️ 5 | ✅ | ⚠️ 3 | ✅ | ✅ 0 | — |
| MVE_O5_Vuln2 | Vuln | ⚠️ 4 | ✅ | ⚠️ 4 | ✅ | ✅ 0 | — |
| MVE_O6_Fixed | Fixed | ⚠️ 7 | ✅ | ⚠️ 4 | ✅ | ⚠️ 7 | — |
| MVE_O6_Fixed2 | Fixed | ⚠️ 3 | ✅ | ⚠️ 2 | ✅ | ⚠️ 1 | — |
| MVE_O6_Vuln | Vuln | ⚠️ 8 | ✅ | ⚠️ 3 | ✅ | ⚠️ 8 | — |
| MVE_O6_Vuln2 | Vuln | ⚠️ 4 | ✅ | ⚠️ 2 | ✅ | ⚠️ 1 | — |
| MVE_O7_Fixed | Fixed | ⚠️ 4 | ✅ | ⚠️ 4 | ✅ | ✅ 0 | — |
| MVE_O7_Vuln | Vuln | ⚠️ 5 | ✅ | ⚠️ 4 | ✅ | ✅ 0 | — |
| MVE_O8_Fixed | Fixed | ⚠️ 3 | ✅ | ⚠️ 3 | ✅ | ⚠️ 3 | — |
| MVE_O8_Fixed2 | Fixed | ⚠️ 4 | ✅ | ⚠️ 3 | ✅ | ⚠️ 3 | — |
| MVE_O8_Vuln | Vuln | ⚠️ 4 | ✅ | ⚠️ 2 | ✅ | ⚠️ 3 | — |
| MVE_O8_Vuln2 | Vuln | ⚠️ 5 | ✅ | ⚠️ 3 | ✅ | ⚠️ 3 | — |
| MVE_O9_Fixed | Fixed | ⚠️ 2 | ✅ | ⚠️ 3 | ✅ | ✅ 0 | ✅ PASS |
| MVE_O9_Fixed2 | Fixed | ⚠️ 2 | ✅ | ⚠️ 4 | ✅ | ✅ 0 | — |
| MVE_O9_Vuln | Vuln | ⚠️ 2 | ✅ | ⚠️ 2 | ✅ | ⚠️ 1 | 🔴 FAIL |
| MVE_O9_Vuln2 | Vuln | ⚠️ 2 | ✅ | ⚠️ 2 | ✅ | ⚠️ 1 | — |
| MVE_O10_Fixed | Fixed | ⚠️ 5 | ✅ | ⚠️ 1 | ✅ | ✅ 0 | — |
| MVE_O10_Vuln | Vuln | ⚠️ 5 | ✅ | ⚠️ 2 | ✅ | ✅ 0 | — |

## Fuzzing (Echidna / Medusa)

| Contrato | Variante | Echidna | Medusa |
|----------|----------|---------|--------|
| MVE_O1_Fixed | Fixed | ✅ OK | ✅ OK |
| MVE_O1_Fixed2 | Fixed | ✅ OK | 🔴 VULN |
| MVE_O1_Vuln | Vuln | 🔴 VULN | 🔴 VULN |
| MVE_O1_Vuln2 | Vuln | ✅ OK | 🔴 VULN |
| MVE_O2_Fixed | Fixed | ✅ OK | ✅ OK |
| MVE_O2_Vuln | Vuln | 🔴 VULN | 🔴 VULN |
| MVE_O3_Fixed | Fixed | ✅ OK | ✅ OK |
| MVE_O3_Vuln | Vuln | 🔴 VULN | 🔴 VULN |
| MVE_O4_Fixed | Fixed | ✅ OK | ✅ OK |
| MVE_O4_Vuln | Vuln | 🔴 VULN | 🔴 VULN |
| MVE_O5_Fixed | Fixed | ✅ OK | ✅ OK |
| MVE_O5_Fixed2 | Fixed | ✅ OK | ✅ OK |
| MVE_O5_Vuln | Vuln | ✅ OK | ✅ OK |
| MVE_O5_Vuln2 | Vuln | ✅ OK | ✅ OK |
| MVE_O6_Fixed | Fixed | ✅ OK | ✅ OK |
| MVE_O6_Fixed2 | Fixed | ✅ OK | ✅ OK |
| MVE_O6_Vuln | Vuln | 🔴 VULN | 🔴 VULN |
| MVE_O6_Vuln2 | Vuln | 🔴 VULN | 🔴 VULN |
| MVE_O7_Fixed | Fixed | ✅ OK | ✅ OK |
| MVE_O7_Vuln | Vuln | 🔴 VULN | 🔴 VULN |
| MVE_O8_Fixed | Fixed | ✅ OK | ✅ OK |
| MVE_O8_Fixed2 | Fixed | ✅ OK | ✅ OK |
| MVE_O8_Vuln | Vuln | ✅ OK | ✅ OK |
| MVE_O8_Vuln2 | Vuln | ✅ OK | ✅ OK |
| MVE_O9_Fixed | Fixed | ✅ OK | ✅ OK |
| MVE_O9_Fixed2 | Fixed | ✅ OK | ✅ OK |
| MVE_O9_Vuln | Vuln | 🔴 VULN | 🔴 VULN |
| MVE_O9_Vuln2 | Vuln | 🔴 VULN | 🔴 VULN |
| MVE_O10_Fixed | Fixed | ✅ OK | ✅ OK |
| MVE_O10_Vuln | Vuln | 🔴 VULN | 🔴 VULN |

## LLM (score 1–10, onde 1 = criticamente vulnerável)

| Contrato | Variante | LLaMA-3.3 (Groq) | Gemini 2.5 Flash | Qwen3-32B |
|----------|----------|------------------|-----------------|-----------|
| MVE_O1_Fixed | Fixed | 🟢 8/10 | 🟢 7/10 | 🟢 8/10 |
| MVE_O1_Fixed2 | Fixed | 🟡 6/10 | 🟡 4/10 | 🟢 7/10 |
| MVE_O1_Vuln | Vuln | 🔴 2/10 | 🔴 1/10 | 🔴 3/10 |
| MVE_O1_Vuln2 | Vuln | 🔴 2/10 | 🔴 2/10 | 🔴 3/10 |
| MVE_O2_Fixed | Fixed | 🟡 6/10 | 🟡 6/10 | 🟡 4/10 |
| MVE_O2_Vuln | Vuln | 🟡 4/10 | 🔴 2/10 | 🔴 3/10 |
| MVE_O3_Fixed | Fixed | 🟡 6/10 | 🔴 2/10 | 🟡 4/10 |
| MVE_O3_Vuln | Vuln | 🟡 4/10 | 🔴 2/10 | 🟡 4/10 |
| MVE_O4_Fixed | Fixed | 🟡 6/10 | 🔴 2/10 | 🟡 4/10 |
| MVE_O4_Vuln | Vuln | 🟡 4/10 | 🔴 3/10 | 🔴 3/10 |
| MVE_O5_Fixed | Fixed | 🟢 7/10 | 🔴 2/10 | 🟡 5/10 |
| MVE_O5_Fixed2 | Fixed | 🟢 8/10 | 🔴 2/10 | 🟢 9/10 |
| MVE_O5_Vuln | Vuln | 🟡 4/10 | 🔴 3/10 | 🔴 3/10 |
| MVE_O5_Vuln2 | Vuln | 🟡 4/10 | 🟡 4/10 | 🟡 4/10 |
| MVE_O6_Fixed | Fixed | 🟡 4/10 | 🔴 1/10 | 🟡 5/10 |
| MVE_O6_Fixed2 | Fixed | 🟡 6/10 | 🟡 4/10 | 🟢 9/10 |
| MVE_O6_Vuln | Vuln | 🟡 4/10 | 🔴 2/10 | 🔴 3/10 |
| MVE_O6_Vuln2 | Vuln | 🟡 4/10 | 🔴 2/10 | 🔴 3/10 |
| MVE_O7_Fixed | Fixed | 🟡 6/10 | 🔴 3/10 | 🟡 5/10 |
| MVE_O7_Vuln | Vuln | 🟡 4/10 | 🔴 2/10 | 🟡 4/10 |
| MVE_O8_Fixed | Fixed | 🟢 7/10 | 🟢 9/10 | 🟢 9/10 |
| MVE_O8_Fixed2 | Fixed | 🟢 8/10 | 🟢 9/10 | 🟢 9/10 |
| MVE_O8_Vuln | Vuln | 🟡 4/10 | 🔴 2/10 | 🟡 4/10 |
| MVE_O8_Vuln2 | Vuln | 🔴 2/10 | 🔴 2/10 | 🔴 3/10 |
| MVE_O9_Fixed | Fixed | 🟢 8/10 | 🔴 3/10 | 🟡 4/10 |
| MVE_O9_Fixed2 | Fixed | 🟢 8/10 | 🔴 2/10 | 🔴 3/10 |
| MVE_O9_Vuln | Vuln | 🟡 4/10 | 🟡 6/10 | 🔴 3/10 |
| MVE_O9_Vuln2 | Vuln | 🔴 2/10 | 🔴 2/10 | 🔴 3/10 |
| MVE_O10_Fixed | Fixed | 🟢 8/10 | 🔴 2/10 | 🟢 8/10 |
| MVE_O10_Vuln | Vuln | 🟡 4/10 | 🔴 2/10 | 🔴 3/10 |
