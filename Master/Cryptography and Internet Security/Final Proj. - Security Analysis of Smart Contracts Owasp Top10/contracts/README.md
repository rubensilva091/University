# Contratos — OWASP Smart Contract Top 10

Cada categoria tem um ou dois pares **Vuln** + **Fixed**:

| Categoria | Vuln | Fixed | Vulnerabilidade |
|-----------|------|-------|-----------------|
| OWASP_01 | MVE_O1_Vuln.sol | MVE_O1_Fixed.sol | Access Control (sem onlyOwner) |
| OWASP_01 | MVE_O1_Vuln2.sol | MVE_O1_Fixed2.sol | Access Control (tx.origin phishing) |
| OWASP_02 | MVE_O2_Vuln.sol | MVE_O2_Fixed.sol | Bonus Abuse / Business Logic |
| OWASP_03 | MVE_O3_Vuln.sol | MVE_O3_Fixed.sol | Price Oracle Manipulation |
| OWASP_04 | MVE_O4_Vuln.sol | MVE_O4_Fixed.sol | Flash Loan / Same-block Abuse |
| OWASP_05 | MVE_O5_Vuln.sol | MVE_O5_Fixed.sol | Signature Replay (sem nonce/deadline) |
| OWASP_05 | MVE_O5_Vuln2.sol | MVE_O5_Fixed2.sol | Signature Replay (falta validação de input) |
| OWASP_06 | MVE_O6_Vuln.sol | MVE_O6_Fixed.sol | Unchecked Return Value (ETH call) |
| OWASP_06 | MVE_O6_Vuln2.sol | MVE_O6_Fixed2.sol | Unchecked Return Value (ERC20 transfer) |
| OWASP_07 | MVE_O7_Vuln.sol | MVE_O7_Fixed.sol | Precision Loss (divide-before-multiply) |
| OWASP_08 | MVE_O8_Vuln.sol | MVE_O8_Fixed.sol | Reentrancy (single-function) |
| OWASP_08 | MVE_O8_Vuln2.sol | MVE_O8_Fixed2.sol | Reentrancy (cross-function) |
| OWASP_09 | MVE_O9_Vuln.sol | MVE_O9_Fixed.sol | Integer Overflow (unchecked totalSupply) |
| OWASP_09 | MVE_O9_Vuln2.sol | MVE_O9_Fixed2.sol | Integer Underflow (unchecked burn) |
| OWASP_10 | MVE_O10_Vuln.sol | MVE_O10_Fixed.sol | Uninitialized Proxy |

Para análise de segurança com as ferramentas automáticas:
```bash
./analyze.sh
./fuzz.sh
python3 scripts/llm_analyze.py --all
```
