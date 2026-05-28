# CSI Project: Security Analysis of Smart Contract Languages

## Objective
This project provides an automated empirical analysis of vulnerabilities in Solidity smart contracts, covering the categories defined by the OWASP Smart Contract Top 10. The primary goal is to evaluate and compare the effectiveness of different security analysis paradigms, including static analysis, symbolic execution, fuzzing, and Large Language Models (LLMs).

The methodology utilizes a Minimal Viable Exploit (MVE) approach, featuring 30 contracts organized into Vulnerable and Fixed pairs across 10 categories. This dual-baseline setup allows for precise measurement of detection rates (True Positives) and false alarm rates (False Positives).

## Repository Structure
* contracts/OWASP_XX/: Solidity source files for each vulnerability category, featuring both Vulnerable and Fixed variants.
* test/: Support contracts for fuzzing, containing invariant definitions for Echidna and Medusa.
* config/: Configuration files, version pinning, and tool-specific settings (e.g., echidna.yaml, medusa.json).
* scripts/: Automation for LLM analysis and consolidated report generation.
* results/: Detailed tool outputs, execution logs, and consolidated summaries in Markdown and CSV formats.
* docs/: Project documentation regarding setup, tools, and technical methodology.

## Evaluated Tools and Methodologies

| Paradigm | Tools |
| :--- | :--- |
| Static Analysis | Slither, Aderyn, Semgrep, Solhint |
| Symbolic & Formal Analysis | Mythril, Halmos |
| Fuzzing | Echidna, Medusa |
| Large Language Models | Groq (LLaMA-3.3), Gemini 2.5 Flash, Qwen3-32B |

## Setup and Usage

### 1. Environment Preparation
Ensure the system has Python 3.8+, pipx, Node.js, and Foundry installed. 

    ./setup.sh
    ./setup.sh --check

Note: The scripts automatically unset VIRTUAL_ENV to prevent permission conflicts with solc-select.

### 2. Canonical Execution Flow
The complete pipeline can be reproduced using the following sequence:

    # Step 1: Static and Symbolic Analysis
    ./analyze.sh

    # Step 2: Fuzzing Analysis
    ./fuzz.sh

    # Step 3: LLM Analysis (requires .env with API keys)
    .venv/bin/python scripts/llm_analyze.py --all

    # Step 4: Final Consolidated Report Generation
    .venv/bin/python scripts/build_report.py


## Comparative Results Summary

Based on the evaluation of 15 vulnerable scenarios:

| Tool | Effectiveness (TP) | Observations |
| :--- | :--- | :--- |
| Slither | 7 / 15 | Strong in detecting reentrancy and tx.origin issues; high noise in generic findings. |
| Mythril | 6 / 15 | Highly effective for arithmetic issues (SWC-101); prone to state-lock false positives (SWC-107). |
| Echidna / Medusa | 10 / 15 | Strongest practical performance; limited only by off-chain signature requirements. |
| Aderyn | 3 / 15 | Limited detection, primarily focused on specific patterns like tx.origin and reentrancy. |
| LLMs | ~15 / 15 | Excellent semantic triage and vulnerability explanation. |

## Technical Limitations

* Halmos: Produced formal proofs on two contracts where check_* properties were authored (O1 and O9); remaining contracts lack check_* properties.
* Semgrep / Solhint: These tools function primarily as pattern-based linters; they did not detect exploit-level vulnerabilities in this specific benchmark.
* Fuzzing Constraints: Fuzzers cannot currently validate scenarios requiring valid off-chain ECDSA signatures (OWASP_05) or certain multi-contract reentrancy callbacks.

## Status and Maintenance
The practical analysis pipeline is fully implemented and reproducible. Gemini analysis completed for all 30 contracts (see results/summary/report.csv); ensure GEMINI_API_KEY is set in .env for reproducibility.

---
This work was developed as a Project in Cryptography and Information Security.