#!/usr/bin/env python3
"""
llm_analyze.py — Análise de contratos Solidity com LLMs
Guarda outputs em results/llm/<owasp>/<contrato>_<modelo>.txt

Uso:
  python3 scripts/llm_analyze.py contracts/OWASP_08/MVE_O8_Vuln.sol
  python3 scripts/llm_analyze.py --model groq contracts/OWASP_08/MVE_O8_Vuln.sol
  python3 scripts/llm_analyze.py --category OWASP_08
  python3 scripts/llm_analyze.py --all
"""

import argparse
import os
import sys
import time
from datetime import datetime
from pathlib import Path

from dotenv import load_dotenv

load_dotenv()

ROOT_DIR = Path(__file__).parent.parent
RESULTS_DIR = ROOT_DIR / "results" / "llm"
CONTRACTS_DIR = ROOT_DIR / "contracts"

RESULTS_DIR.mkdir(parents=True, exist_ok=True)

# ============================================================
# Prompt de análise de segurança
# ============================================================
SECURITY_PROMPT = """You are an expert Solidity smart contract security auditor.
Analyze the following smart contract and identify ALL security vulnerabilities.

For each vulnerability found, report:
1. Vulnerability type (e.g., reentrancy, integer overflow, access control, etc.)
2. Severity: Critical / High / Medium / Low / Informational
3. Affected function and line number(s)
4. Brief description of the risk
5. Recommended fix (1-2 sentences)

Also provide at the end:
- Overall security score (1-10, where 10 = perfectly secure, 1 = critically vulnerable)
- One-sentence summary of the most critical finding (or "No critical issues found")

Contract:
```solidity
{code}
```"""


# ============================================================
# Clientes LLM
# ============================================================

def analyze_groq(code: str) -> tuple[str, float]:
    """Groq — llama-3.3-70b-versatile"""
    from groq import Groq
    api_key = os.getenv("GROQ_API_KEY")
    if not api_key:
        # Prefixo [ERRO] é intencional: build_report.py detecta este marcador.
        return "[ERRO] GROQ_API_KEY não definida em .env", 0.0

    client = Groq(api_key=api_key)
    t0 = time.time()
    response = client.chat.completions.create(
        model="llama-3.3-70b-versatile",
        messages=[{"role": "user", "content": SECURITY_PROMPT.format(code=code)}],
        temperature=0.1,
        max_tokens=2048,
    )
    elapsed = time.time() - t0
    return response.choices[0].message.content, elapsed


def analyze_gemini(code: str) -> tuple[str, float]:
    """Google Gemini — gemini-2.5-flash (novo SDK google-genai)"""
    from google import genai
    api_key = os.getenv("GEMINI_API_KEY")
    if not api_key:
        return "[ERRO] GEMINI_API_KEY não definida em .env", 0.0

    client = genai.Client(api_key=api_key)
    t0 = time.time()
    for attempt in range(4):
        try:
            response = client.models.generate_content(
                model="gemini-2.5-flash",
                contents=SECURITY_PROMPT.format(code=code),
                temperature=0.1,
            )
            return response.text, time.time() - t0
        except Exception as e:
            if attempt == 3:
                raise
            wait = 30 * (attempt + 1)
            print(f"     [GEMINI] retry {attempt+1}/3 em {wait}s ({e})")
            time.sleep(wait)


def analyze_qwen(code: str) -> tuple[str, float]:
    """Groq — Qwen3-32B (arquitetura diferente do LLaMA, Alibaba)"""
    from groq import Groq
    api_key = os.getenv("GROQ_API_KEY")
    if not api_key:
        return "[ERRO] GROQ_API_KEY não definida em .env", 0.0

    client = Groq(api_key=api_key)
    t0 = time.time()
    response = client.chat.completions.create(
        model="qwen/qwen3-32b",
        messages=[{"role": "user", "content": "/no_think\n\n" + SECURITY_PROMPT.format(code=code)}],
        temperature=0.1,
        max_tokens=4096,  # /no_think desativa chain-of-thought; output é curto e cabe no limite Groq free (6k TPM)
    )
    elapsed = time.time() - t0
    return response.choices[0].message.content, elapsed


MODELS = {
    "groq":   ("Groq / LLaMA-3.3-70B",       analyze_groq),
    "gemini": ("Google Gemini 2.5 Flash",     analyze_gemini),
    "qwen":   ("Groq / Qwen3-32B (Alibaba)",  analyze_qwen),
}


# ============================================================
# Análise de um contrato
# ============================================================

def analyze_contract(contract_path: Path, model_keys: list[str]) -> None:
    if not contract_path.exists():
        print(f"[ERRO] Ficheiro não encontrado: {contract_path}")
        return

    code = contract_path.read_text()
    name = contract_path.stem           # ex: MVE_O8_Vuln
    owasp = contract_path.parent.name   # ex: OWASP_08

    # Estrutura: results/llm/OWASP_08/MVE_O8_Vuln_groq.txt
    owasp_dir = RESULTS_DIR / owasp
    owasp_dir.mkdir(parents=True, exist_ok=True)

    print(f"\n  >> {owasp}/{name}")

    for key in model_keys:
        model_name, fn = MODELS[key]
        out_path = owasp_dir / f"{name}_{key}.txt"
        print(f"     [{key.upper()}] a analisar ({model_name})...")

        if key == "gemini":
            time.sleep(15)  # free tier: 5 req/min → wait 15s between calls

        try:
            response, elapsed = fn(code)
            with open(out_path, "w") as f:
                # Cabeçalho padronizado para rastreabilidade (modelo, contrato, timestamp, tempo).
                f.write(f"# LLM Analysis — {model_name}\n")
                f.write(f"# Contract  : {owasp}/{name}.sol\n")
                f.write(f"# Timestamp : {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
                f.write(f"# Time      : {elapsed:.1f}s\n")
                f.write("#\n\n")
                f.write(response)
            print(f"     [{key.upper()}] ✓ {elapsed:.1f}s → {out_path.name}")
        except Exception as e:
            with open(out_path, "w") as f:
                f.write(f"[ERRO] {e}\n")
            print(f"     [{key.upper()}] ✗ ERRO: {e}")


# ============================================================
# CLI
# ============================================================

def main():
    parser = argparse.ArgumentParser(
        description="Análise de contratos Solidity com LLMs"
    )
    parser.add_argument(
        "contract",
        nargs="?",
        help="Caminho para o contrato .sol a analisar",
    )
    parser.add_argument(
        "--model",
        choices=list(MODELS.keys()),
        help="Modelo específico a usar (omitir = todos)",
    )
    parser.add_argument(
        "--category",
        help="Analisar todos os contratos de uma categoria (ex: OWASP_08)",
    )
    parser.add_argument(
        "--all",
        action="store_true",
        help="Analisar todos os contratos em contracts/",
    )
    args = parser.parse_args()

    model_keys = [args.model] if args.model else list(MODELS.keys())

    print("=== LLM Analyzer ===")
    print(f"  Modelos: {', '.join(model_keys)}")
    print(f"  Output : {RESULTS_DIR}/")
    print()

    contracts: list[Path] = []

    if args.all:
        contracts = sorted(CONTRACTS_DIR.glob("OWASP_*/*.sol"))
    elif args.category:
        cat_dir = CONTRACTS_DIR / args.category
        if not cat_dir.exists():
            print(f"[ERRO] Categoria não encontrada: {cat_dir}")
            sys.exit(1)
        contracts = sorted(cat_dir.glob("*.sol"))
    elif args.contract:
        contracts = [Path(args.contract)]
    else:
        parser.print_help()
        sys.exit(1)

    if not contracts:
        print("[AVISO] Nenhum contrato encontrado.")
        sys.exit(0)

    print(f"  {len(contracts)} contrato(s) a analisar")
    print()

    for c in contracts:
        analyze_contract(c, model_keys)

    print()
    print("=== Análise LLM concluída ===")
    print(f"  Resultados em: {RESULTS_DIR}/")


if __name__ == "__main__":
    main()
