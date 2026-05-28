#!/usr/bin/env python3
"""
build_report.py — Constrói o relatório unificado de todas as ferramentas.
Lê os resultados de results/ e gera:
  results/summary/report.md   — tabela Markdown com todas as ferramentas
  results/summary/report.csv  — CSV para análise em Excel/Python

Cobre: Slither, Aderyn, Semgrep, Solhint, Mythril, Halmos (via analyze.sh)
        Echidna, Medusa (via fuzz.sh)
        Groq, Gemini, Qwen (via scripts/llm_analyze.py)

Uso:
  python3 scripts/build_report.py
"""

import csv
import os
import re
from datetime import datetime
from pathlib import Path

ROOT = Path(__file__).parent.parent
RESULTS = ROOT / "results"
CONTRACTS = ROOT / "contracts"

STATIC_TOOLS  = ["slither", "aderyn", "semgrep", "solhint", "mythril", "halmos"]

# Contratos com funções check_* para verificação formal Halmos.
# Chave: (categoria, nome_contrato) → ficheiro de resultado em results/halmos/
HALMOS_CHECKS = {
    ("OWASP_01", "MVE_O1_Vuln"):  "OWASP_01_OWASP_01_VulnCheck.txt",
    ("OWASP_01", "MVE_O1_Fixed"): "OWASP_01_OWASP_01_FixedCheck.txt",
    ("OWASP_09", "MVE_O9_Vuln"):  "OWASP_09_OWASP_09_VulnCheck.txt",
    ("OWASP_09", "MVE_O9_Fixed"): "OWASP_09_OWASP_09_FixedCheck.txt",
}
FUZZ_TOOLS    = ["echidna", "medusa"]
LLM_MODELS    = ["groq", "gemini", "qwen"]
ALL_TOOLS     = STATIC_TOOLS + FUZZ_TOOLS + LLM_MODELS


# ============================================================
# Detecção de estado por ferramenta
# ============================================================

def _file_status(path: Path) -> str:
    """✅ se o ficheiro existe e tem conteúdo útil, ❌ se não existe."""
    if not path.exists() or path.stat().st_size == 0:
        return "❌"
    content = path.read_text()
    if content.startswith("[ERRO]"):
        return "ERR"
    return "✅"


def get_static_status(tool: str, owasp: str, name: str) -> str:
    """Estado para ferramentas estáticas/simbólicas (slither, semgrep, etc.)."""
    # O relatório normaliza outputs muito diferentes para uma escala curta:
    # "✅ 0" (sem findings), "⚠️ N" (findings), "ERR" (erro técnico), "—" (não executado).
    # Aderyn usa .md, todas as outras .txt
    ext = ".md" if tool == "aderyn" else ".txt"
    path = RESULTS / tool / f"{owasp}_{name}{ext}"
    if not path.exists():
        return "—"
    content = path.read_text()
    if not content.strip():
        return "—"
    # Para slither/mythril, contar findings reais (não apenas ruído)
    if tool == "slither":
        match = re.search(r"(\d+) result\(s\) found", content)
        n = int(match.group(1)) if match else 0
        return f"⚠️ {n}" if n > 0 else "✅ 0"
    if tool == "mythril":
        if "No issues were detected" in content:
            return "✅ 0"
        issues = len(re.findall(r"^={4}", content, re.MULTILINE))
        return f"⚠️ {issues}" if issues > 0 else "✅ 0"
    if tool == "semgrep":
        match = re.search(r"(\d+) finding", content)
        n = int(match.group(1)) if match else 0
        return f"⚠️ {n}" if n > 0 else "✅ 0"
    if tool == "halmos":
        # Verificar primeiro se existe um contrato check_* dedicado para este MVE.
        check_file = HALMOS_CHECKS.get((owasp, name))
        if check_file:
            check_path = RESULTS / "halmos" / check_file
            if check_path.exists():
                c = check_path.read_text()
                if re.search(r"\d+ passed; 0 failed", c):
                    return "✅ PASS"
                if re.search(r"0 passed; \d+ failed", c) or "[FAIL]" in c:
                    return "🔴 FAIL"
        # Fallback: ficheiro gerado pelo loop principal (contratos sem check_*)
        if "FileNotFoundError" in content or "Traceback" in content:
            return "ERR"
        if "No tests found" in content:
            return "—"
        if re.search(r"\b\d+\s+passed\b", content):
            return "✅"
        return "—"
    # solhint, aderyn: ficheiro existe = OK
    return "✅" if path.exists() else "—"


def get_fuzz_status(tool: str, owasp: str, name: str) -> str:
    """Estado para fuzzing (echidna, medusa).

    Os logs de fuzzing têm nomes baseados no contrato de fuzz (ex: OWASP_01_VulnFuzz),
    não no contrato principal (ex: MVE_O1_Vuln). Fazemos a derivação aqui.
    Formato do ficheiro: results/{tool}/{owasp}_{owasp}_{variant}Fuzz.log
    """
    # Usar a variante completa do nome do contrato para preservar Vuln2/Fixed2.
    variant = name.split("_")[-1]
    fuzz_contract = f"{owasp}_{variant}Fuzz"
    path = RESULTS / tool / f"{owasp}_{fuzz_contract}.log"
    if not path.exists():
        return "—"
    content = path.read_text()
    if not content.strip():
        return "—"

    if tool == "echidna":
        # Echidna: "echidna_xxx: failed!💥" ou "echidna_xxx: passing"
        if re.search(r"echidna_\w+: failed!", content):
            return "🔴 VULN"
        if re.search(r"echidna_\w+: passing", content):
            return "✅ OK"
        return "ERR"

    if tool == "medusa":
        # Medusa: "[FAILED] Property Test: ..." ou "[PASSED] Property Test: ..."
        if "[FAILED] Property" in content:
            return "🔴 VULN"
        if "[PASSED] Property" in content:
            return "✅ OK"
        if "Failed to initialize" in content or "Failed to compile" in content:
            return "ERR"
        return "—"

    return "—"


def get_llm_status(model: str, owasp: str, name: str) -> str:
    """Estado para LLMs — mostra score se disponível."""
    path = RESULTS / "llm" / owasp / f"{name}_{model}.txt"
    if not path.exists():
        return "—"
    content = path.read_text()
    if content.startswith("[ERRO]"):
        return "ERR"
    # Extrair score numérico (ex: "4/10" ou "Score: 4") para comparar modelos.
    patterns = [
        r"(?im)^\s*\*{0,2}\s*score\s*\*{0,2}\s*[:=-]\s*\*{0,2}\s*(\d{1,2})(?:\s*/\s*10)?",
        r"(?i)\bscore\b[^\d]{0,24}(\d{1,2})(?:\s*/\s*10)?",
    ]
    for pattern in patterns:
        match = re.search(pattern, content)
        if not match:
            continue
        score = int(match.group(1))
        if score > 10:
            continue
        # Score visual: 1-3=🔴, 4-6=🟡, 7-10=🟢
        icon = "🔴" if score <= 3 else ("🟡" if score <= 6 else "🟢")
        return f"{icon} {score}/10"
    return "—"


# ============================================================
# Construir tabela
# ============================================================

def build_rows() -> list[dict]:
    rows = []
    for owasp_dir in sorted(CONTRACTS.glob("OWASP_*/")):
        owasp = owasp_dir.name
        for contract in sorted(owasp_dir.glob("*.sol")):
            name = contract.stem
            variant = "Vuln" if "Vuln" in name else "Fixed"

            row = {
                "category": owasp,
                "contract": name,
                "variant": variant,
            }

            for tool in STATIC_TOOLS:
                row[tool] = get_static_status(tool, owasp, name)
            for tool in FUZZ_TOOLS:
                row[tool] = get_fuzz_status(tool, owasp, name)
            for model in LLM_MODELS:
                row[model] = get_llm_status(model, owasp, name)

            rows.append(row)
    return rows


# ============================================================
# Gerar Markdown
# ============================================================

def build_markdown(rows: list[dict]) -> str:
    lines = [
        f"# Relatório Unificado — {datetime.now().strftime('%Y-%m-%d %H:%M')}",
        "",
        "Gerado por `scripts/build_report.py`.",
        "Legenda: ✅ sem findings / ⚠️ N findings / 🔴🟡🟢 score LLM / — não executado ou sem score estruturado / ERR erro",
        "",
        "## Análise Estática + Simbólica",
        "",
        "| Contrato | Variante | Slither | Aderyn | Semgrep | Solhint | Mythril | Halmos |",
        "|----------|----------|---------|--------|---------|---------|---------|--------|",
    ]
    for r in rows:
        lines.append(
            f"| {r['contract']} | {r['variant']} "
            f"| {r['slither']} | {r['aderyn']} | {r['semgrep']} "
            f"| {r['solhint']} | {r['mythril']} | {r['halmos']} |"
        )

    lines += [
        "",
        "## Fuzzing (Echidna / Medusa)",
        "",
        "| Contrato | Variante | Echidna | Medusa |",
        "|----------|----------|---------|--------|",
    ]
    for r in rows:
        lines.append(
            f"| {r['contract']} | {r['variant']} "
            f"| {r['echidna']} | {r['medusa']} |"
        )

    lines += [
        "",
        "## LLM (score 1–10, onde 1 = criticamente vulnerável)",
        "",
        "| Contrato | Variante | LLaMA-3.3 (Groq) | Gemini 2.5 Flash | Qwen3-32B |",
        "|----------|----------|------------------|-----------------|-----------|",
    ]
    for r in rows:
        lines.append(
            f"| {r['contract']} | {r['variant']} "
            f"| {r['groq']} | {r['gemini']} | {r['qwen']} |"
        )

    return "\n".join(lines) + "\n"


# ============================================================
# Gerar CSV
# ============================================================

def build_csv(rows: list[dict], path: Path) -> None:
    fieldnames = ["category", "contract", "variant"] + ALL_TOOLS
    with open(path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


# ============================================================
# Main
# ============================================================

def main():
    summary_dir = RESULTS / "summary"
    summary_dir.mkdir(parents=True, exist_ok=True)

    print("A construir relatório unificado...")
    rows = build_rows()

    md_path = summary_dir / "report.md"
    md_path.write_text(build_markdown(rows))
    print(f"  ✅ Markdown → {md_path}")

    csv_path = summary_dir / "report.csv"
    build_csv(rows, csv_path)
    print(f"  ✅ CSV      → {csv_path}")

    # Estatísticas rápidas
    total = len(rows)
    with_llm = sum(1 for r in rows if r["groq"] not in ("—", "ERR"))
    with_static = sum(1 for r in rows if r["slither"] not in ("—", "ERR"))
    with_fuzz = sum(1 for r in rows if r["echidna"] not in ("—", "ERR"))
    print(f"\n  Contratos totais : {total}")
    print(f"  Com análise estática : {with_static}/{total}")
    print(f"  Com análise LLM      : {with_llm}/{total}")
    print(f"  Com fuzzing          : {with_fuzz}/{total}")


if __name__ == "__main__":
    main()
