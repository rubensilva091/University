# Week 3 - Questions

**General Guidelines:**

1. Read each question carefully.

2. If links are provided, open them and spend some time exploring the page/document. The goal is to learn more than just the answer. If no links are provided, do your own research.

3. Never copy-paste: write answers in your own words (writing helps you learn). The answer can be written in portuguese or english, your choice.

4. There's no rush — take your time for each step.


**How to do this assignment:**

1. Create a new folder in your repository named `week3`. Copy this file into that folder (using the same name, `QUESTIONS.md`), then commit (the message should be "w3part1") and push.

2. For each question, type your answer immediately after "Answer:".
   - For multiple-choice questions, write only the letter.
   - For open questions, write your answer in a single line.

3. After answering 1 question, commit and push.
   - The commit message should be: "w3:q1".

4. After every additional question, commit and push again.
   - Example: "w3:q2", then "w3:q3".

5. Repeat until you have completed all questions.

---

## Questions

<!-- ---------------------------------------------------------------------- -->
### Q1. Methodology Analysis

**Preliminary Reading:**  
- Methodology (focus especially on "Scoring"): [https://cwe.mitre.org/top25/archive/2025/2025_methodology.html](https://cwe.mitre.org/top25/archive/2025/2025_methodology.html)
- 2025 Top 25 List: [https://cwe.mitre.org/top25/archive/2025/2025_cwe_top25.html](https://cwe.mitre.org/top25/archive/2025/2025_cwe_top25.html)
- 2025 “On the Cusp” List: [https://cwe.mitre.org/top25/archive/2025/2025_onthecusp_list.html](https://cwe.mitre.org/top25/archive/2025/2025_onthecusp_list.html)

**Question:**  
The 2025 CWE Top 25 ranking is computed using:

- The number of CVEs mapped to each CWE (frequency)
- The average CVSS severity of those CVEs
- A defined scoring formula combining these elements

Identify **at least four distinct sources of bias or distortion** in the methodology or data used to construct the Top 25 ranking. For each source of bias:
1. Name the type of bias.
2. Explain how it arises.
3. Describe how it may affect the final ranking.
4. Reference at least one observation from the provided links.

**Some Notes:**

1. **Bias** can be seen as a systematic deviation in measurement, representation, or analysis that causes results to consistently diverge from the true underlying reality.
  - In the context of security metrics (for example, CWE Top 25 rankings), bias occurs when the data source, methodology, or scoring model systematically favors or disadvantages certain categories, leading to results that reflect the structure of the measurement process rather than the actual distribution of weaknesses in software. Note that it is difficult, if not impossible, to actually know the "**actual** distribution of weaknesses in software", mainly because there are several projects that are not as heavily scrutinized as some others.
  - **Distortion** is the resulting effect of bias: the way in which outputs (such as rankings) are skewed, exaggerated, or suppressed relative to the true situation.

2. Carefully read the **"Scoring" section** of the methodology page. Understand how the score is computed.
3. Understand the differences between, for instance, CVSS 3.1 and CVSS 4.0. Proposed reading: [blog post from mend.io](https://www.mend.io/blog/cvss-3-1-vs-cvss-4-0-a-look-at-the-data/) and [blog post from qualys](https://blog.qualys.com/product-tech/2023/11/02/cvss-v4-is-now-live-and-what-do-you-need-to-know). Interesting quote from the first proposed blog post: "CVSS 4.0 base scores may be higher but your actual risk hasn’t changed.".
4. Experiment a couple of different scnearios in the [CVSS 4.0 calculator](https://www.first.org/cvss/calculator/4.0) and observe the computed scores.
4. Consider how CVEs are mapped to CWEs: is that process perfectly consistent? [https://cwe.mitre.org/documents/cwe_usage/guidance.html](https://cwe.mitre.org/documents/cwe_usage/guidance.html)
5. Examine the “On the Cusp” list. What does it suggest about ranking stability? Do small variations in CVSS scores or CVE frequency meaningfully impact inclusion in the Top 25? Should a CWE ranked 35 be considered substantially less severe or less important than one ranked 5?
6. Think about which ecosystems (projects) are most heavily scrutinized and how it affects CVE reporting and discovery.
7. Explore the following website: [https://www.cvedetails.com/](https://www.cvedetails.com/)
  
**Now some notes about the goal of this exercise before you answer:**

The goal of this exercise is to develop critical thinking about security metrics and rankings. Tools like the CWE Top 25 and CVSS are extremely useful, but they are models built on data and methodological choices. Consider the following:

- They organize and prioritize information: they do not represent absolute truth.
- Every scoring system reflects assumptions, simplifications, and potential biases.
- A higher rank does not automatically mean higher real-world risk in every context.

You should learn to ask what a score measures and what it does not measure. You should use these tools thoughtfully, not blindly. The objective is to question intelligently, not to reject the tools themselves.

**Answer Q1:**

## Bias 1
1. **Name** "Quem faz Reports"
2. **How it Arrives** Grandes empresas e projetos open-source reportam muitos CVEs. Software mais pequenes (i.e, web apps) ou fechados, raramente geram CVEs públicos.
3. **Effect** Distorce o ranking para os problemas típicos dos grandes sistemas, ignorando as falhas reais do dia a dia em apps corporativas fechadas.
4. **Ref** O texto [About CWE](https://cwe.mitre.org/about/index.html) destaca que existem vistas específicas para "software escrito em C, C++". Olhando para a lista do Top 25, vemos que falhas típicas destas linguagens (como o CWE-787) dominam o topo, provando que o ranking reflete o ecossistema das grandes empresas que mais reportam publicamente, e não o total global de falhas de todo o software.

## Bias 2
1. **Name** "O Momento do Report"
2. **How it Arrives** Ao registar um CVE, muitas vezes não se tem todos os detalhes técnicos, e acaba-se por mapear a vulnerabilidade para uma categoria mais abrangente.
3. **Effect** Inflacionamento de categorias genéricas e falta de precisão no conteúdo
4. **Ref** O documento [CWE "Root Cause Mapping"](https://cwe.mitre.org/documents/cwe_usage/guidance.html) mostra exatamente isto ao explicar que o mapeamento ideal deve ser feito a níveis específicos ("Base"), mas como na prática isso falha, categorias abstratas como o CWE-20 (Improper Input Validation) acabam no topo do ranking, apesar de o próprio documento classificar o seu uso como "Desencorajado" (DISCOURAGED) por ser demasiado genérico.

## Bias 3
1. **Name** "Impacto Teórico vs. Risco Real"
2. **How it Arrives** A fórmula de pontuação da metodologia do Top 25 calcula a gravidade multiplicando a frequência pela média do CVSS Base Score dos CVEs. No entanto, o CVSS avalia apenas o impacto técnico e a complexidade, não a probabilidade real de a falha estar a ser ativamente explorada pelos hackers.
3. **Effect** Falhas gravíssimas no papel, mas quase impossíveis de explorar na prática, acabam por ter pontuações mais altas do que vulnerabilidades de gravidade "média" que estão a ser usadas todos os dias em ataques reais.
4. **Ref** A secção "Scoring" do documento da [Metodologia do Top 25](https://cwe.mitre.org/top25/archive/2025/2025_methodology.html) prova isto ao mostrar a sua fórmula exata: $Score(CWE_X) = Fr(CWE_X) * Sv(CWE_X)*100$ , onde a Severidade é apenas a média do CVSS. Como refere no enunciado o CVSS foca-se na gravidade técnica e não no risco real.

## Bias 4
1. **Name** "Refinamento e Intervenção Manual"
2. **How it Arrives** Para calcular o ranking, a equipa não utiliza os dados públicos do NVD no seu estado puro. Eles aplicam ferramentas internas.
3. **Effect** O ranking final deixa de ser um reflexo estatístico exato dos dados reportados globalmente. Em vez disso, passa a ser fortemente moldado pelas intervenções manuais, algoritmos de correção e decisões editoriais da própria equipa da MITRE.
4. **Ref** O documento [2025 CWE Top 25 Methodology](https://cwe.mitre.org/top25/archive/2025/2025_methodology.html) no capítulo *Dataset Refinement*, ilustra isto perfeitamente. A equipa notou que o fornecedor com maior volume de publicações (1.884 CVEs) mapeava frequentemente para categorias genéricas, o que "influenciou o dataset consideravelmente"

---

<!-- ---------------------------------------------------------------------- -->

### Q2. Software Identification: CPE and PURL

CPE (Common Platform Enumeration), originally maintained by MITRE (check [https://cpe.mitre.org/](https://cpe.mitre.org/)) is now managed by NIST (check [https://nvd.nist.gov/products/cpe](https://nvd.nist.gov/products/cpe). As per NIST website, "CPE is a structured naming scheme for information technology systems, software, and packages.".

PURL (Package URL) "introduces a standardized URL-based syntax that uniquely identifies software packages, independent of their ecosystem or distribution channel." (quote taken from [https://packageurl.org/](https://packageurl.org/)). PURL is a Ecma International (former European Computer Manufacturers Association) standard: [https://ecma-international.org/publications-and-standards/standards/ecma-427/](https://ecma-international.org/publications-and-standards/standards/ecma-427/). It is/will be widely adopted. Suggested reading:
- [https://github.com/package-url/purl-spec](https://github.com/package-url/purl-spec) 
- [https://fossa.com/blog/understanding-purl-specification-package-url/](https://fossa.com/blog/understanding-purl-specification-package-url/) - Understanding PURL specification; includes section "PURL vs. CPE"
- [https://www.cve.org/CVERecord/UserGuide](https://www.cve.org/CVERecord/UserGuide) - mentions CPE and PURL in the context of CVE Record;
- [https://github.com/CVEProject/cve-schema/tree/main](https://github.com/CVEProject/cve-schema/tree/main) - "The CVE Record Format is the JSON schema defining the structure of CVE records."; 
- [https://github.com/CVEProject/cve-schema/releases/tag/v5.2.0](https://github.com/CVEProject/cve-schema/releases/tag/v5.2.0) - Release 5.2.0 of CVE Record Format (the latest, from October 25) adds support for PURL.
- [https://cyclonedx.org/use-cases/identify-known-vulnerabilities/](https://cyclonedx.org/use-cases/identify-known-vulnerabilities/)
- [https://tc54.org/](https://tc54.org/) [Ecma Technical Committee 54](https://ecma-international.org/technical-committees/tc54/?tab=general) also published ECMA-424, CycloneDX Bill of materials specification.

**Question:**  

Based on the provided material:

1. What is the main conceptual difference between CPE and PURL in how they identify software?

2. Why was support for PURL added to the CVE Record Format (v5.2.0+) if CPE already existed?

Answer concisely and support your reasoning with references to the specifications.


**Answer Q2.1:**

A diferença conceptual principal reside no modelo de gestão e na forma como identificam o software: o CPE é centralizado e baseado num dicionário, enquanto o PURL é descentralizado e baseado no ecossistema.

**CPE (Common Platform Enumeration)**: É um sistema centralizado que depende de um dicionário oficial gerido por uma autoridade, *NIST*. Não escala bem para bibliotecas de software modernas, pois exige que alguém crie e mantenha o registo formal de cada pacote.
[Fonte](https://cpe.mitre.org/)

**PURL (Package URL)**: É um sistema descentralizado que usa uma sintaxe baseada em URLs. Identifica o software diretamente pelo seu ecossistema ou gestor de pacotes (ex: pkg:npm/lodash), permitindo gerar identificadores de forma automática e instantânea, sem depender de uma base de dados central.
[Fonte](https://packageurl.org/)

[Fonte Extra](https://www.cve.org/CVERecord/UserGuide)

---

**Answer Q2.2:**

O suporte para PURL foi adicionado ao formato CVE (v5.2.0+) para resolver as limitações do CPE face à cadeias modernas de fornecimento de software e para permitir a integração direta com os *Software Bill of Materials* (SBOMs)

**Limitações práticas do CPE**: O ecossistema atual depende massivamente de pacotes open-source (ex: npm, PyPI). O modelo centralizado do CPE não consegue acompanhar o ritmo de publicação destas bibliotecas, resultando em muitos pacotes vulneráveis sem qualquer identificador CPE atribuído.

**Automação (SBOMs)**: Padrões modernos de listas de materiais de software, como o CycloneDX (ECMA-424), utilizam o PURL nativamente para inventariar componentes. A inclusão do PURL nos registos CVE permite que ferramentas de segurança cruzem automaticamente, e com precisão absoluta, os pacotes encontrados num projeto com as bases de dados de vulnerabilidades.

[Fonte 1](https://github.com/CVEProject/cve-schema/releases/tag/v5.2.0)
[Fonte 2](https://cyclonedx.org/use-cases/identify-known-vulnerabilities/)

---

<!-- ---------------------------------------------------------------------- -->
### Q3. The Exploit Prediction Scoring System

Consider the following information:
- [https://www.first.org/epss/model](https://www.first.org/epss/model) - getting started information. Please explore the links in the left-hand menu to view other relevant pages (recommended; examples follow)
- [https://www.first.org/epss/who_is_using/](https://www.first.org/epss/who_is_using/) - see who is using EPSS (identify software vendors; might be relevant for the context of the project)
- [https://www.first.org/epss/api](https://www.first.org/epss/api) - note that there is an API
- [https://github.com/empiricalsec/epss_scores/](https://github.com/empiricalsec/epss_scores/) - GitHub repository where you can find scores in the CSV format (compressed with gunzip), for example [epss_scores-2026-02-18.csv.gz](https://github.com/empiricalsec/epss_scores/blob/main/2026/epss_scores-2026-02-18.csv.gz)
- [https://www.cvedetails.com/](https://www.cvedetails.com/) - explore this website and observe different EPSS scores, for example [EPSS score history - Score changes > +-50%](https://www.cvedetails.com/epss/epss-score-history.html?delta=50)
- [https://epsslookuptool.com/](https://epsslookuptool.com/) Check EPSS score website

**Question:**  

Consider a scenario where a vulnerability management team prioritizes remediation based on two metrics:
* CVSS (impact severity)
* EPSS (probability of exploitation in the next 30 days)

Discuss how remediation priority should be assessed in the following scenarios:

1. High CVSS, low EPSS
2. Low CVSS, high EPSS
3. Medium CVSS, medium EPSS

For each case:
- What type of risk does it represent?
- What additional contextual factors would influence your decision?
- Would you treat it as urgent, scheduled, or low priority and why?

**Answer Q3.1:**

**Tipo de Risco**: Representa um risco de alto impacto, mas de baixa probabilidade imediata. A vulnerabilidade pode causar danos catastróficos se for explorada, mas atualmente não há provas de que os atacantes a estejam a usufruir dela.

**Fatores de Contexto Adicionais**: É fundamental avaliar se o sistema vulnerável está exposto à internet e qual o valor dos dados que processa. Além disso.

**Prioridade -> Scheduled**. Como a probabilidade de exploração a curto prazo é baixa, não justifica interromper o Business para uma correção de emergência. Deve entrar no ciclo normal de manutenção e patching, a menos que o ativo seja de criticidade máxima.

---

**Answer Q3.2:**

**Tipo de Risco**: Representa uma ameaça ativa e imediata, com alta probabilidade de ocorrência, mas com um impacto técnico mais limitado. Embora o dano inicial pareça pequeno, os atacantes estão a explorar ativamente esta falha na internet, podendo descubrir novas caracteristicas podendo compromoter ainda mais o sistema em si.

**Fatores de Contexto Adicionais**: O principal fator a avaliar é o *vulnerability chaining*: 
1. Pode esta Vulnerabilidade de baixo impacto ser usado em conjunto com outras para escalar privilégios ou mover-se lateralmente na rede? 
2. Também é crucial perceber se o ativo afetado serve de ponte para dados sensíveis.

**Prioridade -> Urgente**. Um EPSS alto significa que os atacantes já automatizaram ou estão a focar-se ativamente na exploração desta falha hoje. Corrigir vulnerabilidades que estão a ser exploradas na prática previne a evolução das consequências.

---

**Answer Q3.3:**

**Tipo de Risco**: Representa um risco moderado. Tem um impacto considerável e uma probabilidade de exploração razoável, embora não seja o alvo principal.

**Fatores de Contexto Adicionais**: A chave aqui é avaliar a tendência:

1. Qual é o histórico do EPSS para esta falha? Está a subir rapidamente nas últimas semanas ou estabilizou?

2. Existem *Compensatory Controls* já implementados (como uma Firewall ou WAF) que consigam bloquear as tentativas de exploração e reduzir a superfície de ataque?

3. ...

**Prioridade -> Scheduled**. Deve entrar no ciclo normal e planeado de manutenção e patching. Como não há uma exploração massiva nem um impacto destrutivo, não exige uma paragem de emergência. No entanto, requer *monitorização*!

---

<!-- ---------------------------------------------------------------------- -->

## Cooldown section

**Read a bit and take some notes:** 
1. [https://www.cvedetails.com/vulnerabilities-by-types.php](https://www.cvedetails.com/vulnerabilities-by-types.php) - Vulnerabilities By Types/Categories 
2. [https://nvd.nist.gov/vuln/data-feeds](https://nvd.nist.gov/vuln/data-feeds) - APIs
3. [https://www.cisa.gov/resources-tools/programs/coordinated-vulnerability-disclosure-program](https://www.cisa.gov/resources-tools/programs/coordinated-vulnerability-disclosure-program) - CVD


**Related to GitHub Advisory:**  
- ["Visual" Guide](https://github.nih.gov/advanced/security/advisories)
- [Concepts for vulnerability reporting and management](https://docs.github.com/en/code-security/concepts/vulnerability-reporting-and-management)

As you may have noticed in webpage 1., several high-ranking CWEs are related to memory safety.
1. How does programming language choice (e.g., C/C++ vs memory-safe languages) influence the persistence of these weaknesses?
2. Would widespread migration to memory-safe languages eliminate them from future Top 25 lists? Why or why not?

C/C++ allow undefined behavior, manual memory management, and unsafe pointer arithmetic which means that there is a higher likelihood of buffer overflows, Use-after-free, etc. Memory-safe languages (for instance, Rust) can drastically reduce certain classes, but not eliminate all top 25 issues because:
1. Many top weaknesses are not memory-safety (e.g., injection, auth, access control, misconfigurations).
2. Unsafe code still exists at [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface) boundaries, in legacy modules, or via unsafe blocks.


