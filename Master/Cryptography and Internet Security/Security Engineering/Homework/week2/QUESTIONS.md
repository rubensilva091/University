# Week 2 - Questions

**General Guidelines:**

1. Read each question carefully.

2. If links are provided, open them and spend some time exploring the page/document. The goal is to learn more than just the answer. If no links are provided, do your own research.

3. Never copy-paste: write answers in your own words (writing helps you learn). The answer can be written in portuguese or english, your choice.

4. There's no rush — take your time for each step.


**How to do this assignment:**

1. Create a new folder in your repository named `week2`. Copy this file into that folder (using the same name, `QUESTIONS.md`), then commit and push.

2. For each question, type your answer immediately after "Answer:".
   - For multiple-choice questions, write only the letter.
   - For open questions, write your answer in a single line.

3. After answering the first 3 questions, commit and push.
   - The commit message should be: "q1,q2,q3".

4. After every additional group of 3 questions, commit and push again.
   - Example: "q4,q5,q6", then "q7,q8,q9", and so on and so forth.

5. Repeat until you have completed all questions.

## Introduction

Information security is traditionally structured around three core properties: **Confidentiality**, **Integrity**, and **Availability** — commonly known as the **CIA triad**.


### Confidentiality
Confidentiality ensures that information is accessible **only to authorized entities**. It protects against unauthorized disclosure of data.

Typical mechanisms that support confidentiality include encryption, access control, authentication, and network segmentation.

If confidentiality is violated, sensitive information (for example, passwords, personal data, trade secrets) may be exposed to unauthorized parties.

### Integrity
Integrity ensures that data and systems remain **accurate, complete, and unaltered** unless modified by authorized actions. It protects against unauthorized modification, corruption, or tampering.

Mechanisms that support integrity include hashing, digital signatures, input validation, access control, and logging.

If integrity is violated, information can no longer be trusted — even if it remains confidential and available.

### Availability

Availability ensures that systems, services, and data are **accessible when needed by authorized users**. It protects against disruptions such as system crashes, denial-of-service attacks, hardware failures, or misconfigurations.

Mechanisms supporting availability include redundancy, backups, load balancing, and failover systems.

If availability is violated, even perfectly confidential and accurate data becomes useless because it cannot be accessed.

---

## Questions

### Q1. Which of the following are considered the three fundamental properties of security?

A. Confidentiality, Integrity, Availability  
B. Confidentiality, Inspection, Authentication  
C. Control, Isolation, Authorization  
D. Confidentiality, Identification, Accountability  

**Answer:A**

---

### Q2. A database containing student grades is accidentally exposed on a public web server. Which property has been violated?

A. Integrity  
B. Availability  
C. Confidentiality  
D. Non-repudiation  

**Answer:C**

---

### Q3. An attacker captures unencrypted traffic on a Wi-Fi network and reads user credentials. Which security property failed?

A. Confidentiality  
B. Integrity  
C. Availability  
D. Accountability  

**Answer:A**

---

### Q4. A company encrypts all employee laptops to prevent data exposure if devices are stolen. Which property is primarily being protected?

A. Integrity  
B. Confidentiality  
C. Availability  
D. Performance  

**Answer:B**

---

### Q5. A malicious insider alters financial transaction records without authorization. Which property is compromised?

A. Confidentiality  
B. Availability  
C. Integrity  
D. Privacy  

**Answer:C**

---

### Q6. A file is transferred over a network along with a cryptographic hash. The receiver verifies the hash before accepting the file. Which property is primarily being protected?

A. Confidentiality  
B. Integrity  
C. Availability  

**Answer:B**

---

### Q7. An attacker modifies a software update package before it is downloaded by users. What property is primarily at risk?

A. Integrity  
B. Availability  
C. Confidentiality  
D. Redundancy  

**Answer:A**

---

### Q8. A distributed denial-of-service (DDoS) attack overwhelms a public website, making it inaccessible. Which property is violated?

A. Confidentiality  
B. Integrity  
C. Availability  
D. Authorization  

**Answer:C**

---

### Q9. A ransomware attack encrypts a company's files and prevents employees from accessing them. Which property is primarily impacted?

A. Confidentiality  
B. Integrity  
C. Availability  
D. Authenticity  

**Answer:C**

---

### Q10. A cloud service deploys redundant servers in multiple geographic regions to ensure uptime during outages. Which property is this measure primarily supporting?

A. Confidentiality  
B. Integrity  
C. Availability  
D. Non-repudiation  

**Answer:C**

---

### Q11. What is a vulnerability in the context of software security? (open question)

**Answer: Uma vulnerabilidade é uma falha ou fraqueza no design, implementação, software, hardware... que pode ser explorada por um atacante para comprometer a segurança da informação.**

---

### Q12. What does CVE stand for?

Visit the link and read a bit even if you already know what CVE stands for: [https://www.cve.org/ResourcesSupport/Glossary](https://www.cve.org/ResourcesSupport/Glossary)

A. Common Vulnerability Enumeration  
B. Common Vulnerabilities and Exposures  
C. Critical Vulnerability Evaluation  
D. Cyber Vulnerability Entry  

**Answer:B**
(curiosidade, em alguns autores, a opção "A" também pode estar correta, pois era o nome inicialmente pensado para isso... o que bate certo)
(No SCAP -> CCE/CPE/CVE... "E" fica sempre para "Enumeration")

---

### Q13. What problem does the CVE system solve in vulnerability management? (open question)

**Answer: O sistema CVE resolve a falta de padronização na identificação de falhas, permitindo que diferentes ferramentas e equipas usem um nome único e universal para a mesma vulnerabilidade. É de extrema importância para sistemas de Vulnerability Feed para que todos os scanners funcionem de uma maneira padronizada ajudando assim a identificação de vulnerabilidades nos sistemas autómaticos (Nessus, OpenVAS i.e)**

---

### Q14. What does CWE stand for?

Same here, read a bit: [https://cwe.mitre.org/about/new_to_cwe.html](https://cwe.mitre.org/about/new_to_cwe.html)

A. Common Weakness Enumeration  
B. Critical Web Exploit  
C. Common Web Exposure  
D. Cyber Weakness Entry  

**Answer:A**

---

### Q15. Explain the difference between a vulnerability and a weakness. (open question)

Link: [https://cwe.mitre.org/about/faq.html](https://cwe.mitre.org/about/faq.html)

**Answer: Uma fraqueza (CWE) é o tipo de erro técnico ou falha de design conceptual, enquanto uma vulnerabilidade (CVE) é a manifestação específica e explorável dessa fraqueza num software real.**

---

### Q16. What is the key difference between CVE and CWE?

A. CVE is private; CWE is public  
B. CVE describes specific vulnerabilities; CWE describes classes of weaknesses  
C. CVE is for hardware only; CWE is for software only  
D. There is no difference  

**Answer:B**

---

### Q17. What is MITRE and what role does it play in the CVE ecosystem? (open question)

Link: [https://www.cve.org/About/History](https://www.cve.org/About/History)

**Answer: A MITRE é uma organização sem fins lucrativos que gere e mantém o sistema CVE, atuando como a autoridade central que coordena a atribuição de identificadores em todo o mundo.**

---

### Q18. Which organization operates the National Vulnerability Database (NVD)?

Link: [https://nvd.nist.gov/general](https://nvd.nist.gov/general)

A. MITRE  
B. NSA  
C. NIST  
D. FIRST  

**Answer: C**

---

### Q19. What additional information does the NVD provide beyond the basic CVE description? (open question)

Read section "NVD CVE Enrichment" [https://nvd.nist.gov/general/cve-process](https://nvd.nist.gov/general/cve-process)

**Answer: A NVD fornece enriquecimento de dados, incluindo a pontuação de gravidade (CVSS), o mapeamento para categorias CWE e a identificação de produtos afetados via CPE.**

---

### Q20. What is a CNA (CVE Numbering Authority)?

Link1: [https://www.cve.org/programorganization/cnas](https://www.cve.org/programorganization/cnas)  
Link2: [https://www.cve.org/PartnerInformation/ListofPartners](https://www.cve.org/PartnerInformation/ListofPartners)  

A. An organization authorized to assign CVE identifiers  
B. A vulnerability scanning engine  
C. A severity scoring system  
D. A secure coding framework  

**Answer: A**

---

### Q21. Describe the typical lifecycle of a CVE from discovery to publication. (open question)

Link: [https://www.cve.org/About/Process](https://www.cve.org/About/Process)

**Answer: Discover -> Report -> Request -> Reserve -> Submit -> Publish**

---

### Q22. What does it mean when a CVE is "reserved"?

A. The vulnerability is fixed  
B. The vulnerability is classified  
C. An identifier has been allocated but details may not yet be public  
D. The CVE has been deleted  

**Answer: C**

---

### Q23. What is CVSS and why is it important? (open question)

Link1: [https://www.first.org/cvss/](https://www.first.org/cvss/)  
Link2: [https://www.first.org/cvss/v4.0/user-guide](https://www.first.org/cvss/v4.0/user-guide)  
Link3: [https://www.cve.org/CVERecord/UserGuide/](https://www.cve.org/CVERecord/UserGuide/)  

**Answer: O CVSS é um sistema de pontuação universal que quantifica a gravidade de uma vulnerabilidade, tendo em conta várias caracteristicas técnicas da vulnerabilidade, permitindo seja priorizado a correção de falhas com base no risco técnico.**

---

### Q24. Which of the following is NOT a CVSS metric group?

Link: [https://www.first.org/cvss/v4.0/user-guide](https://www.first.org/cvss/v4.0/user-guide)

A. Base  
B. Threat  
C. Environmental  
D. Supplemental  
E. Organizational  

**Answer: E**

---

### Q25. Why might two organizations assign different CVSS scores to the same CVE? (open question)

Read section "3.10 Multiple CVSS Base (CVSS-B) Scores" from [https://www.first.org/cvss/v4.0/user-guide](https://www.first.org/cvss/v4.0/user-guide)

**Answer: As organizações podem atribuir pontuações diferentes devido a interpretações distintas das diretrizes, disparidade nas informações disponíveis no momento da análise ou variações nas configurações e implementações específicas do software.**

---

### Q26. How do security scanners typically use CVE IDs?

A. To generate encryption keys  
B. To match detected vulnerabilities against known issues  
C. To identify software licenses  
D. To classify network traffic  

**Answer: B**

---

### Q27. What is an SBOM and how does it help with CVE management? (open question)

Link1: [https://www.nist.gov/itl/executive-order-14028-improving-nations-cybersecurity/software-security-supply-chains-software-1](https://www.nist.gov/itl/executive-order-14028-improving-nations-cybersecurity/software-security-supply-chains-software-1)  
Link2: [https://cyclonedx.org/](https://cyclonedx.org/)  
Link3: [https://cyclonedx.org/guides/OWASP_CycloneDX-Authoritative-Guide-to-SBOM-en.pdf](https://cyclonedx.org/guides/OWASP_CycloneDX-Authoritative-Guide-to-SBOM-en.pdf)  
Link4: [https://spdx.dev/](https://spdx.dev/)  
Link5: [https://spdx.dev/wp-content/uploads/sites/31/2024/12/SPDX-3.0.1-1.pdf](https://spdx.dev/wp-content/uploads/sites/31/2024/12/SPDX-3.0.1-1.pdf)  

**Answer: Um SBOM é um inventário detalhado de todos os componentes de um software que permite identificar rapidamente quais as dependências ou bibliotecas específicas que possuem vulnerabilidades (CVEs) conhecidas.**

---

### Q28. Can multiple CVEs map to the same CWE?

A. No  
B. Yes, multiple vulnerabilities can share the same underlying weakness  
C. Only for open-source software  
D. Only if assigned by the same CNA  

**Answer: B**

---

### Q29. Discuss one limitation of CVSS as a real-world risk metric. (open question)

Suggested link: [https://www.advens.com/en/media/cyber-news-en/regard-critique-cvss/](https://www.advens.com/en/media/cyber-news-en/regard-critique-cvss/)

Do your own research as well.

**Answer: Uma das limitações do CVSS é que a pontuação base não considera se a vulnerabilidade está a ser ativamente explorada no momento ou qual é a importância crítica do sistema afetado para a organização.**

---

### Q30. What is the primary purpose of CISA's (Cybersecurity and Infrastructure Security Agency) Known Exploited Vulnerabilities (KEV) catalog?

Link1 (short description for KEV): [https://www.cve.org/CVERecord/UserGuide](https://www.cve.org/CVERecord/UserGuide)  
Link2: [https://www.cisa.gov/known-exploited-vulnerabilities-catalog](https://www.cisa.gov/known-exploited-vulnerabilities-catalog)  

A. To assign CVE IDs  
B. To list vulnerabilities actively exploited in the wild  
C. To replace the NVD  
D. To classify weaknesses  

**Answer: B**

---

### Notes

For the following questions, consider the information from the following two links:

1. [https://cwe.mitre.org/top25/archive/2025/2025_cwe_top25.html](https://cwe.mitre.org/top25/archive/2025/2025_cwe_top25.html)

2. [https://cwe.mitre.org/top25/archive/2025/2025_kev_list.html](https://cwe.mitre.org/top25/archive/2025/2025_kev_list.html)

---

### Q31. What is the primary difference between the 2025 CWE Top 25 and the 2025 KEV-related CWE list?

A. The Top 25 lists CVEs, while the KEV list lists CWEs  
B. The Top 25 is based on statistical prevalence and severity, while the KEV list reflects vulnerabilities actively exploited in the wild  
C. The KEV list is theoretical, while the Top 25 is based on real-world data  
D. There is no difference  

**Answer: B**

---

### Q32. If a weakness appears high in the Top 25 ranking but not prominently in the KEV-based list, what is the most reasonable interpretation?

A. The weakness is rare in software  
B. The weakness is common and severe, but not currently widely exploited  
C. The weakness has been fully mitigated  
D. The weakness only affects legacy systems  

**Answer: B**

---

### Q33. What does inclusion of a weakness in the KEV-related CWE list most strongly indicate?

A. It is easy to detect automatically  
B. It frequently appears in academic papers  
C. It is associated with vulnerabilities that attackers are actively exploiting  
D. It only affects open-source software  

**Answer: C**

---

### Q34. Suppose a weakness ranks moderately in the Top 25 but very high in the KEV-based list. What should be the most reasonable response from a security team?

A. Ignore it because it is not high in the Top 25  
B. Prioritize mitigation because it is actively exploited  
C. Remove it from training materials  
D. Wait for next year’s ranking  

**Answer: B**

---

### Q35. Why do weaknesses such as injection or memory safety issues frequently appear in both the Top 25 and KEV-related lists?

A. They are easy to patch automatically  
B. They are deeply rooted in programming language design and common development mistakes  
C. They only affect web applications  
D. They are newly discovered weaknesses  

**Answer: B**

---

### Q36. Consider the following information:

1. Read the description of `CWE-78: Improper Neutralization of Special Elements used in an OS Command ('OS Command Injection')`, available in the following link: [https://cwe.mitre.org/data/definitions/78.html](https://cwe.mitre.org/data/definitions/78.html)

2. In the section `Selected Observed Examples` (of the previous link), you may find a reference to [`CVE-2025-44844`](https://www.cve.org/CVERecord?id=CVE-2025-44844). Read the description.

3. Check for more details here: [https://github.com/Summermu/VulnForIoT/blob/main/Totolink_CA600-PoE/setUpgradeFW/readme.md](https://github.com/Summermu/VulnForIoT/blob/main/Totolink_CA600-PoE/setUpgradeFW/readme.md)

Task: check if the project that you developped during week's 1 assignment is susceptible to a similar attack. 

**A. Did you found something interesting: O week1 não é vulnerável ao "OS Command Injection" (CVE-2025-44844) pois o server.py usa funções nativas do Python para gerir ficheiros, tratando inputs como nomes literais sem invocar a shell do sistema. Além disso, o uso de UUIDs para renomear ficheiros no disco neutraliza ataques via nomes de ficheiros originais.**

### Q37. Hands-on section.

1. Install and configure `docker` on your machine. Make sure you are able to run `docker run hello-world` and see something like:
```
$ docker run hello-world
Unable to find image 'hello-world:latest' locally
latest: Pulling from library/hello-world
17eec7bbc9d7: Pull complete 
Digest: sha256:ef54e839ef541993b4e87f25e752f7cf4238fa55f017957c2eb44077083d7a6a
Status: Downloaded newer image for hello-world:latest

Hello from Docker!
This message shows that your installation appears to be working correctly.

To generate this message, Docker took the following steps:
 1. The Docker client contacted the Docker daemon.
 2. The Docker daemon pulled the "hello-world" image from the Docker Hub.
    (amd64)
 3. The Docker daemon created a new container from that image which runs the
    executable that produces the output you are currently reading.
 4. The Docker daemon streamed that output to the Docker client, which sent it
    to your terminal.

To try something more ambitious, you can run an Ubuntu container with:
 $ docker run -it ubuntu bash

Share images, automate workflows, and more with a free Docker ID:
 https://hub.docker.com/

For more examples and ideas, visit:
 https://docs.docker.com/get-started/
```

2. Get [`trivy`](https://trivy.dev/docs/latest/getting-started/) to work on your machine;

3. Scan your last weeks' project (discover how). 

**A. Vulnerabilities found: O scan inicial ao diretório do projeto (trivy fs .) não detetou vulnerabilidades nas dependências do requirements.txt. Seguindo as instruções do guião para casos sem vulnerabilidades no projeto, efetuei um scan à imagem python:3.9-slim, que revelou 103 vulnerabilidades no sistema operativo (3 CRITICAL, 8 HIGH) e 6 em pacotes Python (3 HIGH), destacando-se falhas graves no OpenSSL (CVE-2025-15467) e na biblioteca wheel (CVE-2026-24049).**

4. Fix it :-) (if applicable: from the 26 students that pushed a `requirements.txt` file, 19 have dependencies with issues)

**A. Was the fix difficult? Describe if the problems that you found directly affected the security of the project: A correção não é difícil, bastando atualizar a imagem base ou os pacotes específicos para as versões corrigidas indicadas pelo Trivy. Estes problemas afetam criticamente a segurança, pois embora o meu código Flask esteja limpo, ele corre sobre uma infraestrutura vulnerável; falhas no OpenSSL ou na glibc podem permitir a execução remota de código (RCE) ou negação de serviço (DoS), comprometendo todo o ambiente da aplicação.**

5. If you didn't find any issues with last week's project, scan your machine (this also applies to the remaining students).

END

