# Security in the SDLC with NIST SP 800-218

## Covered Topic
Security in the software development life cycle (SDLC): SDLC models; NIST SSDF framework.

## Introduction to SDLC 

Software Development Life Cycle (SDLC) is the structured process used to plan, design, build, test, deploy, and maintain software. It helps teams organize development work, assign responsibilities, and improve the quality of the final product. Common SDLC models include waterfall, iterative development, agile, and DevOps-oriented approaches.

A DevOps-oriented approach is an SDLC style where development and operations work closely together, with strong use of automation for building, testing, deploying, and monitoring software. It emphasizes fast, frequent changes and continuous feedback instead of treating development and deployment as separate stages.

From a security perspective, the SDLC matters because weaknesses introduced in early phases can remain in the system until deployment, where they are usually more expensive and harder to fix. A secure SDLC therefore integrates security activities throughout the lifecycle, rather than treating security as a final testing step. This is the main idea behind frameworks such as the NIST Secure Software Development Framework (SSDF).

Complementary reading material: some of the following pages include insights on how the SDLC addresses security. Check the corresponding sections and see how they relate to DevSecOps.
- [What is the SDLC?](https://github.com/resources/articles/what-is-sdlc) - GitHub's overview of the SDLC.
- [What is SDLC?](https://aws.amazon.com/what-is/sdlc/) - AWS's overview of the SDLC.
- [Secure Software Development Lifecycle (SSDLC)](https://snyk.io/articles/secure-sdlc/) - Snyk's overview of SDLC and Secure SDLC. See in particular the figure in the section *5 phases of Secure Software Development Life Cycle*.
- [OWASP in SDLC](https://owasp.org/www-project-integration-standards/writeups/owasp_in_sdlc/) - OWASP's perspective on integrating security into the SDLC.

## Context

The following exercises are based on [NIST SP 800-218 — Secure Software Development Framework (SSDF) Version 1.1](https://csrc.nist.gov/pubs/sp/800/218/final). Alongside this file, you will also find an annotated PDF with selected passages highlighted by the instructor.

This NIST Special Publication is part of the broader [SSDF Project](https://csrc.nist.gov/projects/ssdf). Note that an Initial Public Draft of the next revision is already available: [SSDF Version 1.2](https://csrc.nist.gov/pubs/sp/800/218/r1/ipd). Since this draft is not yet final and may still contain issues under review, the following exercises are based on **Version 1.1**.

Still, it is worth keeping an eye on the progress of Version 1.2, especially if you expect to encounter the SSDF in your professional career.

The goal is not memorization. Understand how the SSDF is structured, and connect its practices to real software development decisions.

## Instructions
- Answer all 10+1 questions.
- Justify your answers with reference to the document.
- When appropriate, cite **SSDF practice/task identifiers** such as **PO.3**, **PW.1.1**, or **RV.2.2**.
- Keep answers concise, but grounded in the text.

---





## Question 1 - Why is the SSDF not "just another SDLC model"

**Context.** In the introduction, the document notes that there are many SDLC models - such as waterfall, spiral, agile, and DevOps - but few address software security in enough detail. Instead of proposing a new SDLC model, NIST defines a set of high-level secure development practices that can be integrated into existing models.

**Question.** Explain why the SSDF was designed as a **set of integrable practices** instead of a new SDLC model. In your answer, identify **two advantages** of this choice and explain how it helps organizations with different contexts adopt the framework.

**Reference pages:** **p. 1-4**.

### Answer

The SSDF is a set of practices because NIST wanted something organizations could layer onto existing SDLC models instead of replacing them. Two advantages are flexibility and reuse: teams can adopt the practices in waterfall, agile, DevOps, or hybrid processes, and they can map the same high-level controls to different organizational contexts without redesigning the whole lifecycle. This makes adoption easier for organizations with different maturity levels, toolchains, and regulatory constraints.



---





## Question 2 - "Shift left" and the cost of fixing security problems

**Context.** The document states that security should be addressed as early as possible in the SDLC. The idea of **shifting left** is emphasized because it can reduce effort, cost, and technical debt while also improving software security and resiliency.

**Question.** Explain the meaning of **shifting left** in the SSDF. Then give **two concrete examples** of SSDF activities that illustrate this idea, and justify why doing them early is better than postponing them until late development or after deployment.

**Reference pages:** **p. 1**, **p. 5 (PO.1)**, and **p. 11 (PW.1)**.

### Answer

“Shifting left” means moving security work earlier in the SDLC, before code is finished or released. Two examples are PW.1.1 threat modeling and PW.2.1 design review: both catch design and risk issues when they are still cheap to change. Doing them late means rework, more technical debt, and higher chances of shipping weaknesses that should have been prevented.



---





## Question 3 - SSDF structure: the logic of the four practice groups

**Context.** The SSDF organizes its practices into four groups:
 - **PO (Prepare the Organization)**
 - **PS (Protect the Software)**
 - **PW (Produce Well-Secured Software)**
 - **RV (Respond to Vulnerabilities)**

This structure suggests that software security is not only about writing secure code.

**Question.** Describe the function of each of the four practice groups. Then explain why it would be a mistake - both conceptually and organizationally - to reduce the SSDF to only the **PW (Produce Well-Secured Software)** group.

**Reference pages:** **p. 4**.

### Answer

PO prepares the organization: requirements, roles, training, tools, checks, and environment security. PS protects the software and its artifacts during development and distribution. PW covers the practices for building secure software, from design through code, third-party reuse, and testing. RV handles post-release vulnerability intake, triage, remediation, and learning. Reducing SSDF to PW misses the organizational, operational, and feedback-loop parts that make secure development actually work.



---




## Question 4 - Security requirements across the lifecycle

**Context.** In **PO.1**, the SSDF states that security requirements for software development and for the software itself should be identified, documented, maintained over time, and communicated to relevant parties. The text also notes that these requirements may come from internal and external sources.

**Question.** Compare **PO.1.1** and **PO.1.2**. What distinguishes security requirements for **development infrastructure/processes** from security requirements for the **software being developed**? Give one plausible example of each and explain why keeping the distinction is useful.

**Reference pages:** **p. 5**.

### Answer

PO.1.1 is about security requirements for the development infrastructure and processes themselves, such as how repos, endpoints, pipelines, and build systems must be secured. PO.1.2 is about security requirements for the software product being built, such as authentication, data protection, or logging features. For example, a development-process requirement could be mandatory MFA for build-system access, while a software requirement could be role-based access control in the application. The distinction is useful because the SDLC has to secure both the factory and the product.



---





## Question 5 - Organization matters: roles, training, and management commitment

**Context.** The **PO** group is not only about tools. The document also addresses roles and responsibilities (**PO.2.1**), role-based training (**PO.2.2**), and commitment from upper management (**PO.2.3**). The implicit message is that secure development does not depend only on individual technical skill.

**Question.** Imagine a small team says: "We do not need formal roles or specific training; it is enough to tell developers to be careful about security." Using the SSDF, critique this position. In your answer, explain the usefulness of **roles**, **role-based training**, and **management commitment**, and show how these three elements reinforce one another.

### Answer

The SSDF says roles matter because secure development is distributed work, not just developer discipline. PO.2.1 defines who is responsible for what, PO.2.2 gives people the training needed for their role, and PO.2.3 ensures management backs the work with authority and resources. Together, roles prevent gaps, training gives people the capability to act, and management commitment makes the process real instead of optional.



---





## Question 6 - Tools, automation, criteria, and evidence

**Context.** In **PO.3** and **PO.4**, the SSDF emphasizes toolchains, automation, security check criteria, and the generation of artifacts that show what was done. At the same time, the document explicitly says that the SSDF is not meant to be treated as a simple checklist.

**Question.** According to the SSDF:
1. What is the purpose of using automation in secure software development?
2. How does automation help with: performing security checks consistently, and producing artifacts or evidence that document those checks?
3. 3. Then discuss one risk of relying on many security tools without prioritizing their use based on risk (i.e., without a risk-based approach). Hint: **too many warnings can lead to alert fatigue**, causing important warnings to be overlooked. 

### Answer

1. Automation is used to reduce human effort and make secure practices repeatable and consistent, not to replace judgment. 2. PO.3.3 and PO.4.2/PO.4.1 show that tools can run checks consistently and generate artifacts such as logs, reports, and evidence that the checks happened. 3. A risk-based approach matters because too many warnings can create alert fatigue; then important findings get buried and the team stops reacting effectively.



---





## Question 7 - Why secure the development environment?

**Context.** In **PO.5**, the SSDF treats development, build, test, and distribution environments as critical assets. It recommends separation of environments, strong access control, monitoring, and measures that reduce lateral movement.

**Question.** Why does the SSDF devote so much attention to securing development environments and not only the final code? Relate your answer to the possible impact of **an intrusion into a build environment or a developer endpoint** (the developer's machine). Identify at least **three measures** suggested in **PO.5** and explain which problem each one is intended to reduce.

### Answer

SSDF devotes attention to environments because compromise there can undermine every build, test, or release. An intrusion into a build environment can inject malicious code into trusted artifacts; a developer endpoint compromise can leak source, secrets, or credentials. Three PO.5 measures are especially important: separate and protect each environment to limit blast radius, secure and harden developer endpoints to reduce credential theft and tampering, and monitor environments to detect intrusion and lateral movement early. Strong access control supports all three by making compromise harder in the first place.



---





## Question 8 - Secure design before implementation

**Context.** The **PW** group begins with design. In **PW.1**, the document discusses risk modeling, threat modeling, and design decisions. In **PW.2**, it requires design review by qualified people or appropriate automated processes.

**Question.** Explain why the SSDF places risk analysis (**PW.1**) and design review (**PW.2**) before (for example) coding (**PW.5**), review (**PW.7**), and testing (**PW.8**). Then choose one kind of software (for example, a web application, mobile app, API, or cloud service) and identify **two design decisions** that should emerge from this early stage (**PW.1**).

### Answer

PW.1 comes before coding and testing because design choices define the attack surface and determine which problems are even possible later. PW.2 then checks that the design actually satisfies the identified risks before implementation hardens them into code. For a web application, two early design decisions are to enforce authentication/authorization centrally and to separate sensitive data flows from untrusted input flows. Those decisions should come from threat modeling and design review, not from code review after the fact.



---





## Question 9 - Reusing third-party components without trusting them blindly

**Context.** In **PW.4**, the SSDF encourages reuse of existing well-secured components when feasible, but it also requires ongoing verification of third-party software, including provenance, known vulnerabilities, maintenance status, and information such as SBOMs.

**Question.** The SSDF recommends reusing existing well-secured components when feasible (**PW.4.1**), but it also says that third-party components should not be trusted blindly (**PW.4.4**). Explain why these two ideas are compatible. In your answer, discuss how the SSDF treats third-party component risk over time and why that risk does not end when a component is first integrated.

### Answer

PW.4.1 encourages reuse because well-secured components can reduce development cost and avoid duplicating risky functionality. PW.4.4 says not to trust them blindly because component risk changes over time: new vulnerabilities appear, maintenance ends, provenance can be questioned, and the component may be used in a different context than originally reviewed. That is why the SSDF requires ongoing verification, not just one-time acceptance.



---





## Question 10 - Vulnerabilities after release: closing the loop

**Context.** The **RV** group shows that security work does not end at release. The document addresses gathering reports, validating vulnerabilities, disclosure programs, prioritization, remediation, root-cause analysis, and updating the SDLC to avoid recurrence.

**Question.** Explain how **RV.1**, **RV.2**, and **RV.3** close the secure development loop. In particular, show the difference between:
1. **identifying** a vulnerability,
2. **deciding on and executing** the response to it, and
3. **learning from it** to improve the development process.

End by giving one example of how a lesson learned under **RV.3** could change earlier SSDF practices, such as in **PO**, **PW**, or both.

### Answer

RV.1 is about identifying and gathering vulnerability reports, RV.2 is about analyzing each vulnerability and deciding the response, and RV.3 is about learning from patterns and root causes so the same class of issue is less likely to recur. In other words, identify, respond, and improve. A lesson from RV.3 could change PO by strengthening requirements for secure development environments or change PW by adding a design rule or coding practice that prevents the recurring flaw.



---





## Final Question.

This exercise sheet is a guided tour, not the whole map. 

Explain why it is still necessary to study the SSDF in depth, **keep an eye on updates to the main document**, and approach secure software development as a continuous process of analysis, revision, and improvement rather than a one-and-done checklist.

### Answer

The SSDF is useful as a living framework, so it has to be studied in depth and revisited as NIST updates the document and the threat landscape changes. Secure development is not a checklist you finish once; it is a cycle of defining requirements, building with them, checking results, responding to issues, and improving the process. Keeping up with updates matters because the framework’s practices, examples, and mappings evolve, and organizations need to adapt without losing the security intent.



---


