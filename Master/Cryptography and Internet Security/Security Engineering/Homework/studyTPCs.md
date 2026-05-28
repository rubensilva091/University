# Study TPCs - Compiled Revision Guide

## How to use this guide
- Each week is compressed into the concepts that matter most for revision.
- Focus on relationships, not on memorizing worksheet phrasing.
- Use the key terms as a glossary and the exam lens as the part to rehearse out loud.

## Week 1 - File Vault API

### Core idea
Build a small authenticated file service where every file action depends on an API key, user ownership, and persisted metadata. The key security pattern is: authenticate first, check ownership second, then read or modify the stored file.

### Key terms
- API key: caller credential.
- Authentication: identify the caller.
- Ownership: decide which files belong to that caller.
- Metadata: stores owner, original name, and file details.
- Trust boundary: place where user input must be revalidated.
- Encryption at rest: protect stored file bytes on disk.

### Exam lens
- A server-generated file ID is safer than using the uploaded filename.
- The filename is display data, not a storage key.
- Metadata is what ties bytes to identity and access control.
- Persistence matters because the service must survive restarts without losing files or ownership records.

### Common mistakes
- Authentication is not the same as authorization.
- Encryption does not replace access control.
- User input should never decide the storage path.

## Week 2 - Security Fundamentals and Vulnerability Management

### Core idea
Security work is about naming, classifying, scoring, and prioritizing flaws so teams can respond consistently. The important distinction is between a weakness as a flaw class and a vulnerability as an exploitable instance.

### Key terms
- CIA triad: confidentiality, integrity, availability.
- Weakness: root flaw pattern.
- Vulnerability: exploitable instance.
- CVE: shared identifier for a specific vulnerability.
- CWE: family of related weakness patterns.
- CVSS: technical severity score.
- NVD: enriched vulnerability database.
- SBOM: inventory of components.
- KEV: vulnerabilities actively exploited in the wild.

### Exam lens
- CVE names the issue; CWE explains the class of mistake.
- CVSS estimates severity, not actual priority.
- KEV and exposure context often matter more than score alone.
- SBOMs help connect vulnerable dependencies to real systems.

### Common mistakes
- A high CVSS score does not automatically mean highest risk.
- A clean application layer can still run on a vulnerable base image.
- Scanning only makes sense if you know the scope.

## Week 3 - Security Metrics, Software Identity, and Prioritization

### Core idea
Security ranking systems are models, not truth. Their output depends on the data they see, the identifiers they can match, and the assumptions used to measure severity or likelihood of exploitation.

### Key terms
- CPE: rigid catalog-style software identifier.
- PURL: package-aware identifier for ecosystems and SBOMs.
- CVSS: severity-focused scoring.
- EPSS: likelihood-of-exploitation scoring.
- Bias: systematic skew from data or scoring assumptions.
- Memory-safe language: reduces memory corruption bugs, not logic flaws.

### Exam lens
- CVSS answers how bad; EPSS answers how soon.
- CPE and PURL are not interchangeable.
- Rankings near the cutoff should be treated as approximate.
- Prioritization combines severity, probability, exposure, and asset value.

### Common mistakes
- A ranking is decision support, not a final verdict.
- Memory safety does not solve authorization, injection, or misconfiguration.
- Better identifiers improve matching, not risk by themselves.

## Week 4 - Threat Modeling with STRIDE

### Core idea
Threat modeling is the design-time practice of identifying assets, trust boundaries, attack surfaces, threats, and mitigations before implementation locks the design in place. STRIDE is the threat taxonomy used to structure that thinking.

### Key terms
- Scope: the system and data in play.
- DFD: data flow diagram.
- Trust boundary: place where assumptions change.
- STRIDE: Spoofing, Tampering, Repudiation, Information Disclosure, Denial of Service, Elevation of Privilege.
- Mitigation: control that reduces likelihood or impact.

### Exam lens
- Start with scope before listing threats.
- Use the DFD to find where trust shifts.
- One component can map to multiple STRIDE categories.
- Threats describe what can go wrong; mitigations are the controls.

### Common mistakes
- Threat modeling is not the same as vulnerability scanning.
- A good model is updated when the design changes.
- A mitigation can reduce more than one threat.

## Week 6 - Git, CI/CD, DevSecOps, and Self-Hosted Runners

### Core idea
The delivery pipeline is part of the security boundary. Repository access, commit authenticity, build automation, and runner trust all affect whether the final artifact can be trusted.

### Key terms
- SSH: protects repository access and transport.
- Commit signing: proves a commit was created by the private key holder.
- CI: continuous integration.
- CD: continuous delivery or deployment.
- DevOps: shared development and operations responsibility.
- DevSecOps: security built into the pipeline.
- Self-hosted runner: workflow execution on infrastructure you control.

### Exam lens
- SSH and commit signing protect different things.
- CI is not the same as deployment.
- A self-hosted runner becomes trusted infrastructure and part of the attack surface.
- Security checks are most effective when added early in the pipeline.

### Common mistakes
- Authentication to Git is not the same as provenance of the code.
- A pipeline can be automated and still be insecure.
- Runner secrets and permissions need least privilege.

## Week 7 - Secure SDLC and NIST SSDF

### Core idea
Security should be integrated across the full software life cycle, not added at the end. SSDF provides a practical security layer on top of the SDLC with four practice groups: Prepare the Organization, Protect the Software, Produce Well-Secured Software, and Respond to Vulnerabilities.

### Key terms
- SDLC: plan, design, build, test, deploy, operate, maintain.
- SSDF: secure development practices.
- PO: prepare the organization.
- PS: protect the software and artifacts.
- PW: produce well-secured software.
- RV: respond to vulnerabilities.

### Exam lens
- SDLC is the lifecycle; SSDF is the security framework applied inside it.
- Early design and process choices shape later security outcomes.
- Vulnerability response must feed back into requirements, design, code, and testing.

### Common mistakes
- SSDF is not a new SDLC model.
- Security is a process property, not a final checkpoint.
- Third-party dependency risk must be managed over time.

## Week 8 - Static and Dynamic Analysis of C Code

### Core idea
Static analysis and dynamic analysis complement each other. Static tools inspect code without running it; dynamic tools observe the running program and confirm how bugs behave on real inputs.

### Key terms
- Static analysis: inspect without execution.
- Dynamic analysis: inspect while running.
- Sanitizer: runtime bug detector.
- Bounds checking: verify sizes and indices.
- Undefined behavior: behavior the language does not define safely.
- Ownership: who is responsible for memory or resources.

### Exam lens
- Compiler warnings are useful but incomplete.
- Static analysis is broad; dynamic analysis is concrete.
- Common bug classes include buffer overflow, null dereference, use-after-free, double free, and uninitialized use.
- A clean run from one tool is not proof of safety.

### Common mistakes
- Runtime tools show behavior, not necessarily exploitability.
- Bugs often depend on specific inputs.
- Secure C code is mostly about validation, bounds, and ownership discipline.

## Week 9 - SAFECode and Secure Development Practices

### Core idea
SAFECode frames security as disciplined development practice: secure design, threat modeling, input handling, third-party risk management, testing, and response all belong to the secure development lifecycle.

### Key terms
- SAFECode: practical SDL guidance.
- Default deny: deny unless explicitly allowed.
- Complete mediation: check access every time.
- Least privilege: give only the needed access.
- Boundary validation: recheck at every trust boundary.
- SAST: static application security testing.
- DAST: dynamic application security testing.

### Exam lens
- SAFECode is guidance, not a rigid standard.
- SAST and DAST are complementary.
- Good processes create fewer recurring flaws.
- Findings matter only if they are triaged, fixed, verified, and fed back.

### Common mistakes
- Security should be built in, not bolted on.
- One boundary check is not enough.
- Reused components still need active risk tracking.

## Week 10 - Secure APIs

### Core idea
Authentication gets you into the API, but authorization decides what you can actually do. Secure APIs need object-level checks, correct token validation, safe query construction, and awareness that internal endpoints still require protection.

### Key terms
- OAuth: delegated authorization.
- OpenID Connect: identity layer on top of OAuth-style flows.
- Scope: broad token claim.
- Permission: concrete backend allow/deny decision.
- Role: identity attribute used by policy.
- Audience claim: binds a token to a specific service.
- BOLA/IDOR: broken object-level authorization / insecure direct object reference.

### Exam lens
- A valid token is not enough by itself.
- Scope is not the same as permission.
- Object-level authorization is required for resource endpoints.
- Parameterized queries prevent SQL injection by separating data from SQL syntax.

### Common mistakes
- Authentication and authorization are separate problems.
- Claims can be stale or misapplied.
- Internal endpoints need the same access control thinking as public ones.

## Week 11 - Symbolic Execution with KLEE

### Core idea
KLEE symbolically executes LLVM bitcode, explores feasible paths, and uses a solver to generate concrete tests. It is useful for test generation and bug finding, especially around edge cases and assertion failures, but it can suffer from path explosion.

### Key terms
- Symbolic execution: explore code with unknown inputs.
- klee_make_symbolic: mark input as symbolic.
- klee_assert: turn a property into a check.
- LLVM bitcode: the compiled form KLEE executes.
- POSIX runtime: models argv, files, and environment.
- Path condition: constraints for one path.
- Path explosion: exponential growth in paths.

### Exam lens
- Concrete testing chooses inputs manually; symbolic execution derives them from constraints.
- `klee_assert` is how KLEE finds violations of expected properties.
- Symbolic arguments need the POSIX runtime.
- KLEE gives one witness per feasible path, not all satisfying values.
- Boundary values such as minimum signed integers are exactly the kind of cases symbolic execution can uncover.

### Common mistakes
- KLEE is for LLVM bitcode, not raw C source.
- Longer symbolic inputs create a larger search space.
- Path explosion is the main practical limitation.

## PDF and slide consolidation

### Core idea
The reference PDFs and slide decks reinforce the same themes across the weeks: layered API security, SSDF lifecycle practices, SAFECode secure development guidance, key management discipline, CNSA 2.0 transition planning, ABAC policy design, and historical assurance models.

### Key terms
- Defense in depth: multiple overlapping controls.
- ABAC: authorization based on attributes and policy.
- RBAC: authorization based on roles.
- Audit logging: record security-relevant actions.
- Rate limiting: reduce abuse and denial-of-service risk.
- Key lifecycle: generation, storage, use, rotation, revocation, archival, destruction.

### Exam lens
- Authentication proves identity; authorization decides access.
- Strong crypto fails if key management is weak.
- ABAC is attribute-driven and policy-centric; RBAC is role-driven.
- Security is strongest when controls are layered across the whole lifecycle.

### Common mistakes
- A standard and a guide are not the same thing.
- Good cryptography does not compensate for weak operational control.
- Policy design matters as much as algorithm choice.
