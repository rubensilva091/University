# SAFECode Quiz - 10 Questions

**Source basis:** SAFECode Fundamental Practices for Secure Software Development (2018).

---



## 1. What kind of document is SAFECode's Fundamental Practices for Secure Software Development?
**Question.** Explain what the document is trying to provide, and also what it explicitly says it is **not**.

**Answer.** 

It is a practical guide to secure software development: a set of fundamental practices and recommendations that organizations can adapt to their own context. It is explicitly not a one-size-fits-all standard, certification, or complete development methodology.

---



## 2. Why does SAFECode insist that security is a lifecycle property rather than a late testing step?
**Question.** Give the main idea in your own words.

**Answer.** 

Because security has to be designed in from the beginning and maintained through development, testing, release, and operation. If you wait until the end, many problems are already baked into the architecture and are much more expensive to fix.

---

## 3. What are application security controls, and why does SAFECode want them managed as structured data?
**Question.** Answer both parts.

**Answer.** 

Application security controls are the concrete safeguards used to reduce risk, such as authentication, authorization, input validation, logging, and secure configuration. SAFECode wants them managed as structured data so they can be tracked, reused, measured, audited, and applied consistently across projects.

---



## 4. Threat modeling is described as one of the best returns on investment in the document. Why?
**Question.** Give two reasons.

**Answer.** 

First, it finds design flaws early, when they are still cheap to correct. Second, it helps teams focus effort on the most relevant threats and controls instead of reacting randomly after implementation.

---



## 5. Explain the connection between default deny, complete mediation, and least privilege.
**Question.** Why do these three ideas fit together?

**Answer.** 

They all reduce the chance and impact of unauthorized access. Default deny blocks everything unless it is explicitly allowed, complete mediation checks access every time, and least privilege limits each user or component to only the permissions it actually needs.

---



## 6. SAFECode says that all user-originated input should be treated as untrusted, but it also says boundary validation is not enough. Why not?

**Question.** If checking input only at the system boundary is not enough -- where boundary means the point where data first enters the system, such as a form, API, or file parser -- what stronger approach does SAFECode recommend instead?

**Answer.** 

Because data can cross multiple trust boundaries, be transformed internally, or be reused later in a different context. SAFECode recommends validating and handling data at every trust boundary and again in the context where it is actually used, instead of assuming one initial check is enough.

---



## 7. The document encourages reuse of third-party components, but also warns against trusting them blindly. Why are these two ideas compatible?
**Question.** This is a short reasoning question.

**Answer.** 

Reuse is good because it saves effort and can improve quality, but third-party software still brings its own risks and vulnerabilities. So the right approach is to use it deliberately, vet it, keep it updated, and monitor it like any other dependency.

---



## 8. Compare SAST and DAST. What does each contribute, and why does SAFECode treat them as complementary?
**Question.** A short comparison is enough.

**Answer.** 

SAST analyzes code or binaries without running the application, so it is good for finding coding and logic issues early. DAST tests the running application from the outside, so it is good for finding runtime, configuration, and integration problems; together they cover different classes of defects.

---



## 9. Why is tracking and acting on security findings an essential part of the SDL, rather than something that can be postponed after vulnerabilities are found?

**Question.** Explain the reasoning.

**Answer.** 

Because finding a problem is not the same as reducing the risk from it. The SDL only works if findings are triaged, fixed, verified, and fed back into the process so the same issues do not keep recurring.

---



## 10. Why is post-release vulnerability response still part of the SDL?
**Question.** Answer in a way that connects response to continuous improvement.

**Answer.** 

Because security work does not end at release: new flaws, dependencies, and attack techniques can appear later. Post-release response lets the organization patch, disclose, learn from incidents, and feed those lessons back into future design and development.

---

