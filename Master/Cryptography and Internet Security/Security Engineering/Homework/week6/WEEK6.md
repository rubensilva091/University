# Week 6 - Git, GitHub, CI/CD, DevOps, DevSecOps, GitHub Actions Self-Hosted


<!-- ---------------------------------------------------------------------- -->
## Part 1 - Git and GitHub

Git is a tool that keeps track of changes in a project's files (mostly source code). It lets developers save snapshots of their work, go back to earlier versions when needed, and work on different ideas or features at the same time using branches. Because Git is distributed, every developer has a full copy of the project history.

GitHub is a website built around Git that makes collaboration easier. It hosts Git repositories online and adds features like pull requests, issue tracking, and code reviews so teams can discuss changes and manage projects together.

### Suggested resources (after the class)

To avoid having people watching videos without headphones during the session, here are some resources you can explore after the class.

Have a look at the [GitHub for Beginners](https://www.youtube.com/playlist?list=PL0lo9MOBetEFcp4SCWinBdpml9B2U25-f) YouTube playlist. If you are new to Git, the full playlist provides a good introduction. If you are short on time, you can start with:

- [Beginner Git commands you need to know](https://youtu.be/rE2zRhZdjFU)

Two other videos that are particularly relevant for the workflow we discussed are:

- [How to create a pull request](https://youtu.be/nCKdihvneS0)  
- [How to use GitHub issues and projects](https://youtu.be/c67GaAkf1BE)

Finally, keep the **almighty cheat sheet** close at hand -- it is always useful:

- [https://git-scm.com/cheat-sheet](https://git-scm.com/cheat-sheet)

---
## Exercise 1: Secure Access to Git Repositories Using SSH

The goal of this exercise is to **improve the security of your development workflow** when interacting with Git repositories.

GitHub repositories can be accessed using **HTTPS** or **SSH**. In this exercise, you will configure **SSH authentication**, which is a more secure and convenient method for frequent development.

### Tasks

1. **Generate** an SSH key pair on your machine. Prefer a modern algorithm such as Ed25519 (supported by GitHub) and protect the private key with a passphrase.
2. Add your **public** SSH key to your GitHub account.
3. Verify that the repositories used in this course are accessed using **SSH instead of HTTPS**.

To check the connection type used by a repository, run (in the repository folder):

```bash
git remote -v
```

If the repository uses SSH, the URL will look similar to:

```bash
git@github.com:username/repository.git
```

If it uses HTTPS, the URL will look like:

```bash
https://github.com/username/repository.git
```

If necessary, update the repository configuration to use the SSH URL (search how).

### Reference guide

**Use the following guide to complete the exercise (read it carefully):**

[https://docs.github.com/en/authentication/connecting-to-github-with-ssh](https://docs.github.com/en/authentication/connecting-to-github-with-ssh)

### Why use SSH instead of HTTPS?

Interesting post here: [https://www.geeksforgeeks.org/git/how-to-use-https-or-ssh-for-git/](https://www.geeksforgeeks.org/git/how-to-use-https-or-ssh-for-git/)


---
## Exercise 2: Understanding Commit Signing

Commit signing helps strengthen the security of the software development process by providing cryptographic authentication of code changes. A signed commit proves that it was created by the holder of a particular private key and that the commit contents have not been modified since it was signed.

Without signatures, Git relies on easily spoofed metadata such as author names and email addresses (recommendation: investigate more about this later), making it harder to detect impersonation or unauthorized changes.

In the context of modern software [supply chain attacks](https://github.com/resources/articles/what-is-software-supply-chain-security), where attackers may try to inject malicious code or impersonate trusted developers, commit signing helps establish a verifiable link between a change and the developer who produced it, improving trust in the repository history.

### Tasks

1. Follow the instructions for commit signing in the GitHub documentation. The goal is to setup **GPG**:

[https://docs.github.com/en/authentication/managing-commit-signature-verification](https://docs.github.com/en/authentication/managing-commit-signature-verification)

2. As you follow the guide, carefully examine each step. Try to understand what each tool does, what security guarantees it actually provides, and which choices are being made on your behalf (if any).

For example: which signing algorithm are you using, how long should the key remain valid, and how is the private key being protected? Which version of GPG do you have installed (if any)? Quickly check what's happening with GPG releases by checking their [website](www.gnupg.org/).

For the algorithm pick, prefer a modern elliptic-curve signing key such as EdDSA when your tooling supports it well; otherwise, RSA 4096 is a reasonable choice. In either case, it is good practice to protect the private key with a strong passphrase, set a limited expiration date rather than making the key valid forever, and rotate or renew it periodically. For this purpose, something like 1 year is a sensible duration: long enough to avoid constant maintenance, but short enough to reinforce that cryptographic keys should not live forever. You can also note that shorter validity periods reduce the damage if a key is lost or exposed.


3. After completing the setup, configure git so that all future commits are signed by default. **From this point on, every commit from the students on this course must include a valid cryptographic signature**.

4. Investigate the related commands, for example, `git verify-commit` or `git log --show-signature`.

5. Some final notes since we were *talking* about signatures. The PQC transition is happening. For reference, RSA-1024 provides roughly 80 bits of security; RSA-2048 around 112; RSA-3072 around 128; and RSA-4096 a bit more than that.

If you are curious about the transition to post-quantum cryptography, take a look at the [NIST SP 800-131A Rev.3 (initial public draft)](https://csrc.nist.gov/pubs/sp/800/131/a/r3/ipd) (the PDF link is on the right side of the page). Initial public draft means the document is not final yet, but it gives a good indication of where things are heading.

For example, check Table 3 on page 19. Solutions providing less than 112 bits of security are disallowed (assuming the organization wants to align with NIST recommendations--which is generally a good idea anyway). The expected timeline for the transition is around 2030.



<!-- ---------------------------------------------------------------------- -->

## Part 2 - CI/CD, DevOps, DevSecOps

[GitHub Articles](https://github.com/resources/articles) is a useful source of information. Consider the following articles (but it has many more! Have a look!):

1. [What is CI/CD?](https://github.com/resources/articles/ci-cd)
2. [What is DevOps?](https://github.com/resources/articles/what-is-devops)
3. [The fundamentals of continuous integration in DevOps](https://github.com/resources/articles/continuous-integration)
4. [What is DevSecOps?](https://github.com/resources/articles/what-is-devsecops)

### Question 1 - CI/CD

**Question:** What does CI/CD stand for? Briefly explain Continuous Integration, Continuous Delivery, and Continuous Deployment. Additionally, research typical CI pipeline execution times and state how long a CI pipeline should ideally take to run.

**Answer:**

CI/CD stands for Continuous Integration and Continuous Delivery/Deployment.

- Continuous Integration (CI): developers integrate code frequently (often multiple times per day), and each push triggers automated build and tests to detect integration issues early.
- Continuous Delivery: after CI passes, the application is automatically packaged and kept in a release-ready state; deployment to production is still a manual decision.
- Continuous Deployment: every change that passes all automated checks is deployed automatically to production, without a manual approval step.

Typical CI times are often 5-15 minutes for the main merge-gating pipeline. Ideally, keep core CI feedback under about 10 minutes and run heavier checks separately.

---



### Question 2 - CI workflow

**Question:** Describe the typical workflow of Continuous Integration when a developer pushes code.

**Answer:**

When a developer pushes code, the CI server is triggered automatically by a webhook. A typical flow is:

1. Checkout source code from the repository.
2. Install dependencies and prepare the environment.
3. Build/compile the project.
4. Run automated tests (unit/integration).
5. Run quality/security checks (linters, static analysis, dependency scan).
6. Publish artifacts and test reports.
7. Mark the commit/PR as pass or fail, blocking merge when checks fail.

This creates fast feedback and ensures only validated changes move forward.

---



### Question 3 - Benefits of CI
**Question:** Why is Continuous Integration beneficial for software development teams?

**Answer:**

Continuous Integration gives fast feedback, catches defects early, and reduces merge conflicts. It improves code quality through automated checks and increases confidence in releases.

---



### Question 4 - DevOps concept
**Question:** What is DevOps and how does it change the relationship between development and operations teams?

**Answer:**

DevOps is a culture/practice model that joins development and operations through collaboration and automation. It replaces siloed handoffs with shared ownership of build, deploy, operate, and monitor.

---



### Question 5 - DevOps Lifecycle
**Question:** Describe the DevOps Lifecycle.

**Answer:**

The DevOps lifecycle is a continuous loop with connected phases:

1. Plan: define requirements and priorities.
2. Code: implement features and fixes.
3. Build: compile/package software.
4. Test: validate functionality and quality.
5. Release: prepare approved version.
6. Deploy: deliver to target environments.
7. Operate: run and maintain the service.
8. Monitor: collect metrics/logs/alerts and feed insights back into planning.

Automation and feedback between phases are key to continuous improvement.

---



### Question 6 - DevSecOps
**Question:** What is DevSecOps and how does it extend DevOps?

**Answer:**

DevSecOps extends DevOps by integrating security across the whole lifecycle. Security checks are automated in CI/CD, enabling earlier vulnerability detection, lower fix cost, and better compliance.

---



### Question 7 - Security in the pipeline
**Question:** Give a couple of examples of security checks that can be automated.

**Answer:**

Examples of automated security checks in CI/CD pipelines:

1. SAST (Static Application Security Testing) to detect insecure code patterns.
2. Dependency/SCA scanning to identify vulnerable third-party libraries.
3. Secret scanning to detect leaked API keys, tokens, or passwords.
4. Container image scanning to find known CVEs in base images/packages.
5. IaC scanning (Terraform/Kubernetes manifests) to detect insecure cloud configurations.

---



### Question 8 - Risks without DevSecOps

What problems can occur when security checks are performed only at the end of the development process instead of being integrated throughout the CI/CD pipeline?

**Answer:**

If security is checked only at the end, vulnerabilities can reach production and fixes become more expensive. This delays releases, increases rework, and raises breach/compliance risk.

---



### Question 9 - Reflection
**Question:** Explain one advantage and one potential security risk of Continuous Deployment.

**Answer:**

One advantage of Continuous Deployment is speed: validated changes reach users quickly, enabling faster bug fixes and shorter feedback cycles.

One potential security risk is rapid propagation of mistakes: if security controls are weak, a vulnerable change can be deployed to production immediately.

---


<!-- ---------------------------------------------------------------------- -->
## Part 3 – GitHub Actions: Self-Hosted Runner (Group Task)

**NOTE: if you don't have a group fill out the form (link at the top of README.md file of this repository), and send me an email (tiago@di.uminho.pt) ASAP (if you are in the classroom, just tell me).**

In this exercise, you will learn how to run GitHub Actions workflows on your own infrastructure using a self-hosted runner. This will be deployed on your group repository (where you have admin rights).

The following documentation might be useful:

- [https://docs.github.com/en/actions/get-started/quickstart](https://docs.github.com/en/actions/get-started/quickstart)
- [https://docs.github.com/en/actions/how-tos/manage-runners/self-hosted-runners/add-runners](https://docs.github.com/en/actions/how-tos/manage-runners/self-hosted-runners/add-runners)

### Goal

Create a Dockerfile that builds a container image capable of running a containerized self-hosted GitHub Actions runner for your project.

The runner should be able to connect to your repository and execute GitHub Actions workflows.

**Tip:** Click Settings (top bar) -> Actions (left menu) -> Runners -> New self-hosted runner.

There you will find several setup steps. Include these steps in your Dockerfile. An authentication token can be obtained from this menu or via `gh`, for example (but check the manual or, in this case, the corresponding API documentation from GitHub):

```bash
TOKEN=$(gh api --method POST -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" /repos/organization_name/repository_name/actions/runners/registration-token --jq .token)
```

See [https://cli.github.com/](https://cli.github.com/) for more information about `gh`.

In your group repository, create a `main.yml` file under the folder `.github/workflows/` (note: it does not need to be called `main`). This file must specify where the workflow runs, for example:
```yaml
runs-on: [self-hosted, linux, X64]
```

### Summary

1. Create a **Dockerfile** that installs and configures a GitHub Actions self-hosted runner.
2. Build a **Docker image** from this Dockerfile.
3. Create a bash script (or equivalent) that starts the runner.
4. Create a **simple GitHub Actions workflow** that runs on this self-hosted runner and performs a basic test (for example running a small script).
5. Stop the container.

### Deliverable of this task

- Push the Dockerfile to your personal repository (all members of the group should commit/push the same Dockerfile).
