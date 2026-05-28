# Secure File Storage - Project proposal

## Overview
Build a secure file storage system with a REST API and a client application. The goal is to implement a realistic system while applying practical software security engineering techniques. You can reuse existing material from the week assignments. You may use LLMs (but be careful: sometimes they inject subtle bugs into the code that are hard to debug or introduce security flaws; don't be too eager with your queries). Focus most of your effort on software security. The final result should be a working system that can be deployed with Docker and includes a documented threat model. Each group can have up to 4 students.

---
## System Description

The system provides a cloud‑style file storage service. Details next.

---
### Server: Core API Features
The REST API supports:
- Upload files
- Download files
- Delete files
- List files
- File versioning
- Share links
- Read and set permissions

Share links may be:
- authenticated (requires login)
- anonymous (public link with secure token)

---
## Clients
Implement a terminal-based client to interact with the server (baseline solution).

Feel free to implement something more complex if you wish to go the extra mile:
- web app
- mobile app

---
## Authentication
You may use OpenID Connect: [check this link for Google Documentation](https://developers.google.com/identity/openid-connect/openid-connect).

---
## Storage Architecture
Files may be stored locally on the server (be careful about OS command injection); Feel free to use an object storage service such as Amazon S3 or any other alternative that you find suitable.

Metadata is stored in a relational database (to *force* you to think about SQL injection) such as:
- MariaDB
- PostgreSQL
- SQLite

Metadata includes:
- file id
- owner
- list of other persons with access (in case of shared file)
- file versions
- permissions
- timestamps
- other data that you may find interesting

---
## Full Privacy Mode
The system should support a full privacy mode.

In this mode:

- Files are encrypted client-side before upload. Notes next. How will you manage the secret keys? Use the same secret key to encrypt all files? Maybe it's not a good idea. Think very carefully about this feature. Also, what happens if the client encrypts the same file twice? Integrity?

- A kind reminder, the previous point means the server never sees plaintext data. Take privacy very seriously. 

- Should the server be allowed to even see the original filename? Probably not, because it can also reveal sensitive information. Maybe just a hash is not enough either

- Should the server learn the exact file size (or close to it)? Maybe split into chunks? Let's say the client splits into 5 MB chunks. If you push such chunks at the same time, the server learns that those are related. Maybe the client keeps a local queue and mixes things up a bit (sends them out of order, spaced with random intervals of time)? Maybe the client sends a bit of garbage once in a while? Ah! What about the last chunk? Should we pad it? In this chunk scenario, how does the client recover the file then? Should the client hold a local mini-database to manage this? Also, before I forget, be very careful with AES-GCM and nonce reuse (check why...).

- Which crypto library will you pick? The minimum is a frequently updated one for sure.

---
## Admin Portal

The system includes an admin interface for support staff (terminal-based for the purpose of this course).

Support staff may:
- view user accounts
- inspect files

Impersonation must be secured:
- only allowed for privileged roles
- requires explicit justification (logged)
- limited duration
- logged in audit logs

---
## Observability

The system should implement centralized logging and tracing.
Logs must enforce redaction rules to prevent leaking:
- secrets
- tokens
- personally identifiable information (PII)

---
## Multi‑Tenancy
The system is multi‑tenant. This means:
- multiple users use the same deployed system
- each user is a tenant

Requirements:
- strict logical separation between tenants
- no cross‑tenant access

---
## Security Engineering Tasks
Students must follow the Secure Development Lifecycle (SDLC).
The report should tell something about it.

### Threat Modeling
Create a threat model including
- architecture diagram
- data flow diagram
- STRIDE threat analysis

### Security Requirements
Derive security requirements from the threat model.

Examples:
- tenant isolation
- strong share link tokens
- safe file handling
- secure logging
- encrypt at rest
- ...

### Security Testing
Implement security tests including:
- permission tests
- tenant isolation tests
- API misuse tests
- ...

Examples:
- accessing another tenant's file
- trying to guess share links
- trying to bypassing permissions
- ... (anything that comes to mind)

### CI Security Checks
The repository should include automated checks :
- dependency vulnerability scanning
- secret detection
- static analysis
- ...

---
## Repository Structure and good practices

Proposed structure (you may deploying it differently):

- `server/` - API implementation

- `cli/` - terminal client

- `deploy/` - docker stuff

- `docs/ - extended information on: threat model; architecture; security report; ...

The repository should also contain a neat README.md, LICENCE, release, CI/CD, SECURITY, ...

Sign commits.

---
## Deployment

The system should be deployable with docker.

Running a simple command (or couple of commands) should start:
- API server / database / ...

The deployment must be secure by default.

Examples:
- **no hardcoded credentials**
- secrets provided via environment variables
- non‑root containers

---
## Delieverable

1. Report: In the form of well-formatted documentation in the README.md of the corresponding repository.
Notes: It should start with installation and usage instructions; it should explain the decisions made, the security guarantees it provides, and the process followed. It may/should include images (place the images in the `doc/images` folder of the repository), a description of the tests/CI that provide a high degree of confidence in the developed solution, and, to conclude, anything else you find relevant.

2. Source code: as described above.

3. The presentation slides should be sent to the teacher shortly before the actual presentation (do not push them to the repository).

---
## A final note

Take into consideration that having a nice portfolio makes the job-hunting task slightly easier. Nowadays, it is common practice for hiring teams (not the HR people—the engineers) to have a look at a candidate’s GitHub account to check what they did and how they did it. This project can be part of your portfolio if you wish to do so. Nice and descriptive commit messages also mean a lot. Use clear and positive language everywhere, from PR, commit messages, issues, etc. (being very positive can help, especially when interacting with US-based people; neutral tone is just fine in Europe).



## Ajuda do professor
Como estou a fazer o trabalho sozinho o professor permite me fazer sobre as seguintes novas condiçoes:

1-> Optional: Admin portal 
2-> Optional: Local storage (com aws)
    (i.e. posso fazer com sqlite)

3-> Optional: relax this part: "Should the server learn the exact file size (or close to it)? Maybe split into chunks? Let's say the client splits into 5 MB chunks. If you push such chunks at the same time, the server learns that those are related. Maybe the client keeps a local queue and mixes things up a bit (sends them out of order, spaced with random intervals of time)? Maybe the client sends a bit of garbage once in a while? Ah! What about the last chunk? Should we pad it? In this chunk scenario, how does the client recover the file then? Should the client hold a local mini-database to manage this? Also, before I forget, be very careful with AES-GCM and nonce reuse (check why...)."
