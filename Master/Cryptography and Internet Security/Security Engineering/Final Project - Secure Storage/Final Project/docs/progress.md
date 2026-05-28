# Project Planning: Secure File Storage

**Deadline:** May 2nd
**Estimated Duration:** 7 Weeks
**Main Focus:** Software Security Engineering (SDLC), Multi-Tenancy, and "Full Privacy Mode".

---

## Base Strategy and Technology Stack
* **Backend:** Python with FastAPI.
* **Database:** SQLite (via SQLAlchemy) for metadata storage.
* **Storage:** Local file system (mapped via UUIDs, without preserving original names or extensions).
* **Client (CLI):** Python (Typer or Click).
* **Cryptography:** `cryptography` (Python) for *client-side* encryption (AES-GCM).
* **Authentication:** OpenID Connect (OIDC).

---

## Development Schedule

### Week 1: Threat Modeling and Requirements (March 10 to 16)
* ✅ Draw the Architecture Diagram.
* ✅ Draw the Data Flow Diagram (DFD)
* ✅ Perform threat analysis using the **STRIDE** methodology.
* ✅ Derive and document security requirements based on the threat model.
* ✅ Initialize the Git repository with the `server/`, `cli/`, `deploy/`, and `docs/` folders.
* ✅ Create the base files: `README.md`, `LICENSE`, and `SECURITY.md`.
* ✅ Configure the Git environment to mandatorily sign *commits*.

### Week 2: Database and Isolation (March 17 to 23)
* ✅ Configure the SQLite database.
* ✅ Develop data models ensuring required fields: `file_id` (UUID), `owner`, `shared_users`, `versions`, `permissions`, `timestamps`.
* ✅ Implement *Multi-Tenancy* logic: ensure strict logical isolation where no *query* accesses data outside the authenticated *tenant*'s scope.

### Week 3: Authentication, API, and Observability (March 24 to 30)
* ✅ Integrate OpenID Connect (OIDC) authentication.
* ✅ Configure *Centralized Logging* with redaction rules (ensure no exposure of *secrets*, *tokens*, or PII).
* ✅ Implement basic *Tracing* by injecting a `Request-ID` in all *logs* and API responses.
* ✅ Develop the permissions management system (endpoints to read and change file permissions).

### Week 4: CRUD Endpoints and Versioning (March 31 to April 6)
* ✅ Implement Upload, Download, Delete, and List files endpoints.
* ✅ Implement the File Versioning system (updates via `PUT /files/{file_id}` generate a new version in the database).
* ✅ Implement the sharing system: Authenticated *Links* and Anonymous *Links* generated with cryptographically secure *tokens*.

### Week 5: Full Privacy Mode and CLI Client (April 7 to 13)
* ✅ Structure the terminal client application (CLI).
* ✅ Implement *client-side* encryption using **AES-GCM**, ensuring secure key management and *Nonce* uniqueness.
* ✅ Ensure anonymization of sensitive metadata (e.g., original name) before sending.
* ✅ Integrate CLI commands with API endpoints.

### Week 6: Security Testing, CI, and Docker (April 14 to 20)
* ✅ Write security tests: validation of *tenant* isolation, permission bypass, and API *misuse* attempts.
* ✅ Configure CI *pipeline*: *Dependency Vulnerability Scanning*, *Secret Detection*, and *Static Analysis*.
* ✅ Build the *deployment* via Docker (`Dockerfile` and `docker-compose.yml`), ensuring *Secure by Default* configurations (*non-root*, *secrets* via `ENV`).

### Week 6.1: AI-Assisted Hardening and Pre-Review (April 20 to 21)
* ✅ Review `dicasGemini.txt` suggestions and map each one to a security requirement in `docs/threat_model.md`.
* ✅ Implement high-impact fixes before final documentation (rate limiting source IP handling, upload size limits, CLI `API_URL` configurability, and orphaned metadata handling).
* ✅ Re-run security tests and validate that no regressions were introduced.
* ✅ Document accepted and rejected AI suggestions with technical justification (security impact, effort, and risk trade-offs) in `docs/ai_hardening_6_1.md`.

### Week 7: Review, Documentation, and Delivery (April 21 to 27)
* ✅ Update `README.md` with installation instructions, architectural decisions, and the adopted SDLC process.
* ✅ Export diagrams to the `docs/images/` folder.
* [ ] Review *commit* *logs* and prepare presentation/demo.

### Week 8: Review, Documentation, and Delivery (Delivery Date)

* [ ] Final PowerPoint Presentation
* ✅ Ensure `README.md` includes installation, architecture, SDLC process, security guarantees, tests, and deployment guidance.
* ✅ Ensure threat model is documented in `docs/threat_model.md`.
* ✅ Keep progress and planning tracked in `docs/progress.md`.

---

## Extra Features (Optional)

### Assignment Extras (Granted Exceptions)
* [ ] **Admin Portal (CLI):** Implement commands restricted to a support *role* to list accounts and inspect files. Include *impersonation* functionality strictly recorded in *audit logs* with mandatory justification.
* [ ] **Hybrid Storage (AWS S3):** Abstract the storage layer using the *Repository* or *Factory* pattern to allow switching between `LocalDisk` and `Amazon S3` via environment variables.
* ✅ **File Size Obfuscation (Traffic Analysis Protection):** Added client-side encrypted payload padding to fixed-size blocks (1MB default) to reduce exact-size leakage.

### Security Quick-Wins (Low effort, High impact)
* ✅ **Cryptographic Erasure:** On delete, the CLI now removes the local DEK and provides a dedicated `erase-key` recovery command when manual key destruction is needed.
* ✅ **File Time-to-Live (TTL):** Added file-level `expires_at` with enforcement in access flows and endpoint `PUT /files/{file_id}/ttl`.
