# Secure APIs

**Reference:** API Security in Action (Neil Madden, 2020)  

---

## Simplified scenario: ProjectHub API

A team built **ProjectHub**, a platform for student projects.

Users can:
- sign in with OpenID Connect,
- view their own profile,
- view projects,
- view submissions,
- administrators can change user roles,
- an internal grading service can import grades.

### Endpoints used in this worksheet

- `GET /api/me`
- `GET /api/projects/{projectId}`
- `GET /api/projects/search?name=...`
- `GET /api/submissions/{submissionId}`
- `POST /api/admin/users/{userId}/role`
- `POST /internal/grades/import`

### Example token claims

```json
{
  "sub": "user-184",
  "scope": "projects.read submissions.read",
  "role": "student",
  "aud": "projecthub-api"
}
```

### Example scope set

- `profile.read`
- `projects.read`
- `submissions.read`
- `admin`
- `grades.import`

### Next Step 

Follow the `TESTING_GUIDE.md`. Inspect the files `client.py` and `server.py`.

---

## Questions

### 1. Why OAuth is not the end of API security

**Question.** The team says: *"We implemented OAuth and OpenID Connect, so our API is secure."* Explain why that conclusion is too strong. Intuition: OAuth/OIDC can help the API determine who is calling, but that is not yet the same as deciding whether the request should be allowed.

**Answer:**


OAuth/OIDC help identify and authenticate the caller, but they do not decide by themselves whether the request should be allowed.

You still need controls such as object-level authorization, audience validation, and injection protection. So OAuth/OIDC are a foundation, not full security.

---

### 2. Scopes versus permissions

**Question.** Explain the difference between **scopes** and **permissions**. Then explain why a token containing `projects.read` is not, by itself, enough to decide which concrete project records may be returned. Intuition: Scopes are broad labels carried by the token, like "this client may try to read projects."; Permissions are the actual decisions the system enforces, like "this user may read project 17, but not project 23."

**Answer:**


Scopes are broad claims in the token, such as `projects.read`.

Permissions are concrete decisions about a specific resource, such as allowing `p1` but not `p2`.

So `projects.read` alone is not enough: the backend must still check access to the requested project.

---

### 3. Valid token, wrong object
**Question.** Assume `GET /api/projects/{projectId}` checks only that the access token is valid and contains `projects.read`.

Explain the security flaw. Then describe what extra backend checks are needed before returning the project.

**Answer:**


The flaw is BOLA/IDOR: a user with `projects.read` can access other users' projects.

Before returning the project, the backend must confirm that the caller is the owner/member, or has a teacher/admin role. If not, it should return `403`.

---

### 4. Submission confidentiality
**Question.** Suppose `GET /api/submissions/{submissionId}` is allowed for any token with `submissions.read`.

Explain why this may be insecure.

**Answer:**


It is insecure because any token with `submissions.read` could read other people's submissions.

The backend must check object-level access: submission author, project member, or an authorized role such as teacher/admin.

---

### 5. Trusting role claims too much
**Question.** The endpoint `POST /api/admin/users/{userId}/role` accepts requests whenever the token contains `role=admin`. Why is that design risky?

**Answer:**


It is risky because the `role=admin` claim may be stale or incorrect.

If the API trusts only that claim, it may allow privilege escalation. The backend should validate the current role and require the `admin` scope.

---

### 6. Audience and token replay
**Question.** The internal grading service accepts a token with `grades.import` even when the token audience is only `projecthub-api`. Explain the problem.

**Answer:**


The problem is token replay across audiences: a token for `projecthub-api` should not work on an internal endpoint.

Without validating `aud`, the wrong client can call `/internal/grades/import`. The API should require `aud=internal-grades`.

---

### 7. API gateway and policy agents

**Question.** Describe API gateways and policy agents. Have a look into section 8.3.3 Policy agents and API gateways, page 289, and [https://www.openpolicyagent.org/](https://www.openpolicyagent.org/)

**Answer:**



An API gateway is the central entry point for the API, handling auth, logging, rate limiting, and routing.

A policy agent, such as OPA, evaluates authorization rules outside the application code and returns allow/deny decisions.

Together, they improve policy consistency and maintainability, but the application is still responsible for business validation.

---


### 8. SQL injection in the search endpoint
**Question.** The endpoint `GET /api/projects/search?name=...` builds its SQL query by concatenating the `name` parameter directly into the query string.

Explain how that can lead to a SQL injection vulnerability. In your answer, mention:
- what the attacker gains by controlling part of the SQL text,
- how a `UNION`-style attack could leak rows from another table,
- and how `server_fixed.py` prevents the attack.

**Answer:**



By concatenating `name` into SQL, the attacker controls part of the query and can change its logic.

With `UNION`, they can combine rows from another table, such as `users`, and leak data.

`server_fixed.py` prevents this with a parameterized query (`LIKE ?`), treating input as data instead of SQL.

---

### 9. Final synthesis
**Question.** Summarize the main lesson of this worksheet.

**Answer:**


Main lesson: a valid token does not mean authorized access.

Real security comes from combining scopes, object-level authorization, audience validation, and safe input handling such as parameterized queries.

---

