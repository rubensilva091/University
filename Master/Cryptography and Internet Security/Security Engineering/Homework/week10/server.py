from __future__ import annotations

import json
import sqlite3
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import Any, Dict, Optional
from urllib.parse import parse_qs, urlparse

BASE_DIR = Path(__file__).resolve().parent
DB_PATH = BASE_DIR / "projecthub_vuln.db"
HOST = "127.0.0.1"
PORT = 5000

TOKENS: Dict[str, Dict[str, Any]] = {
    "student-alice": {
        "sub": "user-184",
        "scope": {"profile.read", "projects.read", "submissions.read"},
        "role": "student",
        "aud": "projecthub-api",
    },
    "student-bob": {
        "sub": "user-200",
        "scope": {"profile.read", "projects.read", "submissions.read"},
        "role": "student",
        "aud": "projecthub-api",
    },
    "teacher-token": {
        "sub": "teacher-1",
        "scope": {"profile.read", "projects.read", "submissions.read"},
        "role": "teacher",
        "aud": "projecthub-api",
    },
    "admin-token": {
        "sub": "admin-1",
        "scope": {"profile.read", "admin"},
        "role": "admin",
        "aud": "projecthub-api",
    },
    "stale-admin-token": {
        "sub": "user-200",
        "scope": {"profile.read"},
        "role": "admin",
        "aud": "projecthub-api",
    },
    "bad-grader-token": {
        "sub": "user-184",
        "scope": {"grades.import"},
        "role": "student",
        "aud": "projecthub-api",
    },
    "internal-grader-token": {
        "sub": "service-grader",
        "scope": {"grades.import"},
        "role": "service",
        "aud": "internal-grades",
    },
}


class APIError(Exception):
    def __init__(self, status: int, message: str) -> None:
        super().__init__(message)
        self.status = status
        self.message = message


def setup_db() -> None:
    if DB_PATH.exists():
        DB_PATH.unlink()

    conn = sqlite3.connect(DB_PATH)
    cur = conn.cursor()
    cur.executescript(
        """
        CREATE TABLE users (
            user_id TEXT PRIMARY KEY,
            display_name TEXT NOT NULL,
            role TEXT NOT NULL
        );

        CREATE TABLE projects (
            id TEXT PRIMARY KEY,
            name TEXT NOT NULL,
            owner_id TEXT NOT NULL
        );

        CREATE TABLE project_members (
            project_id TEXT NOT NULL,
            user_id TEXT NOT NULL,
            PRIMARY KEY (project_id, user_id)
        );

        CREATE TABLE submissions (
            id TEXT PRIMARY KEY,
            project_id TEXT NOT NULL,
            submitted_by TEXT NOT NULL,
            content TEXT NOT NULL,
            grade TEXT
        );
        """
    )
    cur.executemany(
        "INSERT INTO users(user_id, display_name, role) VALUES(?, ?, ?)",
        [
            ("user-184", "Alice", "student"),
            ("user-200", "Bob", "student"),
            ("teacher-1", "Dr. Patel", "teacher"),
            ("admin-1", "Admin", "admin"),
        ],
    )
    cur.executemany(
        "INSERT INTO projects(id, name, owner_id) VALUES(?, ?, ?)",
        [
            ("p1", "Compiler Lab", "user-184"),
            ("p2", "ML Demo", "user-200"),
        ],
    )
    cur.executemany(
        "INSERT INTO project_members(project_id, user_id) VALUES(?, ?)",
        [
            ("p1", "user-184"),
            ("p1", "user-200"),
            ("p2", "user-200"),
        ],
    )
    cur.executemany(
        "INSERT INTO submissions(id, project_id, submitted_by, content, grade) VALUES(?, ?, ?, ?, ?)",
        [
            ("s1", "p1", "user-184", "compiler-report.pdf", "A-"),
            ("s2", "p2", "user-200", "ml-demo-slides.pdf", "B+"),
        ],
    )
    conn.commit()
    conn.close()


def get_conn() -> sqlite3.Connection:
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    return conn


class ProjectHubHandler(BaseHTTPRequestHandler):
    server_version = "ProjectHubTeachingAPI/0.1"

    def do_GET(self) -> None:  # noqa: N802
        self.handle_request("GET")

    def do_POST(self) -> None:  # noqa: N802
        self.handle_request("POST")

    def log_message(self, format: str, *args: Any) -> None:  # noqa: A003
        return

    def handle_request(self, method: str) -> None:
        try:
            self.token = self.token_from_request()
            parsed = urlparse(self.path)
            path = parsed.path
            query = parse_qs(parsed.query)

            if method == "GET" and path == "/health":
                self.send_json(200, {"status": "ok", "database": DB_PATH.name})
                return
            if method == "GET" and path == "/api/me":
                self.handle_get_me()
                return
            if method == "GET" and path == "/api/projects/search":
                self.handle_search_projects(query)
                return
            if method == "GET" and path.startswith("/api/projects/"):
                self.handle_get_project(path)
                return
            if method == "GET" and path.startswith("/api/submissions/"):
                self.handle_get_submission(path)
                return
            if method == "POST" and path.startswith("/api/admin/users/") and path.endswith("/role"):
                self.handle_change_role(path)
                return
            if method == "POST" and path == "/internal/grades/import":
                self.handle_grades_import()
                return

            raise APIError(404, "not found")
        except APIError as exc:
            self.send_json(exc.status, {"error": exc.message})
        except Exception as exc:
            self.send_json(500, {"error": str(exc)})

    def read_json_body(self) -> Dict[str, Any]:
        length = int(self.headers.get("Content-Length", "0"))
        raw = self.rfile.read(length) if length else b"{}"
        return json.loads(raw.decode("utf-8"))

    def send_json(self, status: int, payload: Dict[str, Any]) -> None:
        body = json.dumps(payload).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def token_from_request(self) -> Optional[Dict[str, Any]]:
        auth = self.headers.get("Authorization", "")
        if not auth.startswith("Bearer "):
            return None
        alias = auth[len("Bearer ") :].strip()
        return TOKENS.get(alias)

    def require_token(self) -> Dict[str, Any]:
        if self.token is None:
            raise APIError(401, "missing bearer token")
        return self.token

    def require_scope(self, scope: str) -> Dict[str, Any]:
        token = self.require_token()
        if scope not in token["scope"]:
            raise APIError(403, f"missing scope: {scope}")
        return token

    def handle_get_me(self) -> None:
        token = self.require_scope("profile.read")
        conn = get_conn()
        row = conn.execute(
            "SELECT user_id, display_name, role FROM users WHERE user_id = ?",
            (token["sub"],),
        ).fetchone()
        conn.close()
        if row is None:
            raise APIError(404, "unknown user")
        self.send_json(200, dict(row))

    def handle_get_project(self, path: str) -> None:
        token = self.require_scope("projects.read")
        project_id = path.split("/")[-1]
        conn = get_conn()
        row = conn.execute(
            "SELECT id, name, owner_id FROM projects WHERE id = ?",
            (project_id,),
        ).fetchone()
        conn.close()
        if row is None:
            raise APIError(404, "project not found")

        # Vulnerability: only scope is checked, not object ownership/membership.
        self.send_json(200, {"requested_by": token["sub"], "project": dict(row)})

    def handle_search_projects(self, query: Dict[str, Any]) -> None:
        self.require_scope("projects.read")
        name = query.get("name", [""])[0]

        # VULNERABLE ON PURPOSE: user input is concatenated directly into SQL.
        sql = (
            "SELECT id, name, owner_id FROM projects "
            f"WHERE name LIKE '%{name}%' ORDER BY id"
        )

        conn = get_conn()
        rows = [dict(r) for r in conn.execute(sql).fetchall()]
        conn.close()
        self.send_json(200, {"sql": sql, "results": rows})

    def handle_get_submission(self, path: str) -> None:
        token = self.require_scope("submissions.read")
        submission_id = path.split("/")[-1]
        conn = get_conn()
        row = conn.execute(
            "SELECT id, project_id, submitted_by, content, grade FROM submissions WHERE id = ?",
            (submission_id,),
        ).fetchone()
        conn.close()
        if row is None:
            raise APIError(404, "submission not found")

        # Vulnerability: any token with submissions.read can read any submission.
        self.send_json(200, {"requested_by": token["sub"], "submission": dict(row)})

    def handle_change_role(self, path: str) -> None:
        token = self.require_token()
        if token.get("role") != "admin":
            raise APIError(403, "admin role required")

        parts = path.split("/")
        user_id = parts[-2]
        payload = self.read_json_body()

        conn = get_conn()
        cur = conn.execute("UPDATE users SET role = ? WHERE user_id = ?", (payload["role"], user_id))
        conn.commit()
        row = conn.execute(
            "SELECT user_id, display_name, role FROM users WHERE user_id = ?",
            (user_id,),
        ).fetchone()
        conn.close()
        if cur.rowcount == 0 or row is None:
            raise APIError(404, "user not found")

        # Vulnerability: over-trusts the embedded role claim.
        self.send_json(200, {"changed_by": token["sub"], "user": dict(row)})

    def handle_grades_import(self) -> None:
        token = self.require_scope("grades.import")

        # Vulnerability: no audience check for the internal service endpoint.
        self.send_json(
            200,
            {
                "status": "import started",
                "called_by": token["sub"],
                "accepted_audience": token["aud"],
            },
        )


def main() -> None:
    setup_db()
    server = ThreadingHTTPServer((HOST, PORT), ProjectHubHandler)
    print(f"Serving vulnerable API on http://{HOST}:{PORT}")
    server.serve_forever()


if __name__ == "__main__":
    main()
