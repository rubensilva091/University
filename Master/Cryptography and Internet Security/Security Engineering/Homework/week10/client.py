from __future__ import annotations

import argparse
import json
import sys
import urllib.error
import urllib.parse
import urllib.request
from typing import Any, Dict, Optional

TOKEN_ALIASES = [
    "student-alice",
    "student-bob",
    "teacher-token",
    "admin-token",
    "stale-admin-token",
    "bad-grader-token",
    "internal-grader-token",
]


def request_json(method: str, url: str, token: Optional[str] = None, body: Optional[Dict[str, Any]] = None) -> None:
    headers = {"Accept": "application/json"}
    data = None
    if token:
        headers["Authorization"] = f"Bearer {token}"
    if body is not None:
        headers["Content-Type"] = "application/json"
        data = json.dumps(body).encode("utf-8")

    req = urllib.request.Request(url, data=data, headers=headers, method=method)
    try:
        with urllib.request.urlopen(req) as resp:
            payload = resp.read().decode("utf-8")
            print(f"HTTP {resp.status}")
            print(payload)
    except urllib.error.HTTPError as err:
        payload = err.read().decode("utf-8")
        print(f"HTTP {err.code}")
        print(payload)
    except urllib.error.URLError as err:
        print(f"Request failed: {err}")
        sys.exit(1)


def build_url(base: str, path: str, query: Optional[Dict[str, str]] = None) -> str:
    url = base.rstrip("/") + path
    if query:
        url += "?" + urllib.parse.urlencode(query)
    return url


def main() -> None:
    parser = argparse.ArgumentParser(description="Small client for the ProjectHub teaching API")
    parser.add_argument("command", choices=[
        "health",
        "me",
        "get-project",
        "search-projects",
        "sqli-demo",
        "get-submission",
        "change-role",
        "grades-import",
    ])
    parser.add_argument("--base-url", default="http://127.0.0.1:5000", help="Base URL of the API")
    parser.add_argument("--token", default="student-alice", choices=TOKEN_ALIASES, help="Pretend bearer token alias")
    parser.add_argument("--project-id", default="p1")
    parser.add_argument("--submission-id", default="s1")
    parser.add_argument("--name", default="Compiler")
    parser.add_argument("--user-id", default="user-200")
    parser.add_argument("--role", default="admin")
    args = parser.parse_args()

    if args.command == "health":
        request_json("GET", build_url(args.base_url, "/health"))
    elif args.command == "me":
        request_json("GET", build_url(args.base_url, "/api/me"), token=args.token)
    elif args.command == "get-project":
        request_json("GET", build_url(args.base_url, f"/api/projects/{args.project_id}"), token=args.token)
    elif args.command == "search-projects":
        request_json("GET", build_url(args.base_url, "/api/projects/search", {"name": args.name}), token=args.token)
    elif args.command == "sqli-demo":
        injection = "x' UNION SELECT user_id, role, user_id FROM users -- "
        request_json("GET", build_url(args.base_url, "/api/projects/search", {"name": injection}), token=args.token)
    elif args.command == "get-submission":
        request_json("GET", build_url(args.base_url, f"/api/submissions/{args.submission_id}"), token=args.token)
    elif args.command == "change-role":
        request_json(
            "POST",
            build_url(args.base_url, f"/api/admin/users/{args.user_id}/role"),
            token=args.token,
            body={"role": args.role},
        )
    elif args.command == "grades-import":
        request_json("POST", build_url(args.base_url, "/internal/grades/import"), token=args.token, body={})


if __name__ == "__main__":
    main()
