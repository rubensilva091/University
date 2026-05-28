# Week 1 - File Vault API

## Overview

In this practical lab you will implement a **minimal file storage service** exposed through a **REST API**, together with a simple command-line client.

**How to proceed:**
1. read the details below
2. in your repository create a file `pg60000/week1/security.md`
   - take 10 minutes to think about security requirements and write them in this file (list of bullets). For instance, what to encrypt; how to authenticate; what to protect; ...; **Commit and push the file after the 10 minutes**
3. use any tool that you find appropriate (for instance, LLMs) to produce the code (as fast as possible). If you decide to use such tools, the input can be the description of the problem (bellow) and the contents of the file `security.md`.
4. as soon as you have an implementation that seems to work, commit and push it.
5. read the code and document it and write the README.md (commit and push it).
6. now, try to break security, document your strategy and results in a file named `pg60000/week1/break.md`.

You need to deliver (commit and push):
- `security.md`: the file containing the (quickly written security requirements)
- `server.py`: a REST API server
- `client.py`: a command-line client that interacts with the API
- `tests.py` : a file for testing: on a terminal you launch the `server.py` and, on another terminal you run `tests.py` to use the API
- `requirements.txt` : list of dependencies (read bellow)
- `break.md` : some insights on how to break security
- `README.md` : brief descriptions of: 1) how to run the client and the server; how files are stored; ...; (use quick, one-sentence, descriptions: for instance, "this does that."; favor the usage of bullets and numbered lists;

---

## Functional Requirements

### Users
- Users are identified by a `username`.
- You must implement *some* form of authentication.

### Files
- Files are uploaded to the server and associated with a user.
- Each stored file is identified by a server-generated `file_id`.

---

## Required API Endpoints

### 1. Register or Login

`POST /register`
- returns an `api_key` or something else you may find appropriate

The authentication mechanism is intentionally left open. Document your choice.

---

### 2. Upload a File

`POST /files`

- Authentication required
- Request includes:
  - filename
  - file contents (multipart upload or base64)

Response:
```json
{"file_id": "..."}
```

---

### 3. List My Files

`GET /files`

- Authentication required
- Returns metadata for all files belonging to the authenticated user

Example response:
```json
{
  "files": [
    {
      "file_id": "...",
      "filename": "notes.pdf",
      "size": 12345,
      "uploaded_at": "2026-02-01T12:00:00Z"
    }
  ]
}
```

---

### 4. Download a File

`GET /files/<file_id>`

- Authentication required
- Returns the file contents
- Must include a `Content-Disposition` header with the original filename

---

### 5. Delete a file

`DELETE /files/<file_id>`

- delete a stored file

---

## Constraints

- Python only
- Files stored on disk under a `data/` directory
- Metadata stored in JSON files

---

## Environment Setup

### 1. Create a Virtual Environment

Something like:

**Linux / macOS**
```bash
python3 -m venv .venv
source .venv/bin/activate
```

**Windows (PowerShell)**
```powershell
py -m venv .venv
.venv\Scripts\Activate.ps1
```

---

### 2. Install Dependencies

```bash
python -m pip install --upgrade pip
python -m pip install flask requests cryptography
```

Freeze dependencies:
```bash
python -m pip freeze > requirements.txt
```

---

## Running the System

### Start the Server
```bash
python server.py
```

### Use the Client

Examples:
```bash
python client.py register --username alice
python client.py upload --username alice --path notes.pdf
python client.py list --username alice
python client.py download --username alice --file-id <id> --out downloaded.pdf
```
