# Testing guide for the ProjectHub API

- `server.py` -- server (vulnerable)
- `server_fixed.py` -- fixed version with the key fixes made visible in code
- `client.py` -- command-line client for testing

## 1. Environment

No external Python packages are required. Any reasonably recent Python 3 should work.

## 2. Run the (vulnerable) server

```bash
python3 server.py
```

```text
http://127.0.0.1:5000
```

## 3. Quick tests

### Health check

```bash
python3 client.py health
```

### Read your own profile

```bash
python3 client.py me --token student-alice
```

### Read project `p1`

```bash
python3 client.py get-project --token student-alice --project-id p1
```

### Read another team's project

```bash
python3 client.py get-project --token student-alice --project-id p2
```

On the vulnerable server, this wrongly succeeds because the API checks only a broad scope and not object-level authorization.

### Read another team's submission

```bash
python3 client.py get-submission --token student-alice --submission-id s2
```

On the vulnerable server, this also wrongly succeeds.

## 4. SQL injection test on the vulnerable server

Run:

```bash
python3 client.py sqli-demo --token student-alice
```

The client sends this search string into the vulnerable endpoint:

```text
x' UNION SELECT user_id, role, user_id FROM users -- 
```

Because `server.py` concatenates that input directly into the SQL query, the response can leak rows from the `users` table.

You can also run the equivalent manual request:

```bash
python3 client.py search-projects --token student-alice --name "x' UNION SELECT user_id, role, user_id FROM users -- "
```

## 5. Role-claim test

Run the following command and observe the output:
```
python3 client.py me --token stale-admin-token

HTTP 200
{"user_id": "user-200", "display_name": "Bob", "role": "student"}
```

Now, try the admin endpoint with the simulated stale token:

```bash
python3 client.py change-role --token stale-admin-token --user-id user-200 --role admin
```

Now run the first command again to observe how the role has changed.
```
{"user_id": "user-200", "display_name": "Bob", "role": "admin"}
```
On the vulnerable server, this succeeds because the endpoint trusts the `role` claim too much.

## 6. Audience test on the internal endpoint

Try the internal grading endpoint with the wrong audience:

```bash
python3 client.py grades-import --token bad-grader-token
```

On the vulnerable server, this succeeds even though the token audience is only `projecthub-api`.

## 7. Compare with the fixed server

Stop the vulnerable server, then run:

```bash
python3 server_fixed.py
```

Now repeat the same tests.

Expected differences:

- cross-project reads are rejected when the caller lacks permission on that object
- cross-submission reads are rejected
- the SQL injection demo no longer leaks data
- the stale admin token is rejected
- the wrong-audience grading token is rejected
- a proper internal token works:

```bash
python3 client.py grades-import --token internal-grader-token
```

## 8. Where to look in the code

```
meld server.py server_fixed.py
```

