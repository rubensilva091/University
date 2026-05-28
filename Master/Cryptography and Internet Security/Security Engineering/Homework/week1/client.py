import argparse
import requests
import sys
import os

BASE_URL = "http://127.0.0.1:5000"
SESSION_FILE = "client_session.txt"

def save_token(token):
    with open(SESSION_FILE, 'w') as f:
        f.write(token)

def load_token():
    if not os.path.exists(SESSION_FILE):
        return None
    with open(SESSION_FILE, 'r') as f:
        return f.read().strip()

def register(args):
    res = requests.post(f"{BASE_URL}/register", json={"username": args.username})
    if res.status_code == 201:
        token = res.json()['api_key']
        print(f"Registered successfully. API Key: {token}")
        # Auto-save token for convenience in this lab
        save_token(token) 
        print(f"Token saved to {SESSION_FILE} for future requests.")
    else:
        print(f"Error: {res.text}")

def upload(args):
    token = load_token()
    if not token:
        print("Error: No token found. Register first.")
        return

    try:
        with open(args.path, 'rb') as f:
            files = {'file': (os.path.basename(args.path), f)}
            headers = {'Authorization': token}
            res = requests.post(f"{BASE_URL}/files", files=files, headers=headers)
            print(res.text)
    except FileNotFoundError:
        print("File not found.")

def list_files(args):
    token = load_token()
    if not token:
        print("Error: No token found. Register first.")
        return
        
    headers = {'Authorization': token}
    res = requests.get(f"{BASE_URL}/files", headers=headers)
    print(json.dumps(res.json(), indent=2))

def download(args):
    token = load_token()
    if not token:
        print("Error: No token found.")
        return

    headers = {'Authorization': token}
    res = requests.get(f"{BASE_URL}/files/{args.file_id}", headers=headers)
    
    if res.status_code == 200:
        with open(args.out, 'wb') as f:
            f.write(res.content)
        print(f"File saved to {args.out}")
    else:
        print(f"Error: {res.text}")

def delete(args):
    token = load_token()
    headers = {'Authorization': token}
    res = requests.delete(f"{BASE_URL}/files/{args.file_id}", headers=headers)
    print(res.text)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="File Vault Client")
    subparsers = parser.add_subparsers()

    # Register
    p_reg = subparsers.add_parser('register')
    p_reg.add_argument('--username', required=True)
    p_reg.set_defaults(func=register)

    # Upload
    p_up = subparsers.add_parser('upload')
    p_up.add_argument('--path', required=True)
    p_up.set_defaults(func=upload)

    # List
    p_list = subparsers.add_parser('list')
    p_list.set_defaults(func=list_files)

    # Download
    p_down = subparsers.add_parser('download')
    p_down.add_argument('--file-id', required=True)
    p_down.add_argument('--out', required=True)
    p_down.set_defaults(func=download)

    # Delete
    p_del = subparsers.add_parser('delete')
    p_del.add_argument('--file-id', required=True)
    p_del.set_defaults(func=delete)

    import json
    args = parser.parse_args()
    if hasattr(args, 'func'):
        args.func(args)
    else:
        parser.print_help()