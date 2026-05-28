import os
import json
import secrets
import uuid
import io
from flask import Flask, request, jsonify, send_file, abort
from cryptography.fernet import Fernet

app = Flask(__name__)

# Configuration
DATA_DIR = 'data'
FILES_DIR = os.path.join(DATA_DIR, 'files')
METADATA_FILE = os.path.join(DATA_DIR, 'metadata.json')
USERS_FILE = os.path.join(DATA_DIR, 'users.json')
KEY_FILE = os.path.join(DATA_DIR, 'secret.key')

# Ensure directories exist
os.makedirs(FILES_DIR, exist_ok=True)

# --- Encryption Helper ---
def load_or_create_key():
    """Carrega a chave existente ou cria uma nova se não existir."""
    if not os.path.exists(KEY_FILE):
        key = Fernet.generate_key()
        with open(KEY_FILE, 'wb') as f:
            f.write(key)
    return open(KEY_FILE, 'rb').read()

# Inicializa o motor de encriptação
cipher = Fernet(load_or_create_key())
# -------------------------

# Helper functions for data persistence
def load_json(filepath):
    if not os.path.exists(filepath):
        return {}
    with open(filepath, 'r') as f:
        try:
            return json.load(f)
        except json.JSONDecodeError:
            return {}

def save_json(filepath, data):
    with open(filepath, 'w') as f:
        json.dump(data, f, indent=4)

def get_user_by_token(token):
    users = load_json(USERS_FILE)
    for username, data in users.items():
        if data.get('api_key') == token:
            return username
    return None

# Middleware for authentication
def authenticate():
    token = request.headers.get('Authorization')
    if not token:
        abort(401, description="Missing Authorization header")
    user = get_user_by_token(token)
    if not user:
        abort(403, description="Invalid API Key")
    return user

@app.route('/register', methods=['POST'])
def register():
    data = request.json
    username = data.get('username')
    if not username:
        return jsonify({'error': 'Username required'}), 400
    
    users = load_json(USERS_FILE)
    if username in users:
        return jsonify({'error': 'User already exists'}), 409
    
    # Generate a simple API key
    api_key = secrets.token_hex(16)
    users[username] = {'api_key': api_key}
    save_json(USERS_FILE, users)
    
    return jsonify({'api_key': api_key}), 201

@app.route('/files', methods=['POST'])
def upload_file():
    user = authenticate()
    
    if 'file' not in request.files:
        return jsonify({'error': 'No file part'}), 400
    
    file = request.files['file']
    if file.filename == '':
        return jsonify({'error': 'No selected file'}), 400
        
    # Generate unique ID and safe storage name
    file_id = str(uuid.uuid4())
    # Security: Don't use the original filename for storage to avoid path traversal
    storage_name = file_id 
    
    # --- ENCRYPTION STEP ---
    # Ler conteúdo original
    original_content = file.read()
    # Encriptar
    encrypted_content = cipher.encrypt(original_content)
    
    # Gravar encriptado no disco
    save_path = os.path.join(FILES_DIR, storage_name)
    with open(save_path, 'wb') as f:
        f.write(encrypted_content)
    # -----------------------
    
    # Update metadata
    metadata = load_json(METADATA_FILE)
    if user not in metadata:
        metadata[user] = []
        
    file_info = {
        'file_id': file_id,
        'filename': file.filename, # Original name
        'size': os.path.getsize(save_path), # Size on disk (encrypted)
        'uploaded_at': "2026-02-17T12:00:00Z" # Simplification for example
    }
    metadata[user].append(file_info)
    save_json(METADATA_FILE, metadata)
    
    return jsonify({'file_id': file_id}), 201

@app.route('/files', methods=['GET'])
def list_files():
    user = authenticate()
    metadata = load_json(METADATA_FILE)
    user_files = metadata.get(user, [])
    return jsonify({'files': user_files})

@app.route('/files/<file_id>', methods=['GET'])
def download_file(file_id):
    user = authenticate()
    metadata = load_json(METADATA_FILE)
    user_files = metadata.get(user, [])
    
    # Check if file exists and belongs to user
    target_file = next((f for f in user_files if f['file_id'] == file_id), None)
    
    if not target_file:
        return jsonify({'error': 'File not found or access denied'}), 404
    
    # --- DECRYPTION STEP ---
    file_path = os.path.join(FILES_DIR, file_id)
    if not os.path.exists(file_path):
        return jsonify({'error': 'File lost on server'}), 404

    try:
        with open(file_path, 'rb') as f:
            encrypted_content = f.read()
        
        decrypted_content = cipher.decrypt(encrypted_content)
        
        # Serve decrypted content from memory
        return send_file(
            io.BytesIO(decrypted_content),
            as_attachment=True,
            download_name=target_file['filename'],
            mimetype='application/octet-stream'
        )
    except Exception as e:
        print(f"Decryption error: {e}")
        return jsonify({'error': 'Internal server error'}), 500
    # -----------------------

@app.route('/files/<file_id>', methods=['DELETE'])
def delete_file(file_id):
    user = authenticate()
    metadata = load_json(METADATA_FILE)
    
    if user not in metadata:
        return jsonify({'error': 'File not found'}), 404

    # Filter out the file to be deleted
    original_count = len(metadata[user])
    metadata[user] = [f for f in metadata[user] if f['file_id'] != file_id]
    
    if len(metadata[user]) == original_count:
         return jsonify({'error': 'File not found'}), 404
         
    save_json(METADATA_FILE, metadata)
    
    # Try to remove from disk
    try:
        os.remove(os.path.join(FILES_DIR, file_id))
    except OSError:
        pass # File might be gone already
        
    return jsonify({'message': 'Deleted'}), 200

if __name__ == '__main__':
    # Nota: Em produção, usar HTTPS e não o servidor de desenvolvimento
    app.run(port=5000, debug=True)