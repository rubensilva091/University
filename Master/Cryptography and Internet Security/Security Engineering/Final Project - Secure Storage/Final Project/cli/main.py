import os
import json
import getpass
import typer
import requests
from cryptography.hazmat.primitives.kdf.scrypt import Scrypt
from cryptography.hazmat.primitives.ciphers.aead import AESGCM as _AESGCM
from crypto import generate_key, encrypt_file, decrypt_file

app = typer.Typer(help="Secure File Storage - CLI Client (Full Privacy Mode)")

API_URL = os.getenv("API_URL", "http://127.0.0.1:8000")
KEYS_FILE = "local_keys.json"

def get_headers():
    """Recupera o Token OIDC da variável de ambiente."""
    token = os.getenv("API_TOKEN")
    if not token:
        typer.secho("ERRO: Variável de ambiente API_TOKEN não definida.", fg=typer.colors.RED)
        raise typer.Exit(code=1)
    return {"Authorization": f"Bearer {token}"}

def _derive_vault_key(passphrase: str, salt: bytes) -> bytes:
    # Defense note: Scrypt slows brute-force attempts against stolen vault files.
    kdf = Scrypt(salt=salt, length=32, n=2**14, r=8, p=1)
    return kdf.derive(passphrase.encode())


def _prompt_passphrase(confirm: bool = False) -> str:
    pp = getpass.getpass("Passphrase do cofre de chaves: ")
    if confirm:
        pp2 = getpass.getpass("Confirme a passphrase: ")
        if pp != pp2:
            typer.secho("As passphrases não coincidem.", fg=typer.colors.RED)
            raise typer.Exit(code=1)
    return pp


def load_keys(passphrase: str = None) -> dict:
    """Carrega e desencripta o cofre local de chaves (DEK) e metadados."""
    if not os.path.exists(KEYS_FILE):
        return {}

    with open(KEYS_FILE, "rb") as f:
        data = f.read()

    if len(data) == 0:
        return {}

    # Defense note: keep compatibility so old users can migrate without data loss.
    try:
        legacy = json.loads(data)
        typer.secho(
            "AVISO: Cofre em texto simples detetado. Execute 'migrate-vault' para encriptar.",
            fg=typer.colors.YELLOW,
        )
        return legacy
    except (json.JSONDecodeError, UnicodeDecodeError):
        pass

    # Defense note: encrypted vault format is salt + nonce + ciphertext.
    if len(data) < 28:
        typer.secho("Cofre de chaves corrompido.", fg=typer.colors.RED)
        raise typer.Exit(code=1)

    if passphrase is None:
        passphrase = _prompt_passphrase()

    salt = data[:16]
    nonce = data[16:28]
    ciphertext = data[28:]

    vault_key = _derive_vault_key(passphrase, salt)
    aesgcm = _AESGCM(vault_key)

    try:
        plaintext = aesgcm.decrypt(nonce, ciphertext, None)
        return json.loads(plaintext)
    except Exception:
        typer.secho("Passphrase incorreta ou cofre corrompido.", fg=typer.colors.RED)
        raise typer.Exit(code=1)


def _save_keys(keys: dict, passphrase: str):
    """Encripta e persiste o cofre."""
    # Defense note: every save uses new salt+nonce, so vault ciphertext changes each write.
    salt = os.urandom(16)
    nonce = os.urandom(12)
    vault_key = _derive_vault_key(passphrase, salt)
    aesgcm = _AESGCM(vault_key)
    ciphertext = aesgcm.encrypt(nonce, json.dumps(keys).encode(), None)
    with open(KEYS_FILE, "wb") as f:
        f.write(salt + nonce + ciphertext)


def save_key(file_id: str, original_name: str, key_hex: str, passphrase: str = None):
    """Guarda a chave gerada e o nome original no cofre local encriptado."""
    if not os.path.exists(KEYS_FILE):
        typer.secho("Novo cofre de chaves — defina uma passphrase:", fg=typer.colors.CYAN)
        if passphrase is None:
            passphrase = _prompt_passphrase(confirm=True)
        keys = {}
    else:
        if passphrase is None:
            passphrase = _prompt_passphrase()
        keys = load_keys(passphrase)

    keys[file_id] = {"original_name": original_name, "key": key_hex}
    # Defense note: keys are indexed by file_id to support per-file cryptographic erasure.
    _save_keys(keys, passphrase)


def delete_local_key(file_id: str, passphrase: str = None) -> bool:
    """Remove a chave do cofre local (Cryptographic Erasure)."""
    if not os.path.exists(KEYS_FILE):
        return False

    if passphrase is None:
        passphrase = _prompt_passphrase()
    keys = load_keys(passphrase)
    if file_id not in keys:
        return False

    del keys[file_id]
    _save_keys(keys, passphrase)
    return True

@app.command("list")
def list_files():
    """Lista ficheiros e exibe links anónimos ativos."""
    response = requests.get(f"{API_URL}/files/", headers=get_headers())

    if response.status_code == 200:
        data = response.json()
        typer.secho(f"\n--- Os Meus Ficheiros (Tenant: {data['tenant_id']}) ---", fg=typer.colors.CYAN, bold=True)

        # Defense note: local vault restores friendly names; server only tracks anonymized IDs.
        keys = load_keys()
        for f in data.get("owned_files", []):
            fid = f['file_id']
            orig_name = keys.get(fid, {}).get("original_name", "Desconhecido")
            typer.echo(f"  {orig_name} | ID: {fid} | Versões: {f['versions']}")

            for l_id in f.get("anonymous_links", []):
                typer.secho(f"    └─ Link Anónimo Ativo: {API_URL}/anon/{l_id}", fg=typer.colors.YELLOW)

        shared = data.get("shared_with_me", [])
        if shared:
            typer.secho(f"\n--- Partilhados Comigo ---", fg=typer.colors.CYAN, bold=True)
            for f in shared:
                fid = f['file_id']
                orig_name = keys.get(fid, {}).get("original_name", "Falta Importar Chave")
                typer.echo(f"  {orig_name} | ID: {fid} | Dono: {f['owner']}")
    else:
        typer.secho(f"Erro na API: {response.text}", fg=typer.colors.RED)


@app.command("upload")
def upload_file(filepath: str):
    """Encripta e envia um novo ficheiro."""
    if not os.path.exists(filepath):
        typer.secho("Ficheiro não encontrado.", fg=typer.colors.RED)
        raise typer.Exit(code=1)

    # Defense note: new DEK per file minimizes blast radius if one key is exposed.
    key = generate_key()
    with open(filepath, "rb") as f:
        plaintext = f.read()

    ciphertext = encrypt_file(key, plaintext)
    files = {"file": ("encrypted_blob.bin", ciphertext, "application/octet-stream")}

    response = requests.post(f"{API_URL}/files/", headers=get_headers(), files=files)

    if response.status_code == 200:
        data = response.json()
        save_key(data["file_id"], os.path.basename(filepath), key.hex())
        typer.secho(f"Upload concluído. ID: {data['file_id']}", fg=typer.colors.GREEN)
    else:
        typer.secho(f"Erro: {response.text}", fg=typer.colors.RED)


@app.command("download")
def download_file(file_id: str, output: str = None):
    """Download e desencriptação local."""
    passphrase = _prompt_passphrase()
    keys = load_keys(passphrase)
    if file_id not in keys:
        typer.secho("Chave local não encontrada!", fg=typer.colors.RED)
        raise typer.Exit(code=1)

    # Defense note: decryption key never leaves the client; server returns ciphertext only.
    key = bytes.fromhex(keys[file_id]["key"])
    response = requests.get(f"{API_URL}/files/{file_id}/download", headers=get_headers())

    if response.status_code == 200:
        plaintext = decrypt_file(key, response.content)
        out_path = output or f"dec_{keys[file_id]['original_name']}"
        with open(out_path, "wb") as f:
            f.write(plaintext)
        typer.secho(f"Ficheiro guardado em: {out_path}", fg=typer.colors.GREEN)
    else:
        typer.secho(f"Erro: {response.text}", fg=typer.colors.RED)


@app.command("share")
def share_file(file_id: str, target_user: str):
    """Partilha autenticada com outro utilizador."""
    payload = {"target_user_id": target_user}
    response = requests.post(f"{API_URL}/files/{file_id}/share", headers=get_headers(), json=payload)

    if response.status_code == 200:
        passphrase = _prompt_passphrase()
        keys = load_keys(passphrase)
        typer.secho(f"Acesso concedido a {target_user}.", fg=typer.colors.GREEN)
        if file_id in keys:
            # Defense note: key exchange is intentionally out-of-band to avoid exposing DEKs via API.
            typer.secho(
                "Transmita a chave ao destinatário por um canal seguro (ex: Signal, PGP).",
                fg=typer.colors.YELLOW,
            )
            # Key is printed once for manual sharing; prompt user to clear terminal history.
            typer.echo(f"Chave DEK: {keys[file_id]['key']}")
            typer.secho("Limpe o terminal após copiar a chave.", fg=typer.colors.YELLOW)
    else:
        typer.secho(f"Erro: {response.text}", fg=typer.colors.RED)


@app.command("create-anonymous-link")
def create_anon_link(file_id: str):
    """Gera um link de acesso público (sem autenticação). Válido 7 dias."""
    response = requests.post(f"{API_URL}/files/{file_id}/anonymous-link", headers=get_headers())

    if response.status_code == 200:
        data = response.json()
        typer.secho("Link Anónimo gerado com sucesso!", fg=typer.colors.GREEN, bold=True)
        typer.secho(f"URL: {data['url']}", fg=typer.colors.YELLOW)

        passphrase = _prompt_passphrase()
        keys = load_keys(passphrase)
        if file_id in keys:
            typer.secho(
                "Transmita a chave ao destinatário por um canal seguro (ex: Signal, PGP).",
                fg=typer.colors.YELLOW,
            )
            typer.echo(f"Chave DEK: {keys[file_id]['key']}")
            typer.secho("Limpe o terminal após copiar a chave.", fg=typer.colors.YELLOW)
    else:
        typer.secho(f"Erro ao gerar link: {response.text}", fg=typer.colors.RED)


@app.command("import-key")
def import_key(file_id: str, name: str):
    """Importa uma chave DEK recebida de outro utilizador (input escondido)."""
    key_hex = getpass.getpass("Chave DEK recebida (hex, input escondido): ")
    save_key(file_id, name, key_hex)
    typer.secho("Chave importada com sucesso.", fg=typer.colors.GREEN)


@app.command("delete")
def delete_file(file_id: str):
    """Elimina o ficheiro e destroi a chave local."""
    response = requests.delete(f"{API_URL}/files/{file_id}", headers=get_headers())
    if response.status_code == 200:
        try:
            # Defense note: best-effort cryptographic erasure on client after server-side delete.
            removed = delete_local_key(file_id)
            if removed:
                typer.secho("Ficheiro e chave eliminados.", fg=typer.colors.GREEN)
            else:
                typer.secho(
                    "Ficheiro eliminado no servidor; chave local não existia no cofre.",
                    fg=typer.colors.YELLOW,
                )
        except typer.Exit:
            typer.secho(
                "Ficheiro eliminado no servidor, mas falhou o apagamento da chave local.",
                fg=typer.colors.YELLOW,
            )
            typer.secho(
                f"Execute: python cli/main.py erase-key {file_id}",
                fg=typer.colors.YELLOW,
            )
    else:
        typer.secho(f"Erro: {response.text}", fg=typer.colors.RED)


@app.command("erase-key")
def erase_key(file_id: str):
    """Apaga apenas a DEK local (usado para erasure manual/recovery)."""
    # Defense note: dedicated command helps recover if key wipe fails during delete flow.
    try:
        removed = delete_local_key(file_id)
        if removed:
            typer.secho("Chave local eliminada com sucesso.", fg=typer.colors.GREEN)
        else:
            typer.secho("Chave não encontrada no cofre local.", fg=typer.colors.YELLOW)
    except typer.Exit:
        typer.secho("Falha ao eliminar chave local (passphrase/cofre inválido).", fg=typer.colors.RED)
        raise


@app.command("migrate-vault")
def migrate_vault():
    """Migra o cofre de chaves de texto simples para formato encriptado."""
    if not os.path.exists(KEYS_FILE):
        typer.secho("Nenhum cofre encontrado.", fg=typer.colors.YELLOW)
        raise typer.Exit()

    with open(KEYS_FILE, "rb") as f:
        data = f.read()

    try:
        keys = json.loads(data)
    except Exception:
        typer.secho("O cofre já está encriptado ou está corrompido.", fg=typer.colors.YELLOW)
        raise typer.Exit()

    typer.secho("Cofre em texto simples detetado. Defina uma nova passphrase:", fg=typer.colors.CYAN)
    passphrase = _prompt_passphrase(confirm=True)
    _save_keys(keys, passphrase)
    typer.secho("Cofre migrado com sucesso para formato encriptado.", fg=typer.colors.GREEN)


if __name__ == "__main__":
    app()