import os
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
from cryptography.exceptions import InvalidTag

_ENVELOPE_MAGIC = b"SFS1"
_ENVELOPE_HEADER_LEN = 12  # magic(4) + original_len(8)
_PAD_BLOCK_SIZE = int(os.getenv("SIZE_OBFUSCATION_BLOCK_BYTES", 1024 * 1024))


def _build_padded_envelope(file_data: bytes) -> bytes:
    # Defense note: plaintext is wrapped with original size before random padding.
    if _PAD_BLOCK_SIZE < 64:
        raise ValueError("SIZE_OBFUSCATION_BLOCK_BYTES demasiado pequeno; use >= 64 bytes.")

    header = _ENVELOPE_MAGIC + len(file_data).to_bytes(8, "big")
    payload = header + file_data
    remainder = len(payload) % _PAD_BLOCK_SIZE
    if remainder:
        payload += os.urandom(_PAD_BLOCK_SIZE - remainder)
    return payload


def _extract_original_plaintext(decrypted_data: bytes) -> bytes:
    if len(decrypted_data) < _ENVELOPE_HEADER_LEN:
        return decrypted_data

    if decrypted_data[:4] != _ENVELOPE_MAGIC:
        # Backward compatibility for old encrypted files.
        return decrypted_data

    original_size = int.from_bytes(decrypted_data[4:12], "big")
    start = _ENVELOPE_HEADER_LEN
    end = start + original_size
    if end > len(decrypted_data):
        raise ValueError("Envelope encriptado inválido ou corrompido.")
    return decrypted_data[start:end]

def generate_key() -> bytes:
    return AESGCM.generate_key(bit_length=256)

def encrypt_file(key: bytes, file_data: bytes) -> bytes:
    aesgcm = AESGCM(key)

    # Defense note: each encryption uses a fresh random nonce to avoid AES-GCM nonce reuse.
    nonce = os.urandom(12)

    # Defense note: fixed-size padding reduces file-size leakage to the server/observer.
    protected_payload = _build_padded_envelope(file_data)
    encrypted_data = aesgcm.encrypt(nonce, protected_payload, associated_data=None)

    # Prefix nonce so decrypt can split payload deterministically.
    return nonce + encrypted_data

def decrypt_file(key: bytes, encrypted_payload: bytes) -> bytes:
    # Minimum payload: 12-byte nonce + 16-byte GCM authentication tag.
    if len(encrypted_payload) < 28:
        raise ValueError("O ficheiro encriptado está corrompido ou tem um tamanho inválido.")

    nonce = encrypted_payload[:12]
    encrypted_data = encrypted_payload[12:]
    
    aesgcm = AESGCM(key)
    
    try:
        decrypted_data = aesgcm.decrypt(nonce, encrypted_data, associated_data=None)
        return _extract_original_plaintext(decrypted_data)
    except InvalidTag:
        raise ValueError("ERRO CRÍTICO: A chave está incorreta ou o ficheiro foi adulterado (Integridade Comprometida)!")