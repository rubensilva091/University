import urllib.request
import json
import os
from jose import jwt, JWTError
from fastapi import Depends, HTTPException, status
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials

security = HTTPBearer()

JWKS_URL = "https://www.googleapis.com/oauth2/v3/certs"

EXPECTED_AUDIENCE = os.getenv("GOOGLE_CLIENT_ID")
ALLOW_INSECURE_OIDC_FOR_DEV = os.getenv("ALLOW_INSECURE_OIDC_FOR_DEV", "false").lower() == "true"

if not EXPECTED_AUDIENCE and ALLOW_INSECURE_OIDC_FOR_DEV:
    print(
        "AVISO: GOOGLE_CLIENT_ID não definido e ALLOW_INSECURE_OIDC_FOR_DEV=true. "
        "A validação de audiência JWT foi desativada apenas para desenvolvimento local."
    )

_jwks_cache: dict = {"keys": []}


def _fetch_jwks() -> dict:
    # Defense note: keys come from Google JWKS and are used to verify JWT signatures.
    try:
        with urllib.request.urlopen(JWKS_URL, timeout=5) as response:  # nosec B310
            return json.loads(response.read())
    except Exception as e:
        print(f"Erro ao carregar JWKS: {e}")
        return {"keys": []}


def _get_rsa_key(kid: str, allow_refresh: bool = True) -> dict:
    """Return the JWK matching *kid*, refreshing the cache once if needed."""
    # Defense note: one refresh handles key rotation while avoiding network calls on every request.
    global _jwks_cache
    if not _jwks_cache["keys"]:
        _jwks_cache = _fetch_jwks()

    for key in _jwks_cache.get("keys", []):
        if key["kid"] == kid:
            return {
                "kty": key["kty"],
                "kid": key["kid"],
                "use": key["use"],
                "n":   key["n"],
                "e":   key["e"],
            }

    # Refresh once to handle Google JWKS rotation without extra network calls.
    if allow_refresh:
        _jwks_cache = _fetch_jwks()
        return _get_rsa_key(kid, allow_refresh=False)

    return {}


async def get_current_user(auth: HTTPAuthorizationCredentials = Depends(security)):
    token = auth.credentials

    # Defense note: fail closed in production when audience is not configured.
    if not EXPECTED_AUDIENCE and not ALLOW_INSECURE_OIDC_FOR_DEV:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=(
                "Servidor mal configurado: defina GOOGLE_CLIENT_ID para validar audiência JWT. "
                "Em desenvolvimento local, use ALLOW_INSECURE_OIDC_FOR_DEV=true se necessário."
            ),
        )

    credentials_exception = HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Token inválido ou expirado",
        headers={"WWW-Authenticate": "Bearer"},
    )

    try:
        # Defense note: read header first to select the matching public key by kid.
        unverified_header = jwt.get_unverified_header(token)
        rsa_key = _get_rsa_key(unverified_header.get("kid", ""))

        if not rsa_key:
            raise credentials_exception

        # Defense note: Google ID tokens may omit at_hash depending on flow; skip only this claim.
        decode_options = {"verify_at_hash": False}
        audience = EXPECTED_AUDIENCE
        if not EXPECTED_AUDIENCE and ALLOW_INSECURE_OIDC_FOR_DEV:
            # Explicit local-development-only escape hatch.
            decode_options["verify_aud"] = False
            audience = None

        payload = jwt.decode(
            token,
            rsa_key,
            algorithms=["RS256"],
            audience=audience,
            options=decode_options,
        )

        user_id: str = payload.get("sub")
        if user_id:
            return user_id

    except JWTError as e:
        print(f"ERRO DE VALIDAÇÃO JWT: {e}")
        raise credentials_exception
    except HTTPException:
        raise
    except Exception as e:
        print(f"ERRO GENÉRICO: {e}")
        raise credentials_exception

    raise credentials_exception