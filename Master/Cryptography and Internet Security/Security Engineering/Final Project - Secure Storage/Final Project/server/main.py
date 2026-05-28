import uuid
import os
import threading
from collections import defaultdict
from datetime import datetime, timedelta, timezone
from fastapi import FastAPI, Depends, Request, HTTPException, UploadFile, File
from fastapi.responses import FileResponse
from sqlalchemy.orm import Session
from pydantic import BaseModel

from .database import engine
from .database import SessionLocal 
from . import models
from .auth import get_current_user
from .logger import logger, request_id_context

models.Base.metadata.create_all(bind=engine)

def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()

_rate_store: dict = defaultdict(list)
_rate_lock = threading.Lock()

MAX_REQUESTS = 60
RATE_WINDOW_SECONDS = 60
MAX_UPLOAD_BYTES = int(os.getenv("MAX_UPLOAD_BYTES", 50 * 1024 * 1024))

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
STORAGE_DIR = os.getenv("STORAGE_DIR", os.path.join(BASE_DIR, "data", "storage"))
os.makedirs(STORAGE_DIR, exist_ok=True)

app = FastAPI(
    title="Secure File Storage API",
    description="Backend for the Secure Multi-Tenant File Storage System",
    version="1.0.0"
)


def _get_client_ip(request: Request) -> str:
    """Prefer proxy headers when available, falling back to socket peer IP."""
    # Defense note: behind reverse proxies, request.client.host is often the proxy IP.
    forwarded_for = request.headers.get("x-forwarded-for")
    if forwarded_for:
        first_ip = forwarded_for.split(",")[0].strip()
        if first_ip:
            return first_ip

    real_ip = request.headers.get("x-real-ip")
    if real_ip:
        return real_ip.strip()

    return request.client.host if request.client else "unknown"


def _save_upload_stream_limited(upload: UploadFile, destination: str):
    """Persist upload stream and abort with 413 when exceeding configured max size."""
    # Defense note: stream in chunks to avoid unbounded RAM usage on large uploads.
    total = 0
    chunk_size = 1024 * 1024

    with open(destination, "wb") as buffer:
        while True:
            chunk = upload.file.read(chunk_size)
            if not chunk:
                break
            total += len(chunk)
            if total > MAX_UPLOAD_BYTES:
                # Defense note: 413 signals payload too large and prevents disk exhaustion DoS.
                raise HTTPException(
                    status_code=413,
                    detail=f"Ficheiro excede o limite máximo de {MAX_UPLOAD_BYTES} bytes.",
                )
            buffer.write(chunk)


def _cleanup_orphaned_metadata(db: Session, file_record: models.FileMetadata):
    """Remove stale DB entries that point to a missing object in storage."""
    # Defense note: keeps DB/storage consistency when files are deleted out-of-band.
    file_id = file_record.file_id
    db.delete(file_record)
    db.commit()
    logger.warning(f"Metadados órfãos removidos para file_id={file_id}.")

@app.middleware("http")
async def add_request_id_and_log(request: Request, call_next):
    req_id = str(uuid.uuid4())
    request_id_context.set(req_id)

    # Defense note: sliding-window throttling balances abuse control and user experience.
    client_ip = _get_client_ip(request)
    now = datetime.now(timezone.utc)
    window_start = now - timedelta(seconds=RATE_WINDOW_SECONDS)
    with _rate_lock:
        _rate_store[client_ip] = [t for t in _rate_store[client_ip] if t > window_start]
        if len(_rate_store[client_ip]) >= MAX_REQUESTS:
            logger.warning(f"Rate limit exceeded for IP: {client_ip}")
            from fastapi.responses import JSONResponse
            return JSONResponse(status_code=429, content={"detail": "Demasiados pedidos. Tente novamente mais tarde."})
        _rate_store[client_ip].append(now)

    logger.info(f"Incoming request: {request.method} {request.url}")

    response = await call_next(request)

    response.headers["X-Request-ID"] = req_id
    logger.info(f"Completed request: {request.method} {request.url} - Status: {response.status_code}")

    return response

class PermissionUpdate(BaseModel):
    permissions: str

class ShareRequest(BaseModel):
    target_user_id: str

class FileTTLUpdate(BaseModel):
    expires_in_days: int

class AnonymousLinkResponse(BaseModel):
    link_id: str
    file_id: str
    url: str


def _normalize_utc(dt: datetime | None) -> datetime | None:
    if dt is None:
        return None
    if dt.tzinfo is None:
        return dt.replace(tzinfo=timezone.utc)
    return dt


def _ensure_file_not_expired(file_record: models.FileMetadata):
    # Defense note: normalize UTC first to avoid naive/aware datetime comparison bugs.
    expiry = _normalize_utc(file_record.expires_at)
    if expiry and datetime.now(timezone.utc) > expiry:
        raise HTTPException(status_code=410, detail="Ficheiro expirado e indisponível.")


def _get_file_for_owner_or_shared(db: Session, file_id: str, current_user: str):
    """Return file when requester is owner or explicitly shared user."""
    # Defense note: owner lookup first, then explicit share relation; no cross-tenant fallback path.
    file_record = db.query(models.FileMetadata).filter(
        models.FileMetadata.file_id == file_id,
        models.FileMetadata.owner == current_user,
    ).first()
    if file_record:
        return file_record

    return db.query(models.FileMetadata).join(models.file_shares_table).filter(
        models.FileMetadata.file_id == file_id,
        models.file_shares_table.c.shared_user_id == current_user,
    ).first()

@app.get("/")
def health_check():
    return {
        "status": "success",
        "message": "Secure File Storage API is running!",
        "database": "Connected and tables verified."
    }

@app.get("/users/me")
def read_user_me(user_id: str = Depends(get_current_user)):
    return {
        "message": "Autenticação confirmada!",
        "tenant_id": user_id
    }

@app.post("/files/")
async def upload_file(
    file: UploadFile = File(...), 
    db: Session = Depends(get_db), 
    current_user: str = Depends(get_current_user)
):
    new_file_record = models.FileMetadata(owner=current_user)
    db.add(new_file_record)
    db.commit()
    db.refresh(new_file_record) 
    
    file_path = os.path.join(STORAGE_DIR, new_file_record.file_id)
    
    try:
        _save_upload_stream_limited(file, file_path)
    except HTTPException:
        # Defense note: rollback metadata when storage write fails to avoid dangling records.
        db.delete(new_file_record)
        db.commit()
        raise
    except Exception as e:
        db.delete(new_file_record)
        db.commit()
        logger.error(f"Erro ao guardar o ficheiro {new_file_record.file_id} no disco: {str(e)}")
        raise HTTPException(status_code=500, detail="Erro interno ao guardar o ficheiro.")
        
    logger.info(f"Ficheiro guardado com sucesso: {new_file_record.file_id} (Owner: {current_user}).")
    
    return {
        "message": "Ficheiro recebido e guardado com segurança.",
        "file_id": new_file_record.file_id,
        "created_at": new_file_record.created_at,
        "versions": new_file_record.versions
    }

@app.put("/files/{file_id}")
async def update_file_version(
    file_id: str,
    file: UploadFile = File(...), 
    db: Session = Depends(get_db), 
    current_user: str = Depends(get_current_user)
):
    file_record = _get_file_for_owner_or_shared(db, file_id, current_user)
    
    if not file_record:
        raise HTTPException(status_code=404, detail="Ficheiro não encontrado.")

    _ensure_file_not_expired(file_record)
        
    # Defense note: shared users can update only when owner set read/write permission.
    has_write_access = False
    if file_record.owner == current_user:
        has_write_access = True
    else:
        for shared_user in file_record.shared_with:
            if shared_user.user_id == current_user:
                if file_record.permissions == "read/write":
                    has_write_access = True
                break
                
    if not has_write_access:
        logger.warning(f"Tentativa de atualização sem permissão: {file_id} - User: {current_user}")
        raise HTTPException(status_code=403, detail="Não tem permissões de escrita para atualizar este ficheiro.")
        
    file_path = os.path.join(STORAGE_DIR, file_record.file_id)
    
    try:
        _save_upload_stream_limited(file, file_path)
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Erro ao sobrescrever o ficheiro {file_id} no disco: {str(e)}")
        raise HTTPException(status_code=500, detail="Erro interno ao atualizar o ficheiro.")
        
    file_record.versions += 1
    db.commit()
    db.refresh(file_record)
    
    logger.info(f"Ficheiro {file_id} atualizado para a versão {file_record.versions} pelo utilizador {current_user}.")
    
    return {
        "message": "Ficheiro atualizado com sucesso.",
        "file_id": file_record.file_id,
        "new_version": file_record.versions,
        "updated_at": file_record.updated_at
    }

@app.get("/files/")
def list_files(db: Session = Depends(get_db), current_user: str = Depends(get_current_user)):
    # Defense note: response is intentionally split into owned vs shared to keep authorization explicit.
    owned_files = db.query(models.FileMetadata).filter(models.FileMetadata.owner == current_user).all()
    
    shared_files = db.query(models.FileMetadata).join(models.file_shares_table).filter(
        models.file_shares_table.c.shared_user_id == current_user
    ).all()
    
    return {
        "tenant_id": current_user,
        "owned_files": [
            {
                "file_id": f.file_id, 
                "permissions": f.permissions, 
                "versions": f.versions,
                "created_at": f.created_at, 
                "updated_at": f.updated_at,
                "expires_at": f.expires_at,
                "anonymous_links": [link.link_id for link in f.anonymous_links]
            } for f in owned_files
        ],
        "shared_with_me": [
            {
                "file_id": f.file_id, "owner": f.owner, "versions": f.versions
            } for f in shared_files
        ]
    }

@app.get("/files/{file_id}/download")
def download_file(file_id: str, db: Session = Depends(get_db), current_user: str = Depends(get_current_user)):
    file_record = _get_file_for_owner_or_shared(db, file_id, current_user)
    
    if not file_record:
        raise HTTPException(status_code=404, detail="Ficheiro não encontrado.")

    _ensure_file_not_expired(file_record)
    
    file_path = os.path.join(STORAGE_DIR, file_record.file_id)
    if not os.path.exists(file_path):
        # Defense note: remove stale metadata and return 410 to signal resource is no longer available.
        _cleanup_orphaned_metadata(db, file_record)
        raise HTTPException(status_code=410, detail="Ficheiro indisponível (metadados órfãos removidos).")
        
    return FileResponse(path=file_path, media_type='application/octet-stream', filename=file_id)

@app.delete("/files/{file_id}")
def delete_file(file_id: str, db: Session = Depends(get_db), current_user: str = Depends(get_current_user)):
    file_record = db.query(models.FileMetadata).filter(
        models.FileMetadata.file_id == file_id,
        models.FileMetadata.owner == current_user
    ).first()
    
    if not file_record:
        raise HTTPException(status_code=404, detail="Ficheiro não encontrado ou sem acesso. (Apenas o dono pode apagar)")
        
    # Defense note: delete metadata first so links/shares are removed via cascade rules.
    db.delete(file_record)
    db.commit()
    
    file_path = os.path.join(STORAGE_DIR, file_record.file_id)
    if os.path.exists(file_path):
        os.remove(file_path)
        
    logger.info(f"Ficheiro {file_id} eliminado com sucesso pelo owner {current_user}.")
    return {"message": "Ficheiro eliminado com sucesso.", "file_id": file_id}

@app.post("/files/{file_id}/share")
def share_file(file_id: str, share_data: ShareRequest, db: Session = Depends(get_db), current_user: str = Depends(get_current_user)):
    file_record = db.query(models.FileMetadata).filter(
        models.FileMetadata.file_id == file_id,
        models.FileMetadata.owner == current_user
    ).first()
    
    if not file_record:
        raise HTTPException(status_code=404, detail="Ficheiro não encontrado ou sem permissão.")

    _ensure_file_not_expired(file_record)
    
    if share_data.target_user_id == current_user:
        raise HTTPException(status_code=400, detail="Não pode partilhar consigo mesmo.")
        
    target_user = db.query(models.FileShareRecord).filter(models.FileShareRecord.user_id == share_data.target_user_id).first()
    if not target_user:
        target_user = models.FileShareRecord(user_id=share_data.target_user_id)
        db.add(target_user)
        
    if target_user not in file_record.shared_with:
        file_record.shared_with.append(target_user)
        db.commit()
        logger.info(f"Ficheiro {file_id} partilhado com {share_data.target_user_id}.")
        return {"message": f"Ficheiro partilhado com sucesso com {share_data.target_user_id}."}
    else:
        return {"message": f"O ficheiro já se encontra partilhado com {share_data.target_user_id}."}

@app.post("/files/{file_id}/anonymous-link", response_model=AnonymousLinkResponse)
def create_anonymous_link(
    file_id: str, 
    request: Request,
    db: Session = Depends(get_db), 
    current_user: str = Depends(get_current_user)
):
    """Gera um link anónimo seguro para um ficheiro."""
    file_record = db.query(models.FileMetadata).filter(
        models.FileMetadata.file_id == file_id,
        models.FileMetadata.owner == current_user
    ).first()
    
    if not file_record:
        raise HTTPException(status_code=404, detail="Ficheiro não encontrado ou sem permissão para gerar links.")

    _ensure_file_not_expired(file_record)
        
    new_link = models.AnonymousLink(file_id=file_id)
    # Defense note: token is generated server-side with cryptographic randomness in the model.
    db.add(new_link)
    db.commit()
    db.refresh(new_link)
    
    base_url = str(request.base_url).rstrip('/')
    full_url = f"{base_url}/anon/{new_link.link_id}"
    
    logger.info(f"Link anónimo gerado para o ficheiro {file_id} pelo utilizador {current_user}.")
    
    return {
        "link_id": new_link.link_id,
        "file_id": file_id,
        "url": full_url
    }

@app.get("/anon/{link_id}")
def download_file_anonymous(link_id: str, db: Session = Depends(get_db)):
    """Permite o download anónimo através de um token válido."""
    link_record = db.query(models.AnonymousLink).filter(models.AnonymousLink.link_id == link_id).first()
    
    if not link_record:
        raise HTTPException(status_code=404, detail="Link inválido ou expirado.")

    # Defense note: normalize DB timestamps before comparing against UTC "now".
    if link_record.expires_at:
        expiry = _normalize_utc(link_record.expires_at)
        if datetime.now(timezone.utc) > expiry:
            logger.warning(f"Tentativa de acesso a link expirado: {link_id}")
            raise HTTPException(status_code=410, detail="Este link expirou.")

    file_record = db.query(models.FileMetadata).filter(models.FileMetadata.file_id == link_record.file_id).first()
    
    if not file_record:
        raise HTTPException(status_code=404, detail="O ficheiro associado a este link já não existe.")

    _ensure_file_not_expired(file_record)
        
    file_path = os.path.join(STORAGE_DIR, file_record.file_id)
    if not os.path.exists(file_path):
        # Defense note: anonymous flow also triggers orphan cleanup to keep state consistent.
        _cleanup_orphaned_metadata(db, file_record)
        raise HTTPException(status_code=410, detail="Ficheiro indisponível (metadados órfãos removidos).")
        
    logger.info(f"Download anónimo realizado via link: {link_id}")
    return FileResponse(path=file_path, media_type='application/octet-stream', filename=file_record.file_id)

@app.get("/files/{file_id}/permissions")
def get_file_permissions(file_id: str, db: Session = Depends(get_db), current_user: str = Depends(get_current_user)):
    file_record = db.query(models.FileMetadata).filter(
        models.FileMetadata.file_id == file_id,
        models.FileMetadata.owner == current_user
    ).first()
    
    if not file_record:
        raise HTTPException(status_code=404, detail="Ficheiro não encontrado ou sem acesso.")
        
    return {"file_id": file_id, "permissions": file_record.permissions}

@app.put("/files/{file_id}/permissions")
def update_file_permissions(file_id: str, perm_data: PermissionUpdate, db: Session = Depends(get_db), current_user: str = Depends(get_current_user)):
    file_record = db.query(models.FileMetadata).filter(
        models.FileMetadata.file_id == file_id,
        models.FileMetadata.owner == current_user
    ).first()
    
    if not file_record:
        raise HTTPException(status_code=404, detail="Ficheiro não encontrado ou sem acesso.")

    _ensure_file_not_expired(file_record)
    
    if perm_data.permissions not in ["read", "read/write"]:
        raise HTTPException(status_code=400, detail="Permissão inválida. Use 'read' ou 'read/write'.")
        
    file_record.permissions = perm_data.permissions
    db.commit()
    logger.info(f"Permissões do ficheiro {file_id} alteradas para '{perm_data.permissions}' pelo owner {current_user}.")
    
    return {"message": "Permissões atualizadas com sucesso", "permissions": file_record.permissions}


@app.put("/files/{file_id}/ttl")
def update_file_ttl(
    file_id: str,
    ttl_data: FileTTLUpdate,
    db: Session = Depends(get_db),
    current_user: str = Depends(get_current_user),
):
    file_record = db.query(models.FileMetadata).filter(
        models.FileMetadata.file_id == file_id,
        models.FileMetadata.owner == current_user
    ).first()

    if not file_record:
        raise HTTPException(status_code=404, detail="Ficheiro não encontrado ou sem acesso.")

    if ttl_data.expires_in_days <= 0 or ttl_data.expires_in_days > 3650:
        # Defense note: bounded TTL prevents invalid input and unrealistic retention windows.
        raise HTTPException(status_code=400, detail="expires_in_days deve estar entre 1 e 3650.")

    file_record.expires_at = datetime.now(timezone.utc) + timedelta(days=ttl_data.expires_in_days)
    db.commit()
    db.refresh(file_record)

    logger.info(
        f"TTL do ficheiro {file_id} atualizado para {ttl_data.expires_in_days} dias por {current_user}."
    )

    return {
        "message": "TTL atualizado com sucesso.",
        "file_id": file_id,
        "expires_at": file_record.expires_at,
    }