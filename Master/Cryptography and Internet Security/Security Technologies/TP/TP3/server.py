from fastapi import Header,FastAPI, HTTPException, Depends
from pydantic import BaseModel
import jwt
import uvicorn
from BLP import BLP
import datetime

app = FastAPI()
blp = BLP()
SECRET_KEY = "mysecretkey123" #Simplemente para fins de teste, nao usar em produção

class RegisterRequest(BaseModel):
    username: str
    password: str
    # Removed sec_level, compartments, and is_trusted - these are now set by system

class User(BaseModel):
    username: str
    password: str

class Object(BaseModel):
    obj_name: str
    content: str
    sec_level: int
    compartments: list[str]

class ExpurgateRequest(BaseModel):
    obj_name: str
    new_obj_name: str
    filtered_content: str
    new_sec_level: int

class ClearanceRequest(BaseModel):
    target_username: str
    new_sec_level: int
    new_compartments: list[str]

# Função para obter o usuário atual a partir do token JWT
def get_current_user(authorization: str = Header(None)):
    if authorization is None:
        raise HTTPException(status_code=401, detail="Authorization header missing")
    try:
        scheme, token = authorization.split()
        if scheme.lower() != "bearer":
            raise HTTPException(status_code=401, detail="Invalid authentication scheme")
        payload = jwt.decode(token, SECRET_KEY, algorithms=["HS256"])
        return payload["username"]
    except (ValueError, IndexError, jwt.InvalidTokenError):
        raise HTTPException(status_code=401, detail="Invalid or expired token")

# Endpoint para registar um novo usuário (simplified - only username/password)
@app.post("/register")
def register(request: RegisterRequest):
    if blp.register_user(request.username, request.password):
        return {"message": "User registered with default permissions"}
    raise HTTPException(status_code=400, detail="User already exists")

# Endpoint para login e geração de token JWT
@app.post("/login")
def login(user: User):
    if blp.login(user.username, user.password):
        
        expiration = datetime.datetime.now() + datetime.timedelta(minutes=30) # Token válido por 30 minutos
        exp_timestamp = int(expiration.timestamp())
        
        token = jwt.encode(
            {"username": user.username, "exp": exp_timestamp},
            SECRET_KEY,
            algorithm="HS256"
        )
        return {"token": token}
    raise HTTPException(status_code=401, detail="Invalid credentials")

# New endpoint to list users (admin only)
@app.get("/list_users")
def list_users(username: str = Depends(get_current_user)):
    users = blp.list_users(username)
    if users is None:
        raise HTTPException(status_code=403, detail="Permission denied")
    return {"users": users}

# Endpoints para operações de BLP
@app.post("/create_object")
def create_object(obj: Object, username: str = Depends(get_current_user)):
    if blp.create_object(username, obj.obj_name, obj.content, obj.sec_level, obj.compartments):
        return {"message": "Object created"}
    raise HTTPException(status_code=403, detail="Permission denied")

@app.get("/read_object/{obj_name}")
def read_object(obj_name: str, token: str = Depends(get_current_user)):
    content = blp.read_object(token, obj_name)
    if content is not None:
        return {"content": content}
    raise HTTPException(status_code=403, detail="Permission denied or object not found")

@app.post("/expurgate_object")
def expurgate_object(request: ExpurgateRequest, token: str = Depends(get_current_user)):
    if blp.expurgate_object(token, request.obj_name, request.new_obj_name, request.filtered_content, request.new_sec_level):
        return {"message": "Object expurgated"}
    raise HTTPException(status_code=403, detail="Permission denied or invalid security level")

@app.post("/set_clearance")
def set_clearance(request: ClearanceRequest, token: str = Depends(get_current_user)):
    if blp.set_clearance(token, request.target_username, request.new_sec_level, request.new_compartments):
        return {"message": "Clearance updated"}
    raise HTTPException(status_code=403, detail="Permission denied or user not found")

@app.get("/list_readable_objects")
def list_readable_objects(token: str = Depends(get_current_user)):
    objects = blp.list_readable_objects(token)
    return {"objects": objects}

@app.get("/list_expurgatable_objects")
def list_expurgatable_objects(token: str = Depends(get_current_user)):
    objects = blp.list_expurgatable_objects(token)
    return {"objects": objects}

@app.get("/get_logs")
def get_logs(username: str = Depends(get_current_user)):
    logs = blp.get_logs(username)
    if logs is None:
        raise HTTPException(status_code=403, detail="Not enough security")
    return {"logs": logs}

if __name__ == "__main__":
    # Servidor para teste local
    uvicorn.run(
        app,
        host="0.0.0.0",
        port=8000,

        # Generate with: openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -days 365 -nodes

        ssl_keyfile="certs\\server.key.pem",
        ssl_certfile="certs\\fullchain.pem"
    )