import sqlite3
import hashlib
import bcrypt
from cryptography.fernet import Fernet
import datetime
import os

# BLP (Bell-LaPadula) Security Model Implementation
class BLP:
    def __init__(self, db_path="blp.db", log_path="log.db", key_path="fernet_key.txt"):
        # Paths para os DBs e a chave de criptografia
        self.db_path = db_path  # Principal
        self.log_path = log_path  # Logs
        self.key_path = key_path  # Path para a chave de criptografia
        self.key = self._load_or_generate_key()  # Carrega ou gera a chave de criptografia
        self.cipher = Fernet(self.key)  # Cria o objeto Fernet para criptografia

        # Inicializa os bancos de dados
        self._initialize_db()
        self._initialize_log_db()
        self._initialize_default_users()  # Add default users

        self.MIN_SEC_LEVEL = 1  # Nível mínimo de segurança permitido
        self.MAX_SEC_LEVEL = 5

    # Método para carregar ou gerar a chave de criptografia
    def _load_or_generate_key(self):
        # Se a chave já existe, carrega-a
        if os.path.exists(self.key_path):
            with open(self.key_path, 'rb') as key_file:
                key = key_file.read()

        # Caso contrário, gera uma nova chave
        else:
            key = Fernet.generate_key()
            with open(self.key_path, 'wb') as key_file:
                key_file.write(key)
            # Define permissões para a chave
            os.chmod(self.key_path, 0o600)
        return key

    # Método para inicializar o banco de dados principal
    def _initialize_db(self):
        try:
            with sqlite3.connect(self.db_path) as conn:
                conn.execute("""
                    CREATE TABLE IF NOT EXISTS users (
                        username TEXT PRIMARY KEY,
                        password_hash TEXT,
                        sec_level INTEGER,
                        compartments TEXT,
                        is_trusted INTEGER
                    )
                """)
                conn.execute("""
                    CREATE TABLE IF NOT EXISTS objects (
                        obj_name TEXT PRIMARY KEY,
                        content BLOB,
                        sec_level INTEGER,
                        compartments TEXT
                    )
                """)
                conn.commit()
                # Verify table creation
                cursor = conn.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='users'")
                if cursor.fetchone():
                    print("Users table created successfully.")
                else:
                    print("Users table not found after initialization.")
        except sqlite3.Error as e:
            print("Error initializing database:", e)

    # Método para inicializar o banco de dados de logs
    def _initialize_log_db(self):
        with sqlite3.connect(self.log_path) as conn_log:
            conn_log.execute("""
                CREATE TABLE IF NOT EXISTS logs (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    timestamp DATETIME,
                    username TEXT,
                    action TEXT,
                    details TEXT
                )
            """)
            conn_log.commit()
    

    # Método para inicializar usuários padrão do sistema
    #ISTO AQUI TEM QUE SER REMOVIDO NUMA SITUÇÃO REAL, É SÓ PARA FINS DE TESTE
    def _initialize_default_users(self):
        """Initialize default users with predefined security levels and permissions"""
        default_users = [
            # System Administrator - Full access
            {
                "username": "admin",
                "password": "admin123",
                "sec_level": 5,
                "compartments": ["Air", "Land", "Sea"],
                "is_trusted": True
            },
            # Security Officer - High level access
            {
                "username": "security_officer",
                "password": "secure123",
                "sec_level": 4,
                "compartments": ["Air", "Land", "Sea"],
                "is_trusted": True
            },
            # Military Personnel - Air Operations
            {
                "username": "pilot",
                "password": "fly123",
                "sec_level": 3,
                "compartments": ["Air"],
                "is_trusted": False
            },
            # Military Personnel - Naval Operations
            {
                "username": "naval_officer",
                "password": "sea123",
                "sec_level": 3,
                "compartments": ["Sea"],
                "is_trusted": False
            },
            # Military Personnel - Ground Operations
            {
                "username": "ground_commander",
                "password": "land123",
                "sec_level": 3,
                "compartments": ["Land"],
                "is_trusted": False
            },
            # Intelligence Analyst - Multi-domain access
            {
                "username": "analyst",
                "password": "intel123",
                "sec_level": 4,
                "compartments": ["Air", "Land", "Sea"],
                "is_trusted": True
            },
            # Junior Staff - Limited access
            {
                "username": "clerk",
                "password": "clerk123",
                "sec_level": 1,
                "compartments": ["Land"],
                "is_trusted": False
            },
            # Guest/Contractor - Minimal access
            {
                "username": "contractor",
                "password": "temp123",
                "sec_level": 2,
                "compartments": ["Land"],
                "is_trusted": False
            }
        ]

        with sqlite3.connect(self.db_path) as conn:
            for user in default_users:
                # Check if user already exists
                existing = conn.execute(
                    "SELECT username FROM users WHERE username = ?",
                    (user["username"],)
                ).fetchone()
                
                if not existing:
                    # Hash the password
                    hashed_password = bcrypt.hashpw(
                        user["password"].encode('utf-8'), 
                        bcrypt.gensalt()
                    )
                    hashed_password_str = hashed_password.decode('utf-8')
                    compartments_str = ",".join(user["compartments"])
                    
                    # Insert the user
                    conn.execute(
                        "INSERT INTO users (username, password_hash, sec_level, compartments, is_trusted) VALUES (?, ?, ?, ?, ?)",
                        (user["username"], hashed_password_str, user["sec_level"], 
                         compartments_str, int(user["is_trusted"]))
                    )
                    print(f"Created default user: {user['username']} (Level {user['sec_level']})")
            
            conn.commit()

    # Método para registrar ações no banco de dados de logs
    def _log_action(self, username, action, details):
        with sqlite3.connect(self.log_path) as conn_log:
            timestamp = datetime.datetime.now().isoformat()
            conn_log.execute(
                "INSERT INTO logs (timestamp, username, action, details) VALUES (?, ?, ?, ?)",
                (timestamp, username, action, details)
            )
            conn_log.commit()

    # Método para obter informações do usuário
    def _get_user_info(self, username):
        with sqlite3.connect(self.db_path) as conn:
            result = conn.execute(
                "SELECT sec_level, compartments, is_trusted FROM users WHERE username = ?",
                (username,)
            ).fetchone()
            if result:
                sec_level = result[0]
                compartments = result[1].split(",") if result[1] else []  # Converte "Air,Land,Sea" em ["Air", "Land", "Sea"]
                is_trusted = result[2]
                return sec_level, compartments, is_trusted
            return None, None, None

    # Método simplificado para registrar um novo usuário (apenas username/password)
    def register_user(self, username, password):
        """Register a new user with default minimal permissions"""
        with sqlite3.connect(self.db_path) as conn:
            try:
                # Check if user already exists
                existing = conn.execute(
                    "SELECT username FROM users WHERE username = ?",
                    (username,)
                ).fetchone()
                
                if existing:
                    return False
                
                # Gerar o hash da password com bcrypt
                hashed_password = bcrypt.hashpw(password.encode('utf-8'), bcrypt.gensalt())
                hashed_password_str = hashed_password.decode('utf-8')
                
                # New users get minimal permissions by default
                default_sec_level = 1
                default_compartments = ["Land"]  # Basic compartment
                default_is_trusted = False
                
                compartments_str = ",".join(default_compartments)
                conn.execute(
                    "INSERT INTO users (username, password_hash, sec_level, compartments, is_trusted) VALUES (?, ?, ?, ?, ?)",
                    (username, hashed_password_str, default_sec_level, compartments_str, int(default_is_trusted))
                )
                conn.commit()
                self._log_action("system", "user_registered", f"New user {username} registered with default permissions")
                return True
            except sqlite3.IntegrityError:
                return False

    # Método para autenticar um usuário
    def login(self, username, password):
        with sqlite3.connect(self.db_path) as conn:
            result = conn.execute(
                "SELECT password_hash FROM users WHERE username = ?",
                (username,)
            ).fetchone()
            if result:
                stored_hash = result[0].encode('utf-8')  # Convert stored hash to bytes
                if bcrypt.checkpw(password.encode('utf-8'), stored_hash):
                    self._log_action(username, "login", "success")
                    return True
                else:
                    self._log_action(username, "login", "failed: incorrect password")
                    return False
            else:
                self._log_action(username, "login", "failed: user not found")
                return False

    # Method to list all users (for admin purposes)
    def list_users(self, admin_username):
        """List all users - only for trusted users with high clearance"""
        _, _, is_trusted = self._get_user_info(admin_username)
        admin_sec_level, _, _ = self._get_user_info(admin_username)
        
        if not is_trusted or admin_sec_level < 4:
            self._log_action(admin_username, "list_users", "denied: insufficient privileges")
            return None
            
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.execute(
                "SELECT username, sec_level, compartments, is_trusted FROM users ORDER BY sec_level DESC, username"
            )
            users = []
            for row in cursor.fetchall():
                users.append({
                    "username": row[0],
                    "sec_level": row[1],
                    "compartments": row[2].split(",") if row[2] else [],
                    "is_trusted": bool(row[3])
                })
            
            self._log_action(admin_username, "list_users", f"success: listed {len(users)} users")
            return users

    # Método para criar objetos
    def create_object(self, username, obj_name, content, obj_sec_level, obj_compartments):
        user_sec_level, user_compartments, _ = self._get_user_info(username)

        #importante ser == para garantir que o usuário só pode criar objetos com o mesmo nível de segurança
        if user_sec_level == obj_sec_level and set(obj_compartments).issubset(set(user_compartments)):
            encrypted_content = self.cipher.encrypt(content.encode())
            with sqlite3.connect(self.db_path) as conn:
                compartments_str = ",".join(obj_compartments)
                conn.execute(
                    "INSERT INTO objects (obj_name, content, sec_level, compartments) VALUES (?, ?, ?, ?)",
                    (obj_name, encrypted_content, obj_sec_level, compartments_str)
                )
                conn.commit()
                return True
        return False

    # Método para ler um objeto
    def read_object(self, username, obj_name):
        user_sec_level, user_compartments, _ = self._get_user_info(username)
        with sqlite3.connect(self.db_path) as conn:
            result = conn.execute(
                "SELECT content, sec_level, compartments FROM objects WHERE obj_name = ?",
                (obj_name,)
            ).fetchone()
            if result:
                obj_sec_level = result[1]
                obj_compartments = result[2].split(",") if result[2] else []
                if user_sec_level >= obj_sec_level and set(obj_compartments).issubset(set(user_compartments)):
                    return self.cipher.decrypt(result[0]).decode()
            return None
    
    # Método para expurgar um objeto
    def expurgate_object(self, username, obj_name, new_obj_name, filtered_content, new_sec_level):
            _, _, is_trusted = self._get_user_info(username)
            if not is_trusted:
                self._log_action(username, "expurgate_object", f"denied: not trusted, obj_name={obj_name}")
                return False
            with sqlite3.connect(self.db_path) as conn:
                result = conn.execute(
                    "SELECT sec_level, compartments FROM objects WHERE obj_name = ?",
                    (obj_name,)
                ).fetchone()
                if not result:
                    self._log_action(username, "expurgate_object", f"denied: obj_name={obj_name} not found")
                    return False
                obj_sec_level, obj_compartments = result[0], result[1].split(",") if result[1] else []

                # Validar o nível de segurança do novo objeto
                if new_sec_level < self.MIN_SEC_LEVEL or new_sec_level > obj_sec_level:
                    self._log_action(username, "expurgate_object", f"denied: invalid new_sec_level={new_sec_level}, must be between {self.MIN_SEC_LEVEL} and {obj_sec_level}")
                    return False

                encrypted_content = self.cipher.encrypt(filtered_content.encode())
                compartments_str = ",".join(obj_compartments)  
                try:
                    conn.execute(
                        "INSERT INTO objects (obj_name, content, sec_level, compartments) VALUES (?, ?, ?, ?)",
                        (new_obj_name, encrypted_content, new_sec_level, compartments_str)
                    )
                    conn.commit()
                    self._log_action(username, "expurgate_object", f"success: from {obj_name} to {new_obj_name} with sec_level={new_sec_level}")
                    return True
                except sqlite3.IntegrityError:
                    self._log_action(username, "expurgate_object", f"failed: new_obj_name={new_obj_name} already exists")
                    return False

    # Método para definir os níveis de segurança de outro usuário
    def set_clearance(self, admin_username, target_username, new_sec_level, new_compartments):
        _, _, is_trusted = self._get_user_info(admin_username)
        if not is_trusted:
            self._log_action(admin_username, "set_clearance", f"denied: not trusted, target={target_username}")
            return False
        with sqlite3.connect(self.db_path) as conn:
            compartments_str = ",".join(new_compartments)
            result = conn.execute(
                "UPDATE users SET sec_level = ?, compartments = ? WHERE username = ?",
                (new_sec_level, compartments_str, target_username)
            )
            conn.commit()
            if result.rowcount > 0:
                self._log_action(admin_username, "set_clearance", f"success: target={target_username}, new_sec_level={new_sec_level}, new_compartments={new_compartments}")
                return True
            return False

    # Método para obter logs de ações
    def get_logs(self, username):
        sec_level, _, _ = self._get_user_info(username)
        if sec_level is None or sec_level != 5:
            self._log_action(username, "get_logs", "denied: insufficient security level")
            return None
        with sqlite3.connect(self.log_path) as conn_log:
            cursor = conn_log.execute("SELECT id, timestamp, username, action, details FROM logs ORDER BY timestamp DESC")
            logs = [dict(id=row[0], timestamp=row[1], username=row[2], action=row[3], details=row[4]) for row in cursor.fetchall()]
            self._log_action(username, "get_logs", "success")
            return logs

    # Método para listar objetos legíveis
    def list_readable_objects(self, username):
        user_sec_level, user_compartments, _ = self._get_user_info(username)
        if user_sec_level is None:
            self._log_action(username, "list_readable_objects", "denied: user not found")
            return []
        with sqlite3.connect(self.db_path) as conn:
            result = conn.execute(
                "SELECT obj_name, sec_level, compartments FROM objects"
            ).fetchall()
            objects = [
                row[0] for row in result
                if user_sec_level >= row[1] and set(row[2].split(",") if row[2] else []).issubset(set(user_compartments))
            ]
            self._log_action(username, "list_readable_objects", f"success: found {len(objects)} objects")
            return objects

    # Método para listar objetos que o usuário confiável pode expurgar
    def list_expurgatable_objects(self, username):
        _, _, is_trusted = self._get_user_info(username)
        if not is_trusted:
            self._log_action(username, "list_expurgatable_objects", "denied: not trusted")
            return []
        with sqlite3.connect(self.db_path) as conn:
            result = conn.execute("SELECT obj_name FROM objects").fetchall()
            objects = [row[0] for row in result]
            self._log_action(username, "list_expurgatable_objects", f"success: found {len(objects)} objects")
            return objects