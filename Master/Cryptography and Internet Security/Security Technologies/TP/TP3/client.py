import requests
import json
import os

BASE_URL = "https://localhost:8000"  # Use HTTPS
TOKEN = None
USERNAME = None
CA = f"certs\\ca.cert.pem"

# Função de Login
def login(username, password):
    # Validar se o utilizador já está autenticado
    global TOKEN, USERNAME
    if TOKEN:
        print("Já estás logado!!!! Reinicia o programa para te logares com outro utilizador")
        input("Press Enter to continue...")
        return
    try:
        # Preparar os dados de login
        login_data = {"username": username, 
                      "password": password}

        USERNAME = username

        # Fazer a requisição de login
        response = requests.post(f"{BASE_URL}/login", json=login_data, verify=CA)
        response.raise_for_status()
        TOKEN = response.json()["token"]
        print("Login Bem Sucessido")
        input("Press Enter to continue...")
    except requests.RequestException as e:
        print(f"Login falhou: {e}")
        input("Press Enter to continue...")

# Função de Registar - SIMPLIFIED (only username/password)
def register(username, password):
    # Validar se o utilizador já está autenticado
    if TOKEN:
        print("Já estás logado!!!! Reinicia o programa para te registares")
        input("Press Enter to continue...")
        return
    try:
        registo_data = {
            "username": username,
            "password": password
            # Security levels and permissions are now set by the system
        }

        response = requests.post(f"{BASE_URL}/register", json=registo_data, verify=CA)
        response.raise_for_status()
        print("Registado com sucesso! (Nível de segurança padrão: 1, Compartimento: Land)")
        print("Contacte um administrador para alterar as suas permissões.")
        input("Press Enter to continue...")
        # Executar o login após o registo
        login(username, password)
    except requests.RequestException as e:
        print(f"Registo Falhou!: {e}")
        input("Press Enter to continue...")

# Função para criar um objeto
def create_object(obj_name, content, sec_level, compartments):
    # Validar se o utilizador está autenticado
    if not TOKEN:
        print("Por favor, faça login primeiro!")
        input("Press Enter to continue...")
        return
    headers = {"Authorization": f"Bearer {TOKEN}"}
    try:
        # Objeto a ser criado
        objeto = {
            "obj_name": obj_name,
            "content": content,
            "sec_level": sec_level,
            "compartments": compartments,  # Lista com "Air", "Land", "Sea" ou subconjunto
        }

        response = requests.post(f"{BASE_URL}/create_object", json=objeto, headers=headers, verify=CA)
        response.raise_for_status()

        print("Objeto criado com sucesso!")
        input("Press Enter to continue...")
    except requests.RequestException as e:
        if hasattr(e, 'response') and e.response is not None:
            print(f"Falha na Criacao do Objeto: {e.response.status_code} - {e.response.text}")
        else:
            print(f"Falha na Criacao do Objeto: {e}")
        input("Press Enter to continue...")

# Função para ler os Objetos que o utilizador pode ler.
def read_object(obj_name):
    # Validar se o utilizador está autenticado
    if not TOKEN:
        print("Por favor, faça login primeiro.")
        input("Press Enter to continue...")
        return
    headers = {"Authorization": f"Bearer {TOKEN}"}
    try:
        # Fazer a requisição para ler o objeto
        response = requests.get(f"{BASE_URL}/read_object/{obj_name}", headers=headers, verify=CA)
        response.raise_for_status()

        print(f"Conteúdo do objeto: {response.json()['content']}")
        input("Press Enter to continue...")
    except requests.RequestException as e:
        print(f"Falha ao ler objeto: {e}")
        input("Press Enter to continue...")

# Função para listar e ler objetos que o utilizador pode ler.
def list_and_read_object():
    # Validar se o utilizador está autenticado
    if not TOKEN:
        print("Faça login primeiro!")
        input("Press Enter to continue...")
        return
    headers = {"Authorization": f"Bearer {TOKEN}"}
    try:
        # Fazer a requisição para listar os objetos que o utilizador pode ler
        response = requests.get(f"{BASE_URL}/list_readable_objects", headers=headers, verify=CA)
        response.raise_for_status()
        objects = response.json()["objects"]

        # Verificar se há objetos disponíveis
        if not objects:
            print("Nao ha objetos disponiveis para leitura.")
            input("Press Enter to continue...")
            return
        
        # Exibir os objetos disponíveis
        print("Objetos disponíveis para leitura:")
        for i, obj in enumerate(objects, 1):
            print(f"{i}. {obj}")

        try:
            choice = int(input("Selecione um objeto pelo número: ")) - 1
            if 0 <= choice < len(objects):
                selected_obj = objects[choice]
                read_object(selected_obj)
            else:
                print("Seleção inválida.")
                input("Press Enter to continue...")
        except ValueError:
            print("Entrada inválida. Digite um número.")
            input("Press Enter to continue...")

    except requests.RequestException as e:
        print(f"Falha ao listar objetos: {e}")
        input("Press Enter to continue...")

# Função para expurgar um objeto
def get_expurgatable_objects():
    headers = {"Authorization": f"Bearer {TOKEN}"}
    try:
        response = requests.get(f"{BASE_URL}/list_expurgatable_objects", headers=headers, verify=CA)
        response.raise_for_status()
        return response.json().get("objects", [])
    except requests.RequestException as e:
        print(f"Erro ao buscar objetos expurgáveis: {str(e)}")
        return []

def expurgate_object(obj_name, new_obj_name, filtered_content, new_sec_level):
    # Validar se o utilizador está autenticado
    if not TOKEN:
        print("Faça login primeiro!")
        return
    headers = {"Authorization": f"Bearer {TOKEN}"}
    payload = {
        "obj_name": obj_name,
        "new_obj_name": new_obj_name,
        "filtered_content": filtered_content,
        "new_sec_level": new_sec_level
    }
    try:
        response = requests.post(f"{BASE_URL}/expurgate_object", json=payload, headers=headers, verify=CA)
        response.raise_for_status()
        print("Objeto expurgado com sucesso!")
        input("Press Enter to continue...")
    except requests.RequestException as e:
        error_msg = "Erro desconhecido"
        if e.response is not None:
            try:
                error_msg = f"{e.response.status_code} - {e.response.json().get('detail', 'Sem detalhes')}"
            except:
                error_msg = f"{e.response.status_code} - {e.response.text}"
        print(f"Falha ao expurgar objeto: {error_msg}")
        input("Press Enter to continue...")

def list_and_expurgate_object():
    # Recolher os objetos que o utilizador pode expurgar
    objects = get_expurgatable_objects()
    if not objects:
        print("Nenhum objeto disponível para expurgar.")
        input("Press Enter to continue...")
        return

    print("\nObjetos expurgáveis:")
    for i, obj in enumerate(objects, 1):
        print(f"{i}. {obj}")

    # Obter a seleção do utilizador
    while True:
        try:
            choice = int(input("\nSelecione o número do objeto para expurgar: ")) - 1
            if 0 <= choice < len(objects):
                break
            print(f"Por favor, selecione um número entre 1 e {len(objects)}.")
        except ValueError:
            print("Entrada inválida. Digite um número inteiro.")

    selected_obj = objects[choice]

    # Obter detalhes para o expurgo
    new_obj_name = input("Novo nome para a versão expurgada: ")
    filtered_content = input("Conteúdo filtrado: ")

    while True:
        try:
            new_sec_level = int(input("Novo nível de segurança (1-5): "))
            if 1 <= new_sec_level <= 5:
                break
            print("O nível de segurança deve estar entre 1 e 5.")
        except ValueError:
            print("Por favor, insira um número inteiro para o nível de segurança.")

    # Perform expurgation
    expurgate_object(selected_obj, new_obj_name, filtered_content, new_sec_level)

# Função para visualizar os logs de ações do utilizador
def view_logs():
    # Validar se o utilizador está autenticado
    if not TOKEN:
        print("Faça login primeiro")
        input("Press Enter to continue...")
        return
    headers = {"Authorization": f"Bearer {TOKEN}"}
    try:
        response = requests.get(f"{BASE_URL}/get_logs", headers=headers, verify=CA)
        response.raise_for_status()

        # Exibir os logs
        logs = response.json()["logs"]
        if not logs:
            print("Nenhum log disponível.")
        else:
            print(f"\n=== LOGS ({len(logs)} entradas) ===")
            for log in logs:
                print(f"{log['timestamp']} - {log['username']} - {log['action']} - {log['details']}")

        input("\nPress Enter to continue...")
    except requests.RequestException as e:
        if e.response and e.response.status_code == 403:
            print("Não tem permissões suficientes para ver os logs.")
        else:
            print(f"Falha ao recuperar logs: {e}")
        input("Press Enter to continue...")

# Função para definir o nível de segurança de um utilizador
def set_clearance(target_username, new_sec_level, new_compartments):
    if not TOKEN:
        print("Faça login primeiro!")
        input("Press Enter to continue...")
        return
    headers = {"Authorization": f"Bearer {TOKEN}"}
    try:
        target = {
            "target_username": target_username,
            "new_sec_level": new_sec_level,
            "new_compartments": new_compartments
        }
        response = requests.post(f"{BASE_URL}/set_clearance", json=target, headers=headers, verify=CA)
        response.raise_for_status()
        print("Nível de segurança atualizado com sucesso!")
        input("Press Enter to continue...")
    except requests.RequestException as e:
        if e.response and e.response.status_code == 403:
            print("Não tem permissões suficientes para alterar níveis de segurança.")
        else:
            print(f"Falha ao definir nível de segurança: {e}")
        input("Press Enter to continue...")

# New function to list users (admin only)
def list_users():
    if not TOKEN:
        print("Faça login primeiro!")
        input("Press Enter to continue...")
        return
    headers = {"Authorization": f"Bearer {TOKEN}"}
    try:
        response = requests.get(f"{BASE_URL}/list_users", headers=headers, verify=CA)
        response.raise_for_status()
        
        users = response.json()["users"]
        if not users:
            print("Nenhum utilizador encontrado.")
        else:
            print(f"\n=== UTILIZADORES ({len(users)}) ===")
            print(f"{'Username':<20} {'Nível':<6} {'Trusted':<8} {'Compartimentos'}")
            print("-" * 60)
            for user in users:
                trusted_str = "Sim" if user["is_trusted"] else "Não"
                compartments_str = ", ".join(user["compartments"]) if user["compartments"] else "Nenhum"
                print(f"{user['username']:<20} {user['sec_level']:<6} {trusted_str:<8} {compartments_str}")
        
        input("\nPress Enter to continue...")
    except requests.RequestException as e:
        if e.response and e.response.status_code == 403:
            print("Não tem permissões suficientes para listar utilizadores.")
        else:
            print(f"Falha ao listar utilizadores: {e}")
        input("Press Enter to continue...")

def receive_compartments():
    # Função para receber os compartimentos do utilizador
    compartments = []
    for comp in ["Air", "Sea", "Land"]:
        if input(f"Incluir compartimento {comp}? (y/n): ").lower().strip() == "y":
            compartments.append(comp)
    return compartments

def main():
    while True:
        os.system('cls' if os.name == 'nt' else 'clear')
        print("====== Cliente BLP (Bell-LaPadula Security Model) ======")
        if TOKEN:
            print(f"Logged in as: {USERNAME}")
            # Don't show raw token for security
        else:
            print("Not logged in")
        
        print("\n=== MENU ===")
        print("1. Login")
        print("2. Registar (novo utilizador)")
        print("3. Criar Objeto")
        print("4. Listar e Ler Objeto")
        print("5. Listar e Expurgar Objeto")
        print("6. Definir Nível de Segurança (Admin)")
        print("7. Ver Logs (Admin)")
        print("8. Listar Utilizadores (Admin)")
        print("9. Sair")
        
        choice = input("\nEscolha uma opção (1-9): ").strip()
        
        if choice == "1":
            if TOKEN:
                print("Já está logado! Reinicie o programa para fazer login com outro utilizador.")
                input("Press Enter to continue...")
                continue
            username = input("Username: ").strip()
            password = input("Password: ")
            login(username, password)
            
        elif choice == "2":
            if TOKEN:
                print("Já está logado! Reinicie o programa para se registar.")
                input("Press Enter to continue...")
                continue
            username = input("Username: ").strip()
            password = input("Password: ")
            register(username, password)
            
        elif choice == "3":
            if not TOKEN:
                print("Faça login primeiro!")
                input("Press Enter to continue...")
                continue
            obj_name = input("Nome do Objeto: ").strip()
            content = input("Conteúdo: ")
            while True:
                try:
                    sec_level = int(input("Nível de Segurança (1-5): "))
                    if 1 <= sec_level <= 5:
                        break
                    print("Nível deve estar entre 1 e 5.")
                except ValueError:
                    print("Digite um número válido.")
            compartments = receive_compartments()
            if not compartments:
                print("Deve selecionar pelo menos um compartimento.")
                input("Press Enter to continue...")
                continue
            create_object(obj_name, content, sec_level, compartments)
            
        elif choice == "4":
            list_and_read_object()
            
        elif choice == "5":
            list_and_expurgate_object()
            
        elif choice == "6":
            if not TOKEN:
                print("Faça login primeiro!")
                input("Press Enter to continue...")
                continue
            target_username = input("Target Username: ").strip()
            while True:
                try:
                    new_sec_level = int(input("Novo Nível de Segurança (1-5): "))
                    if 1 <= new_sec_level <= 5:
                        break
                    print("Nível deve estar entre 1 e 5.")
                except ValueError:
                    print("Digite um número válido.")
            new_compartments = receive_compartments()
            if not new_compartments:
                print("Deve selecionar pelo menos um compartimento.")
                input("Press Enter to continue...")
                continue
            set_clearance(target_username, new_sec_level, new_compartments)
            
        elif choice == "7":
            view_logs()
            
        elif choice == "8":
            list_users()
            
        elif choice == "9":
            print("A sair...")
            break
            
        else:
            print("Opção inválida. Escolha um número entre 1 e 9.")
            input("Press Enter to continue...")

if __name__ == "__main__":
    main()