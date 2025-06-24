# GrupoTP-23 - Projeto Moonshot

Moonshot é uma aplicação web baseada em Django, implantada em um ambiente de nuvem utilizando Kubernetes (GKE), com automações feitas via Ansible e Vagrant. Este projeto tem como objetivo demonstrar uma infraestrutura moderna e escalável para aplicações web, integrando boas práticas DevOps e automação.

## 📦 Estrutura do Projeto
```
.
├── docker/                 # Configuração Docker da aplicação Moonshot
│   └── moonshot/          # Código-fonte da aplicação Django + certificados TLS
├── inventory/             # Inventário do Ansible para ambientes GCP
├── roles/                 # Papéis Ansible para deploy, testes e remoção
├── *.yml                  # Playbooks principais do Ansible
├── Vagrantfile            # Configuração da VM local com Vagrant
├── gcp_cred_file.json     # Credenciais de autenticação com GCP
└── README.md              # Documentação do projeto

```

## 🚀 Tecnologias Utilizadas

- **Python 3 / Django** - Backend REST com autenticação e operações CRUD
- **Docker** - Containerização da aplicação
- **Kubernetes (GKE)** - Orquestração de containers na nuvem
- **Ansible** - Automação de infraestrutura e deploy
- **Vagrant** - Provisão de ambiente de desenvolvimento local
- **Google Cloud Platform** - Infraestrutura em nuvem

## 🛠️ Como Usar

### 1. Requisitos

- Docker
- Vagrant
- Ansible
- Conta GCP com permissões para criar clusters GKE
- Python 3

### 2. Deploy da Virtual Machine

```bash
vagrant up
vagrant ssh
```
### 3. Configurar GCP e Cluster

```bash
ansible-playbook gke-cluster-create.yml
```

### 4. Deploy da Aplicação Moonshot

```bash
ansible-playbook moonshot-deploy.yml
```
### 5. Testes

```bash
ansible-playbook test-all.yml
```
### 6. Remover a Aplicação e o Cluster

```bash
ansible-playbook moonshot-undeploy.yml
ansible-playbook gke-cluster-destroy.yml
```


## Testes
Os testes automatizados estão definidos nas roles Ansible dentro de:
```bash
roles/test_moonshot/
```

## Segurança
A aplicação utiliza certificados TLS para comunicação segura, localizados em:

```bash
docker/moonshot/certs/
```
## Documentação Técnica
No o arquivo relatorio_GrupoTP-23.pdf existem detalhes técnicos aprofundados, arquitetura da solução, decisões de projeto e desafios enfrentados.

## Sobre a Aplicação Moonshot
A API Django implementa um sistema REST completo com:

- Autenticação via Token

- Operações CRUD completas

- Estrutura modular para escalabilidade

- Deploy contínuo via Ansible e GKE

🧰 Comandos Úteis (no container)

```bash
python manage.py migrate
python manage.py runserver
```