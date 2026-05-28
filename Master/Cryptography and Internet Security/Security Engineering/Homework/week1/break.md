# Análise de Segurança (Vulnerabilidades)

Durante o desenvolvimento e testes da API, identifiquei as seguintes falhas de segurança na implementação atual:

## 1. Armazenamento de Passwords/Chaves em Texto Limpo
**Problema:** O ficheiro `users.json` guarda as API Keys diretamente em texto.
**Risco:** Se alguém conseguir ler este ficheiro (ex: um administrador mal intencionado ou via outra falha), consegue aceder a todas as contas.
**Solução Académica:** Deveria guardar-se apenas o *hash* da chave (ex: usando SHA-256) e comparar os hashes no login.

## 2. Comunicação insegura (HTTP)
**Problema:** O servidor corre em HTTP.
**Risco:** As chaves de API são enviadas no *Header* de cada pedido. Qualquer pessoa na mesma rede Wi-Fi pode intercetar o tráfego e roubar a chave (Ataque *Man-in-the-Middle*).
**Solução Académica:** É obrigatório o uso de HTTPS (TLS) em produção.

## 3. Path Traversal (Mitigado, mas relevante)
**Observação:** O código atual renomeia os ficheiros para UUIDs, o que protege contra *Path Traversal* (ex: enviar `../../etc/passwd`).
**Nota:** Se usássemos o nome original do ficheiro para gravar no disco, um atacante poderia sobrescrever ficheiros do sistema. Esta foi uma decisão de design consciente para mitigar o risco.

## 4. Negação de Serviço (DoS)
**Problema:** Não há limite para o tamanho dos ficheiros nem para o número de ficheiros por utilizador.
**Risco:** Um utilizador pode enviar ficheiros gigantes até encher o disco do servidor, fazendo com que o serviço pare de funcionar para todos.
**Solução Académica:** Implementar verificação de `Content-Length` e quotas por utilizador.

## 5. Upload de Ficheiros Executáveis
**Problema:** A API aceita qualquer tipo de ficheiro (ex: `.py`, `.exe`).
**Risco:** Embora o servidor não os execute diretamente, o sistema serve de repositório para malware.
**Solução Académica:** Validar o tipo de ficheiro (MIME type) e aceitar apenas formatos seguros (PDF, TXT, IMG).

## 7. Fuga de Informação via Metadados (Metadata Leakage)
**Problema:** Embora o conteúdo do ficheiro esteja agora encriptado (graças à correção do ponto 2), o ficheiro metadata.json continua em texto limpo.
**Risco:** Um atacante com acesso ao servidor consegue ver os nomes originais dos ficheiros, o tamanho exato e a data de upload.
**Cenário:** Se o utilizador Alice fizer upload de um ficheiro chamado lista_de_clientes_com_cancro.pdf, a privacidade foi violada apenas pelo nome, mesmo que o atacante não consiga ler o conteúdo do PDF.
**Solução Académica:** Os metadados também devem ser encriptados ou armazenados numa base de dados cifrada. O nome do ficheiro nunca deve revelar o contexto do conteúdo.

## 8. Ataques de Timing (Side-Channel Attack) na Autenticação
**Problema:** A verificação da API Key no código (if data.get('api_key') == token) utiliza a comparação de strings padrão do Python.
**Risco:** A comparação padrão retorna False assim que encontra o primeiro carácter diferente. Isto significa que verificar uma chave quase correta demora milissegundos a mais do que uma chave totalmente errada. Um atacante pode medir estes tempos de resposta minúsculos para descobrir a chave, carácter a carácter.
**Solução Académica:** Usar uma função de "comparação de tempo constante", como secrets.compare_digest(user_input, actual_key), que demora sempre o mesmo tempo a responder, independentemente de a chave estar quase certa ou toda errada.
