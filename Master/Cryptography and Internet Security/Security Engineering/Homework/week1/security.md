# Requisitos de Segurança

- **Autenticação Forte**: Usar chaves de API longas e aleatórias (32+ bytes) para identificar inequivocamente cada utilizador.
- **HTTPS Obrigatório**: Todo o tráfego deve ser encriptado (TLS) para impedir o roubo de chaves na rede (Sniffing).
- **Encriptação em Repouso**: Os ficheiros no disco devem ser guardados encriptados para que o acesso físico ao servidor não comprometa os dados.
- **Sanitização de Inputs**: Nunca usar o nome original do ficheiro no sistema de ficheiros para evitar ataques de *Path Traversal*.
- **Validação de Conteúdo**: Verificar os "magic bytes" do ficheiro e não confiar apenas na extensão (ex: impedir renomear `.exe` para `.txt`).
- **Limites de Tamanho (Quotas)**: Definir tamanho máximo de upload para evitar que encham o disco (Negação de Serviço).
- **Rate Limiting**: Limitar o número de pedidos por minuto para impedir ataques de força bruta ou scrapers.
- **Prevenção de IDOR**: Verificar sempre, no backend, se o ID do ficheiro pedido pertence realmente ao utilizador autenticado.
- **Least Privillege**: O processo do servidor deve correr com um utilizador limitado do sistema operativo, nunca como `root`.
- **Ocultação de Erros**: As respostas de erro devem ser genéricas ("Erro Interno") e nunca mostrar demais ou detalhes de código.
- **Logs de Auditoria**: Registar todas as ações críticas (quem fez upload/download e quando) para efeitos forenses e de incident Response.
- **Gestão de Segredos**: Nunca deixar chaves de encriptação ou configurações sensíveis escritas no código (hardcoded); usar variáveis de ambiente.