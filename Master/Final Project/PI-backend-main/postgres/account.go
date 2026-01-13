package postgres

import (
	"database/sql" // Importar sql para sql.ErrNoRows
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
)

type AccountService struct {
	db                         *DB
	AccountConfirmationService npadmin.AccountConfirmationService
	ResetPasswordService       npadmin.ResetPasswordService
	UpdateEmailService         npadmin.UpdateEmailService
}

func NewAccountService(db *DB) *AccountService {
	return &AccountService{db: db}
}

// Create regista uma conta global e associa-a ao tenant atual com role 'associate'.
func (s AccountService) Create(q npadmin.Queryable, account *npadmin.Account) (*crypto.Token, error) {
	// 1. Obter o tenant_id do contexto RLS (definido pelo middleware)
	var tenantID int64
	if err := q.QueryRow("SELECT current_setting('myapp.tenant_id', true)::bigint").Scan(&tenantID); err != nil {
		return nil, fmt.Errorf("failed to retrieve current tenant ID from RLS session: %w", err)
	}

	// 2. Definir Queries (accounts já não tem tenant_id)
	accountQuery := `
		INSERT INTO accounts (email, status_id)
		VALUES ($1, $2)
		ON CONFLICT (email) DO NOTHING
		RETURNING id, (SELECT name FROM account_status WHERE id = $2)`

	passwordQuery := `
		INSERT INTO account_password_hashes (id, hash)
		VALUES ($1, $2)`

	profileQuery := `INSERT INTO profiles (id, first_name, last_name, nif, tlm, category_id, tenant_id) -- Role removido de profiles
	VALUES ($1, $2, $3, $4, $5, $6, $7)` // Removido Role, TenantID é o 8º param -> $7

	membershipQuery := `INSERT INTO account_tenant_memberships (account_id, tenant_id, role)
	VALUES ($1, $2, $3)`

	token, err := crypto.GenerateVerificationToken(3 * 24 * time.Hour)
	if err != nil {
		return nil, fmt.Errorf("failed to generate verification token: %w", err)
	}

	// 3. Inserir na tabela global 'accounts'
	argsAccount := []any{strings.ToLower(account.Email), account.StatusID}
	errScan := q.QueryRow(accountQuery, argsAccount...).Scan(&account.ID, &account.StatusDescription)
	accountExists := false
	if errScan != nil {
		if errScan == sql.ErrNoRows {
			accountExists = true
			errGet := q.Get(&account.ID, "SELECT id FROM accounts WHERE email = $1", strings.ToLower(account.Email))
			if errGet != nil {
				return nil, fmt.Errorf("failed to retrieve existing account ID after conflict: %w", errGet)
			}
			errGetStatus := q.Get(&account.StatusDescription, "SELECT name FROM account_status WHERE id = (SELECT status_id FROM accounts WHERE id = $1)", account.ID)
			if errGetStatus != nil {
				fmt.Printf("Warning: could not get status description for existing account %d: %v\n", account.ID, errGetStatus)
			}
		} else {
			return nil, fmt.Errorf("failed to add new account entry: %w", errScan)
		}
	}

	// 4. Inserir Hash da Password (só se a conta foi criada agora)
	if !accountExists {
		argsPassword := []any{account.ID, account.Password.Hash}
		if _, err := q.Exec(passwordQuery, argsPassword...); err != nil {
			return nil, fmt.Errorf("failed to add new password entry: %w", err)
		}
	}

	// 5. Validar e Inserir Profile (específico do tenant)
	if account.TLM.String == "" {
		account.TLM = npadmin.NullString{}
	}
	if _, err := strconv.Atoi(account.NIF.String); err != nil {
		return nil, fmt.Errorf("NIF should be number: %w", err)
	}
	if account.TLM.Valid {
		if _, err := strconv.Atoi(account.TLM.String); err != nil {
			return nil, fmt.Errorf("TLM should be number: %w", err)
		}
	}
	// Usar tenantID obtido no início
	argsProfile := []any{account.ID, account.FirstName, account.LastName, account.NIF, account.TLM, account.Category, tenantID}
	_, errProfile := q.Exec(profileQuery, argsProfile...)
	if errProfile != nil {
		return nil, fmt.Errorf("failed to add profile entry for tenant %d: %w", tenantID, errProfile)
	}

	// 6. Inserir na tabela de Memberships
	argsMembership := []any{account.ID, tenantID, "associate"} // Role fixo
	_, errMembership := q.Exec(membershipQuery, argsMembership...)
	if errMembership != nil {
		// Se conta já existia, pode dar erro de UNIQUE constraint aqui. Tratar.
		// Verificar se o erro é pq: duplicate key value violates unique constraint "account_tenant_memberships_account_id_tenant_id_key"
		if strings.Contains(errMembership.Error(), "account_tenant_memberships_account_id_tenant_id_key") {
			// Utilizador já era membro deste tenant, não retornar erro fatal, talvez logar?
			fmt.Printf("Info: Account %d is already a member of tenant %d.\n", account.ID, tenantID)
		} else {
			return nil, fmt.Errorf("failed to add account membership entry for tenant %d: %w", tenantID, errMembership)
		}
	}

	// 7. Criar Hash de Verificação de Email (associado ao ID da conta global)
	verificationToken := npadmin.VerificationToken{
		ID:       account.ID,
		Hash:     crypto.HexEncode(token.Hash),
		Deadline: token.Deadline,
		TenantID: npadmin.NewNullInt64(tenantID), // Associar ao tenant onde o registo ocorreu
	}
	if err := s.AccountConfirmationService.CreateEmailVerificationHash(&verificationToken, npadmin.WithTx(q)); err != nil { //
		return nil, fmt.Errorf("failed to create email verification hash: %w", err)
	}

	return token, nil
}


// Get busca dados da conta global pelo ID.
func (s AccountService) Get(q npadmin.Queryable, id int64) (*npadmin.Account, error) {
	query := `
		SELECT accounts.id, accounts.email, accounts.status_id, accounts.created_at, accounts.updated_at, account_status.name as status
		FROM accounts
		JOIN account_status ON account_status.id = accounts.status_id
		WHERE accounts.id = $1`

	var account npadmin.Account
	if err := q.Get(&account, query, id); err != nil {
		return nil, fmt.Errorf("failed to fetch account: %w", err)
	}
	return &account, nil
}

// GetByEmail busca dados da conta global pelo email.
func (s AccountService) GetByEmail(q npadmin.Queryable, email string) (*npadmin.Account, error) {
	var account npadmin.Account
	query := `
		SELECT accounts.id, accounts.email, accounts.status_id, accounts.created_at, accounts.updated_at, account_status.name as status FROM accounts
		JOIN account_status ON accounts.status_id = account_status.id
		WHERE email = $1`

	if err := q.Get(&account, query, strings.ToLower(email)); err != nil {
		return nil, fmt.Errorf("failed to fetch account by email: %w", err)
	}
	return &account, nil
}

// Update atualiza o status da conta global.
func (s AccountService) Update(q npadmin.Queryable, account *npadmin.Account) error {
	query := `
		UPDATE accounts SET status_id = $1, updated_at = now() at time zone 'utc' WHERE id = $2
		RETURNING (SELECT name FROM account_status WHERE id = $1)`
	args := []any{account.StatusID, account.ID}
	if err := q.QueryRow(query, args...).Scan(&account.StatusDescription); err != nil {
		return fmt.Errorf("failed to update account status: %w", err)
	}
	return nil
}

// GetUnverifiedByEmail busca contas globais pendentes por email.
func (s AccountService) GetUnverifiedByEmail(q npadmin.Queryable, email string) (*npadmin.Account, error) {
	var account npadmin.Account
	query := `
		SELECT accounts.id, accounts.email, accounts.status_id, accounts.created_at, accounts.updated_at,
		(SELECT name FROM account_status WHERE id = $2) as status
		FROM accounts
		WHERE email = $1 AND status_id = $2`
	args := []any{strings.ToLower(email), npadmin.AccountPending}
	if err := q.Get(&account, query, args...); err != nil {
		return nil, fmt.Errorf("failed to fetch unverified accounts by email: %w", err)
	}
	return &account, nil
}

// GetByJwtID busca conta global associada a um refresh token ID.
func (s AccountService) GetByJwtID(q npadmin.Queryable, uid string) (*npadmin.Account, error) {
	query := `
		SELECT acct.id, acct.email, acct.status_id, acct.created_at, acct.updated_at, status.name as status
		FROM accounts acct
		JOIN account_jwt_refresh_keys ajrk ON acct.id = ajrk.account_id
		JOIN account_status status ON acct.status_id = status.id
		WHERE ajrk.id = $1
		AND acct.status_id = $2` // Assume só users verificados têm refresh tokens
	var account npadmin.Account
	args := []any{uid, npadmin.AccountVerified}
	if err := q.Get(&account, query, args...); err != nil {
		return nil, fmt.Errorf("failed to fetch account by JWT refresh key ID: %w", err)
	}
	return &account, nil
}

// ExecutePasswordReset opera na tabela global account_password_hashes.
func (s AccountService) ExecutePasswordReset(q npadmin.Queryable, account *npadmin.Account, password crypto.Password) error {
	if err := s.ResetPasswordService.Update(q, account.ID, password); err != nil { //
		return fmt.Errorf("failed to reset password: %w", err)
	}
	return nil
}

// InitiateAccountConfirmation opera na conta global pendente, associa hash ao tenant atual.
func (s AccountService) InitiateAccountConfirmation(q npadmin.Queryable, email string) (*npadmin.Account, *crypto.Token, error) {
	// Obter tenant_id do contexto RLS para associar ao hash
	var tenantID int64
	if err := q.QueryRow("SELECT current_setting('myapp.tenant_id', true)::bigint").Scan(&tenantID); err != nil {
		return nil, nil, fmt.Errorf("failed to retrieve current tenant ID from RLS session for InitiateAccountConfirmation: %w", err)
	}

	account, err := s.GetUnverifiedByEmail(q, email) // Busca global
	if err != nil {
		return nil, nil, err
	}

	token, err := crypto.GenerateVerificationToken(3 * 24 * time.Hour)
	if err != nil {
		return nil, nil, fmt.Errorf("failed to generate verification token")
	}

	// Associar hash ao tenantID atual
	verificationToken := npadmin.VerificationToken{
		ID:       account.ID,
		Hash:     crypto.HexEncode(token.Hash),
		Deadline: token.Deadline,
		TenantID: npadmin.NewNullInt64(tenantID), // Associar ao tenant do contexto atual
	}

	if err := s.AccountConfirmationService.CreateEmailVerificationHash(&verificationToken, npadmin.WithTx(q)); err != nil { //
		return nil, nil, err
	}

	return account, token, nil
}


// ExecuteAccountConfirmation verifica token, ativa conta global, retorna ID global e tenantID original.
func (s AccountService) ExecuteAccountConfirmation(q npadmin.Queryable, token string) (*npadmin.Account, int64, error) {
	accountID, tenantID, err := s.AccountConfirmationService.Verify(q, token) //
	if err != nil {
		return nil, 0, fmt.Errorf("account confirmation verification failed: %w", err)
	}
	if tenantID == 0 { // Verify retorna erro se tenantID for inválido agora
		// Este check pode ser redundante se Verify sempre retornar erro com tenantID inválido
		return nil, 0, fmt.Errorf("tenant ID could not be determined during account confirmation for account %d", accountID)
	}

	account, err := s.Get(q, accountID) // Busca conta global atualizada
	if err != nil {
		return nil, 0, fmt.Errorf("failed to retrieve account details after confirmation: %w", err)
	}
	return account, tenantID, nil
}

// CreateEmailVerificationHash usa o serviço subjacente.
func (s AccountService) CreateEmailVerificationHash(token *npadmin.VerificationToken, opts ...func(*npadmin.Transactional)) error {
	if err := s.AccountConfirmationService.CreateEmailVerificationHash(token, opts...); err != nil { //
		return fmt.Errorf("failed to create account verification hash: %w", err)
	}
	return nil
}

// InitiateEmailUpdate opera na conta global, verifica password global.
func (s AccountService) InitiateEmailUpdate(q npadmin.Queryable, id int64, params npadmin.ResetEmailParams) (*crypto.Token, error) {
	var hash string
	account, err := s.Get(q, id) // Busca global
	if err != nil {
		return nil, fmt.Errorf("failed to fetch account to initiate email update: %w", err)
	}
	query := `SELECT hash FROM account_password_hashes WHERE id = $1`
	if err := q.Get(&hash, query, id); err != nil {
		return nil, fmt.Errorf("failed to fetch password hash for email update: %w", err)
	}
	password := crypto.Password{Plaintext: params.Password, Hash: []byte(hash)}
	if ok, _ := password.Matches(); !ok {
		return nil, errors.New("passwords do not match during email update initiation")
	}
	token, err := crypto.GenerateVerificationToken(24 * time.Hour)
	if err != nil {
		return nil, fmt.Errorf("failed to generate verification token for email update: %w", err)
	}
	// Passar q para CreateEmailKey (assume que UpdateEmailService foi ajustado para aceitar q)
	err = s.UpdateEmailService.CreateEmailKey(q, account.ID, params.NewEmail, token.Hash, token.Deadline)
	if err != nil {
		return nil, fmt.Errorf("failed to create email update key: %w", err)
	}
	return token, nil
}

// ValidateEmailUpdate usa o serviço subjacente (opera globalmente).
func (s AccountService) ValidateEmailUpdate(q npadmin.Queryable, token string) error {
	if _, err := s.UpdateEmailService.VerifyEmail(q, token); err != nil { //
		return fmt.Errorf("failed to verify email update token: %w", err)
	}
	return nil
}

// ExecuteEmailUpdate verifica token, atualiza email global, destrói token.
func (s AccountService) ExecuteEmailUpdate(q npadmin.Queryable, account *npadmin.Account, params npadmin.TokenParams) error {
	verificationEmailToken, err := s.UpdateEmailService.VerifyEmail(q, params.Token) //
	if err != nil {
		return fmt.Errorf("failed to fetch/verify email verification token during execution: %w", err)
	}
	if verificationEmailToken.ID != account.ID {
		return errors.New("email verification token mismatch with current account")
	}
	// Passar q para UpdateEmail (assume que UpdateEmailService foi ajustado)
	if err := s.UpdateEmailService.UpdateEmail(verificationEmailToken.Hash, verificationEmailToken.Email, npadmin.WithTx(q)); err != nil {
		return fmt.Errorf("failed to update email in accounts table: %w", err)
	}
	// Passar q para DestroyEmail (assume que UpdateEmailService foi ajustado)
	if err := s.UpdateEmailService.DestroyEmail(verificationEmailToken.ID, npadmin.WithTx(q)); err != nil {
		fmt.Printf("Warning: failed to remove email verification token after update for account %d: %v\n", verificationEmailToken.ID, err)
	}
	return nil
}

// GetCategory busca categoria DENTRO do tenant atual.
// Se não existir, CRIA-A AUTOMATICAMENTE usando uma função segura (Security Definer).
func (s AccountService) GetCategory(q npadmin.Queryable, name string) (int64, error) {
	var id int64
	
	// 1. Tentar encontrar a categoria existente (Standard RLS check)
	query := `SELECT id FROM associate_category WHERE name = $1`
	err := q.Get(&id, query, name)
	
	if err == nil {
		return id, nil
	}

	if err != sql.ErrNoRows {
		return 0, fmt.Errorf("failed to fetch category id by name: %w", err)
	}

	// 2. Se não encontrou, vamos CRIAR automaticamente via Função Segura.
	
	// Precisamos do Tenant ID da sessão atual
	var tenantID int64
	if err := q.QueryRow("SELECT current_setting('myapp.tenant_id', true)::bigint").Scan(&tenantID); err != nil {
		return 0, fmt.Errorf("failed to retrieve tenant ID for category creation: %w", err)
	}

	// CHAMADA À FUNÇÃO DE SEGURANÇA
	// Isto executa na DB com privilégios elevados, contornando o erro de RLS.
	insertQuery := `SELECT create_associate_category_safe($1, $2)`
	
	err = q.QueryRow(insertQuery, name, tenantID).Scan(&id)
	if err != nil {
		return 0, fmt.Errorf("failed to auto-create category '%s' via safe function: %w", name, err)
	}

	return id, nil
}

// GetCategoryByID busca categoria DENTRO do tenant atual (RLS).
func (s AccountService) GetCategoryByID(q npadmin.Queryable, ID int64) (string, error) {
	var name string
	query := `SELECT name FROM associate_category WHERE id = $1` // RLS aplicada
	if err := q.Get(&name, query, ID); err != nil {
		return "", fmt.Errorf("failed to fetch category name by id (in current tenant context): %w", err)
	}
	return name, nil
}


// --- NOVAS IMPLEMENTAÇÕES ---

// ExecuteAccountDisable atualiza status da conta GLOBAL.
// A permissão para chamar isto deve ser verificada no handler (AdminCtx).
func (s AccountService) ExecuteAccountDisable(q npadmin.Queryable, email npadmin.AssociateID) error {
	// Buscar conta global por email
	account, err := s.GetByEmail(q, email.Email) // Busca globalmente
	if err != nil {
		return fmt.Errorf("failed to get global user id by email '%s' in account disable: %w", email.Email, err)
	}

	// Atualizar status na tabela global 'accounts'
	account.StatusID = npadmin.AccountDisabled
	errUpdate := s.Update(q, account) // Usa o método Update já existente
	if errUpdate != nil {
		return fmt.Errorf("failed to disable global account status for id %d: %w", account.ID, errUpdate)
	}
	return nil
}

// ExecuteAccountVerify atualiza status da conta GLOBAL.
// A permissão deve ser verificada no handler (AdminCtx).
func (s AccountService) ExecuteAccountVerify(q npadmin.Queryable, email npadmin.AssociateID) error {
	// Buscar conta global por email
	account, err := s.GetByEmail(q, email.Email) // Busca globalmente
	if err != nil {
		return fmt.Errorf("failed to get global user id by email '%s' in account verify: %w", email.Email, err)
	}

	// Atualizar status na tabela global 'accounts'
	account.StatusID = npadmin.AccountVerified
	errUpdate := s.Update(q, account) // Usa o método Update já existente
	if errUpdate != nil {
		return fmt.Errorf("failed to manually verify global account status for id %d: %w", account.ID, errUpdate)
	}
	return nil
}