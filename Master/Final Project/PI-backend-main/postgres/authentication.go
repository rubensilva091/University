package postgres

import (
	"fmt"
	"time"
	"errors"

	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
)

type AuthenticationService struct {
	db                                *DB
	AuthenticationConfirmationService npadmin.AuthenticationConfirmationService // Interface
}

func NewAuthenticationService(db *DB) *AuthenticationService {
	return &AuthenticationService{db: db}
}

// CreatePasswordLogin verifica credenciais na tabela global 'accounts'.
func (s AuthenticationService) CreatePasswordLogin(q npadmin.Queryable, params npadmin.AuthenticationParams) (*npadmin.Account, error) {
	// A struct Authentication agora reflete a Account global (sem tenant_id) + Hash
	var authentication npadmin.Authentication
	// Query ajustada: seleciona da 'accounts' global, sem tenant_id
	query := `
		SELECT
			a.id, a.email, a.status_id, a.created_at, a.updated_at, -- Selecionar campos explícitos de 'accounts'
			aph.hash, -- Hash da password
			ast.name as status -- Nome do status
		FROM accounts a
		JOIN account_password_hashes aph ON a.id = aph.id
		JOIN account_status ast ON ast.id = a.status_id
		WHERE a.email = $1
		AND a.status_id = $2` // status_id = 2 é Verificado (npadmin.AccountVerified)

	// Usar 'q' para buscar
	// Usar Get para buscar numa struct Authentication que embute a struct Account (sem TenantID)
	if err := q.Get(&authentication, query, params.Email, npadmin.AccountVerified); err != nil {
		// Envolver erro para contexto
		return nil, fmt.Errorf("failed to fetch account for authentication: %w", err)
	}

	password := crypto.Password{
		Plaintext: params.Password,
		Hash:      []byte(authentication.Hash), // Hash vindo de account_password_hashes
	}

	// Verificar password
	if ok, _ := password.Matches(); !ok { // Ignorar erro detalhado de bcrypt
		// Manter erro genérico por segurança
		return nil, fmt.Errorf("password does not match")
	}

	// Retorna a struct Account global (sem TenantID) embutida em Authentication
	return &authentication.Account, nil
}

// CreateEmailLogin gera link de login para conta global.
func (s AuthenticationService) CreateEmailLogin(q npadmin.Queryable, params npadmin.EmailAuthenticationParams) (*npadmin.Account, *crypto.Token, error) {
	var authentication npadmin.Authentication // Embeds Account global
	// Query ajustada: seleciona da 'accounts' global, sem tenant_id
	query := `
		SELECT
			a.id, a.email, a.status_id, a.created_at, a.updated_at, -- Campos explícitos de 'accounts'
			aph.hash, -- Hash da password (necessário para a struct Authentication, embora não usado aqui)
			ast.name as status -- Nome do status
		FROM accounts a
		JOIN account_password_hashes aph ON a.id = aph.id -- Join ainda necessário para a struct
		JOIN account_status ast ON ast.id = a.status_id
		WHERE a.email = $1
		AND a.status_id = $2` // status_id = 2 é Verificado

	// Usar 'q' para buscar
	if err := q.Get(&authentication, query, params.Email, npadmin.AccountVerified); err != nil {
		return nil, nil, fmt.Errorf("failed to fetch account for email authentication: %w", err)
	}

	// Gerar token de verificação
	token, err := crypto.GenerateVerificationToken(5 * time.Minute) // TTL 5 minutos
	if err != nil {
		return nil, nil, fmt.Errorf("failed to generate verification token for email login: %w", err)
	}

	// Passar 'q' e account.ID global para o serviço de confirmação
	// O serviço de confirmação decide se armazena tenant_id com o hash (atualmente não)
	if err := s.AuthenticationConfirmationService.CreateEmailLoginHash(q, authentication.ID, token.Hash, token.Deadline); err != nil { //
		return nil, nil, fmt.Errorf("failed to create email login hash: %w", err)
	}

	// Retorna a Account global e o token
	return &authentication.Account, token, nil
}

// ExecuteEmailLoginConfirmation verifica token, retorna conta global.
func (s AuthenticationService) ExecuteEmailLoginConfirmation(q npadmin.Queryable, token string) (*npadmin.Account, error) {
	// Chamar Verify, que agora retorna (accountID, tenantID, err)
	// O tenantID aqui representa o contexto do tenant QUANDO o link foi gerado/verificado.
	// Pode não ser necessário nesta função específica, dependendo da lógica do chamador (HTTP handler).
	accountID, _, err := s.AuthenticationConfirmationService.Verify(q, token) // Descartar tenantID com _
	if err != nil {
		// Retornar erro se a verificação falhar
		return nil, fmt.Errorf("failed to confirm email authentication: %w", err)
	}

	// Buscar detalhes da conta global usando o accountID retornado
	// A query seleciona da tabela global 'accounts' e junta status
	// Query NÃO precisa mais juntar profiles aqui, a conta global é suficiente.
	query := `
		SELECT
			accounts.id, accounts.email, accounts.status_id, accounts.created_at, accounts.updated_at,
			account_status.name as status
		FROM accounts
		JOIN account_status ON account_status.id = accounts.status_id
		WHERE accounts.id = $1`

	var account npadmin.Account // Struct Account global (sem TenantID)
	// Usar 'q' para buscar
	if err := q.Get(&account, query, accountID); err != nil {
		// Erro se não conseguir buscar a conta após verificação bem-sucedida
		return nil, fmt.Errorf("failed to fetch account details after email login confirmation: %w", err)
	}

	// Retorna a conta global
	return &account, nil
}

// Destroy não parece interagir com DB, sem alterações necessárias para 'q'.
func (s AuthenticationService) Destroy() error {
	// Lógica de limpeza se necessário (e.g., fechar conexões mantidas pelo serviço, se houver)
	return nil
}

func (s AuthenticationService) CheckTenantMembership(q npadmin.Queryable, accountID int64, tenantID int64) error {
	var exists bool
	query := `
		SELECT EXISTS (
			SELECT 1 FROM account_tenant_memberships
			WHERE account_id = $1 AND tenant_id = $2
		)`

	if err := q.Get(&exists, query, accountID, tenantID); err != nil {
		// Se a query falhar (que não seja sql.ErrNoRows, que o EXISTS não deve devolver)
		return fmt.Errorf("failed to check tenant membership: %w", err)
	}

	if !exists {
		// Não encontrou, o que significa que não é membro.
		return errors.New("user is not a member of this tenant")
	}

	// Encontrou, o utilizador é membro.
	return nil
}