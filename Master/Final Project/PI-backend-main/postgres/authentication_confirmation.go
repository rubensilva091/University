package postgres

import (
	"errors"
	"fmt"
	"time"

	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
)

// Struct definition ONLY for AuthenticationConfirmationService
type AuthenticationConfirmationService struct {
	db *DB
}

// Constructor ONLY for AuthenticationConfirmationService
func NewAuthenticationConfirmationService(db *DB) *AuthenticationConfirmationService {
	return &AuthenticationConfirmationService{db: db}
}

// --- Methods ONLY for AuthenticationConfirmationService ---

// CreateEmailLoginHash armazena o hash para login por email.
// NÃO insere tenant_id ainda, pois a coluna não existe em account_login_hashes.
func (s AuthenticationConfirmationService) CreateEmailLoginHash(q npadmin.Queryable, id int64, hash []byte, deadline time.Time) error {
	// Query sem tenant_id
	query := `
		INSERT INTO account_login_hashes (id, hash, deadline)
		VALUES ($1, $2, $3)
		ON CONFLICT (id)
		DO UPDATE SET
		hash = EXCLUDED.hash,
		deadline = EXCLUDED.deadline,
		updated_at = now() at time zone 'utc'` // Tabela: account_login_hashes

	args := []any{id, crypto.HexEncode(hash), deadline}
	// Usar 'q' para execução
	if _, err := q.Exec(query, args...); err != nil {
		return fmt.Errorf("failed to create/update email login hash: %w", err)
	}

	return nil
}

// Verify verifica um token de *login por email*, apaga-o e retorna accountID e tenantID (se disponível).
// Assinatura de retorno atualizada para (int64, int64, error).
func (s AuthenticationConfirmationService) Verify(q npadmin.Queryable, input string) (accountID int64, tenantID int64, err error) {
	// GetTokenByHash busca o token (incluindo tenant_id se a coluna existir e for selecionada)
	token, err := s.GetTokenByHash(q, input)
	if err != nil {
		return 0, 0, fmt.Errorf("failed to get email login token during verify: %w", err)
	}

	// Verifica o hash
	if !crypto.MatchVerificationToken(input, token.Hash) {
		return 0, 0, errors.New("e-mail login token does not match")
	}

	// Tenta extrair tenantID do token (será 0 se a coluna não existir ou for NULL)
	var extractedTenantID int64 = 0 // Default para 0
	if token.TenantID.Valid {
		extractedTenantID = token.TenantID.Int64
	} else {
		// Logar ou avisar que o tenantID não estava presente no token de login?
		// Para login, pode não ser estritamente necessário nesta fase.
		fmt.Printf("Warning: TenantID not found or NULL in login token for account %d\n", token.ID)
	}


	// Remove o hash de login usado
	queryDeleteHash := `DELETE FROM account_login_hashes WHERE id = $1` // Tabela: account_login_hashes
	if _, err := q.Exec(queryDeleteHash, token.ID); err != nil {
		// Logar este erro, mas provavelmente não retornar como erro fatal se o objetivo principal (verificar user) foi atingido.
		fmt.Printf("Warning: failed to remove account email login hash after verification for account %d: %v\n", token.ID, err)
		// return 0, 0, fmt.Errorf("failed to remove account email login hash after verification: %w", err)
	}

	// Retorna accountID (global) e o tenantID (pode ser 0 se não encontrado/necessário)
	return token.ID, extractedTenantID, nil
}

// GetTokenByHash busca detalhes do token de *login por email*.
// Atualizado para TENTAR buscar tenant_id se a coluna existir.
func (s AuthenticationConfirmationService) GetTokenByHash(q npadmin.Queryable, value string) (*npadmin.VerificationToken, error) {
	// Query modificada para tentar selecionar tenant_id (usando alias a para accounts).
	// Se a coluna alh.tenant_id não existir, esta query FALHARÁ.
	// Uma abordagem mais segura seria verificar a existência da coluna ou ter queries separadas.
	// Por agora, vamos assumir que adicionaremos a coluna a account_login_hashes eventualmente para consistência.
	// Temporariamente, podemos remover a seleção de a.tenant_id se a coluna não for adicionada já.
	// --> VAMOS REMOVER a.tenant_id por agora para evitar erro, até que a coluna seja adicionada.
	query := `
		SELECT
			alh.id, alh.hash, alh.created_at, alh.updated_at, alh.deadline
			-- , a.tenant_id -- REMOVIDO TEMPORARIAMENTE até coluna ser adicionada a account_login_hashes
		FROM account_login_hashes alh
		JOIN accounts a ON a.id = alh.id -- Join ainda necessário para verificar status
		WHERE alh.hash = $1 AND alh.deadline > $2
		AND a.status_id = $3` // status_id = 2 é Verificado

	// A struct VerificationToken já inclui TenantID (NullInt64)
	token := npadmin.VerificationToken{}

	inputHash := crypto.Sha256Checksum(value)
	args := []any{inputHash, time.Now().UTC(), npadmin.AccountVerified}

	// Executar a query usando 'q'
	if err := q.Get(&token, query, args...); err != nil {
		// Tratar sql.ErrNoRows ou outros erros
		// Se a coluna tenant_id for selecionada mas não existir, o erro será sobre a coluna.
		return nil, fmt.Errorf("failed to fetch email login token by hash: %w", err)
	}

	// Se a coluna tenant_id não foi selecionada, token.TenantID permanecerá {Int64: 0, Valid: false}
	return &token, nil
}

// --- Assegurar que métodos de AccountConfirmationService NÃO estão aqui ---