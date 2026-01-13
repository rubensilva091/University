package postgres

import (
	"errors"
	"fmt"
	"time"

	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
)

// Struct definition for AccountConfirmationService
type AccountConfirmationService struct {
	db *DB
}

// Constructor for AccountConfirmationService
func NewAccountConfirmationService(db *DB) *AccountConfirmationService {
	return &AccountConfirmationService{db: db}
}

// --- Methods ONLY for AccountConfirmationService ---

// CreateEmailVerificationHash armazena o hash para verificação de criação de conta, INCLUINDO tenant_id.
func (s AccountConfirmationService) CreateEmailVerificationHash(token *npadmin.VerificationToken, opts ...func(*npadmin.Transactional)) error {
	// Query agora inclui tenant_id
	query := `
		INSERT INTO account_verification_hashes (id, hash, deadline, tenant_id) -- Adicionado tenant_id
		VALUES ($1, $2, $3, $4) -- Adicionado placeholder $4
		ON CONFLICT (id)
		DO UPDATE SET
		hash = EXCLUDED.hash,
		deadline = EXCLUDED.deadline,
		tenant_id = EXCLUDED.tenant_id, -- Adicionado update para tenant_id
		updated_at = $5` // Placeholder $5 para now()

	q := npadmin.GetQueriable(s.db, opts...)
	// Argumentos agora incluem token.TenantID (que é NullInt64)
	args := []any{token.ID, token.Hash, token.Deadline, token.TenantID, time.Now().UTC()} // Adicionado token.TenantID
	if _, err := q.Exec(query, args...); err != nil {
		return fmt.Errorf("failed to create/update email verification hash with tenant_id: %w", err)
	}

	return nil
}

// Verify verifica um token de *confirmação de conta*, atualiza o status, apaga o hash e retorna IDs.
// A assinatura já retorna (accountID int64, tenantID int64, err error).
func (s AccountConfirmationService) Verify(q npadmin.Queryable, input string) (accountID int64, tenantID int64, err error) {
	// GetTokenByHash agora busca o tenant_id associado ao token
	token, err := s.GetTokenByHash(q, input)
	if err != nil {
		return 0, 0, fmt.Errorf("failed to get account confirmation hash during verify: %w", err)
	}

	// Verifica o hash do token
	if !crypto.MatchVerificationToken(input, token.Hash) {
		return 0, 0, errors.New("e-mail verification token does not match")
	}

	// Verifica se o TenantID obtido do token é válido
	if !token.TenantID.Valid {
		// Se o tenantID não for válido (NULL no DB), retorna erro pois é necessário para o fluxo
		return 0, 0, fmt.Errorf("tenant ID associated with the verification token is missing or invalid for account %d", token.ID)
	}
	extractedTenantID := token.TenantID.Int64 // Extrai o int64 válido

	// Atualiza o status da conta global para Verificado
	// Esta operação não precisa do tenantID, pois opera na conta global
	queryUpdateStatus := `UPDATE accounts SET status_id = $1, updated_at = now() at time zone 'utc' WHERE id = $2`
	if _, err := q.Exec(queryUpdateStatus, npadmin.AccountVerified, token.ID); err != nil {
		// Se falhar aqui, a conta não foi verificada
		return 0, 0, fmt.Errorf("failed to update account status to verified: %w", err)
	}

	// Remove o hash de verificação que foi usado
	queryDeleteHash := `DELETE FROM account_verification_hashes WHERE id = $1`
	if _, err := q.Exec(queryDeleteHash, token.ID); err != nil {
		// Logar este erro, mas talvez não retornar como erro fatal se o status já foi atualizado?
		// Por agora, retornar erro para indicar que a limpeza falhou.
		fmt.Printf("Warning: failed to remove account verification hash after confirmation for account %d: %v\n", token.ID, err)
		// return 0, 0, fmt.Errorf("failed to remove account verification hash after confirmation: %w", err)
	}

	// Retorna o ID da conta global e o ID do tenant associado ao token
	return token.ID, extractedTenantID, nil
}

// GetTokenByHash busca detalhes do token de *confirmação de conta*, incluindo tenant_id.
func (s AccountConfirmationService) GetTokenByHash(q npadmin.Queryable, value string) (*npadmin.VerificationToken, error) {
	// Query agora seleciona tenant_id da tabela de hashes
	// Não precisa mais fazer JOIN com accounts só para pegar o tenant_id
	query := `
		SELECT
			avh.id, avh.hash, avh.created_at, avh.updated_at, avh.deadline,
			avh.tenant_id -- Seleciona tenant_id diretamente de account_verification_hashes
		FROM account_verification_hashes avh
		-- JOIN accounts a ON a.id = avh.id -- JOIN não é mais necessário para tenant_id
		WHERE avh.hash = $1 AND avh.deadline > $2
		-- Ainda podemos querer filtrar pelo status da conta associada se necessário
		AND EXISTS (SELECT 1 FROM accounts a WHERE a.id = avh.id AND a.status_id = $3)` // Verifica se a conta associada está Pendente

	// A struct VerificationToken já inclui TenantID (NullInt64)
	token := npadmin.VerificationToken{}

	// Calcular hash do valor de entrada
	inputHash := crypto.Sha256Checksum(value)
	args := []any{inputHash, time.Now().UTC(), npadmin.AccountPending}

	// Executar a query usando 'q'
	if err := q.Get(&token, query, args...); err != nil {
		// Tratar sql.ErrNoRows ou outros erros
		return nil, fmt.Errorf("failed to fetch account confirmation token by hash: %w", err)
	}

	// O campo token.TenantID (tipo NullInt64) será preenchido pelo Get
	return &token, nil
}

// GetTokenByID busca detalhes do token de *confirmação de conta* por ID, incluindo tenant_id.
func (s AccountConfirmationService) GetTokenByID(q npadmin.Queryable, id int64) (*npadmin.VerificationToken, error) {
	// Query agora seleciona tenant_id da tabela de hashes
	query := `
		SELECT
			avh.id, avh.hash, avh.created_at, avh.updated_at, avh.deadline,
			avh.tenant_id -- Seleciona tenant_id diretamente
		FROM account_verification_hashes avh
		WHERE avh.id = $1 AND avh.deadline > $2
		AND EXISTS (SELECT 1 FROM accounts a WHERE a.id = avh.id AND a.status_id = $3)` // Verifica se a conta associada está Pendente

	// A struct VerificationToken já inclui TenantID (NullInt64)
	token := npadmin.VerificationToken{}
	args := []any{id, time.Now().UTC(), npadmin.AccountPending}

	// Executar a query usando 'q'
	if err := q.Get(&token, query, args...); err != nil {
		// Tratar sql.ErrNoRows ou outros erros
		return nil, fmt.Errorf("failed to fetch account confirmation token by id: %w", err)
	}

	// O campo token.TenantID (tipo NullInt64) será preenchido pelo Get
	return &token, nil
}

// --- Assegurar que métodos de AuthenticationConfirmationService NÃO estão aqui ---