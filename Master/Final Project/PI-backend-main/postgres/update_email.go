package postgres

import (
	"fmt"
	"time"

	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
)

type UpdateEmailService struct {
	db *DB
}

func NewUpdateEmailService(db *DB) *UpdateEmailService {
	return &UpdateEmailService{db: db}
}

// CHANGED: Added 'q npadmin.Queryable'
func (s UpdateEmailService) CreateEmailKey(q npadmin.Queryable, id int64, email string, hash []byte, deadline time.Time) error {
	query := `
		INSERT INTO account_email_reset_hashes VALUES ($1, $2, $3, $4)
		ON CONFLICT (id) DO
		UPDATE SET
		id = EXCLUDED.id,
		hash = EXCLUDED.hash,
		email = EXCLUDED.email,
		deadline = EXCLUDED.deadline,
		updated_at = now() at time zone 'utc'`
	args := []any{id, crypto.HexEncode(hash), email, deadline}
	// CHANGED: use 'q' instead of 's.db'
	if _, err := q.Exec(query, args...); err != nil {
		return fmt.Errorf("failed to create email reset hash: %w", err)
	}

	return nil
}

// CHANGED: Added 'q npadmin.Queryable'
func (s UpdateEmailService) VerifyEmail(q npadmin.Queryable, value string) (*npadmin.VerificationEmailToken, error) {
	hash := crypto.Sha256Checksum(value)

	// CHANGED: Pass 'q' to GetEmailTokenByHash
	token, err := s.GetEmailTokenByHash(q, hash)
	if err != nil {
		return nil, err
	}

	return token, nil
}

// CHANGED: Added 'q npadmin.Queryable'
func (s UpdateEmailService) GetEmailTokenByHash(q npadmin.Queryable, value string) (*npadmin.VerificationEmailToken, error) {
	var token npadmin.VerificationEmailToken

	query := `
		SELECT *
		FROM account_email_reset_hashes
		WHERE hash = $1 AND deadline > (now() at time zone 'utc')`

	// CHANGED: use 'q' instead of 's.db'
	if err := q.Get(&token, query, value); err != nil {
		return nil, fmt.Errorf("failed to fetch email token by hash: %w", err)
	}

	return &token, nil
}

func (s UpdateEmailService) DestroyEmail(id int64, opts ...func(*npadmin.Transactional)) error {
	query := `DELETE FROM account_email_reset_hashes WHERE id = $1`
	q := npadmin.GetQueriable(s.db, opts...)

	if _, err := q.Exec(query, id); err != nil {
		return fmt.Errorf("failed to remove email reset hash: %w", err)
	}

	return nil
}

func (s UpdateEmailService) UpdateEmail(hash string, email string, opts ...func(*npadmin.Transactional)) error {
	query := `
			UPDATE accounts
			SET email = $1,
			updated_at = now() at time zone 'utc'
			WHERE id = $2`

	q := npadmin.GetQueriable(s.db, opts...)

	// CHANGED: Pass 'q' to GetEmailTokenByHash
	verificationToken, err := s.GetEmailTokenByHash(q, hash)
	if err != nil {
		return err
	}

	if _, err := q.Exec(query, email, verificationToken.ID); err != nil {
		return fmt.Errorf("failed to update email: %w", err)
	}

	return nil
}