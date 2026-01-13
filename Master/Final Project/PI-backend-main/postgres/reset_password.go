package postgres

import (
	"fmt"

	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
)

type ResetPasswordService struct {
	db *DB
}

func NewResetPasswordService(db *DB) *ResetPasswordService {
	return &ResetPasswordService{db: db}
}

// CHANGED: Added 'q npadmin.Queryable' as the first argument, removed optional tx
func (s ResetPasswordService) Update(q npadmin.Queryable, accountID int64, password crypto.Password) error {
	query := `
			UPDATE account_password_hashes
			SET hash = $1,
			updated_at = now() at time zone 'utc'
			WHERE id = $2`

	// CHANGED: Use 'q' directly
	if _, err := q.Exec(query, password.Hash, accountID); err != nil {
		return fmt.Errorf("failed to update password reset hash: %w", err)
	}

	return nil
}