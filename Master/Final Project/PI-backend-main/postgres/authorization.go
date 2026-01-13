package postgres

import (
	"fmt"
	"time"

	"github.com/invisiblelab-dev/npadmin"
)

type AuthorizationService struct {
	db *DB
}

func NewAuthorizationService(db *DB) *AuthorizationService {
	return &AuthorizationService{db: db}
}

// CHANGED: Added 'q npadmin.Queryable'
func (s AuthorizationService) StoreRefreshToken(q npadmin.Queryable, id string, account int64, deadline time.Time) error {
	query := `INSERT INTO account_jwt_refresh_keys VALUES ($1, $2, $3)`
	args := []any{id, account, deadline}

	// CHANGED: use 'q' instead of 's.db'
	if _, err := q.Exec(query, args...); err != nil {
		return fmt.Errorf("failed to store refresh token %w", err)
	}

	return nil
}

// CHANGED: Added 'q npadmin.Queryable'
func (s AuthorizationService) RotateRefreshToken(q npadmin.Queryable, oldID string, newID string, deadline time.Time) (bool, error) {
	// TODO: develop a strategy for invalidating all refresh tokens if previous tokens are used to refresh
	query := `UPDATE account_jwt_refresh_keys
		SET id = $1, deadline = $2
		WHERE id = $3`

	args := []any{newID, deadline, oldID}
	// CHANGED: use 'q' instead of 's.db'
	result, err := q.Exec(query, args...)
	if err != nil {
		return false, fmt.Errorf("failed to rotate refresh token: %w", err)
	}

	affected, err := result.RowsAffected()
	if err != nil {
		return false, fmt.Errorf("failed to fetch affected rows for refresh token update: %w", err)
	}

	return affected > 0, nil
}

// CHANGED: Added 'q npadmin.Queryable'
func (s AuthorizationService) GetRefreshToken(q npadmin.Queryable, id string) (*npadmin.RefreshToken, error) {
	var token npadmin.RefreshToken
	query := "SELECT * FROM account_jwt_refresh_keys WHERE id = $1"
	// CHANGED: use 'q' instead of 's.db'
	if err := q.Get(&token, query, id); err != nil {
		return nil, fmt.Errorf("failed to fetch refresh token: %w", err)
	}

	return &token, nil
}