package npadmin

import "time"

type AuthorizationService interface {
	StoreRefreshToken(Queryable, string, int64, time.Time) error // Added Queryable
	RotateRefreshToken(Queryable, string, string, time.Time) (bool, error) // Added Queryable (assuming Rotate also needs it)
	GetRefreshToken(Queryable, string) (*RefreshToken, error) // Added Queryable (assuming Get also needs it)
}

type RefreshToken struct {
	ID        string    `db:"id"`
	AccountID int64     `db:"account_id"`
	Deadline  time.Time `db:"deadline"`
	CreatedAt time.Time `db:"created_at"`
}