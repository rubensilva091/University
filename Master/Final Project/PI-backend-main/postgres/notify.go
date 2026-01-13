package postgres

import (
	"fmt"

	"github.com/invisiblelab-dev/npadmin"
)

type NotifyService struct {
	db *DB
}

func NewNotifyService(db *DB) *NotifyService {
	return &NotifyService{db}
}

// CHANGED: Added 'q npadmin.Queryable'
func (s NotifyService) Create(q npadmin.Queryable, notification *npadmin.NotifyPayload) error {
	query := "INSERT INTO logs (entry) VALUES ($1)"
	// CHANGED: Use 'q'
	if _, err := q.Exec(query, notification.Payload); err != nil {
		return fmt.Errorf("failed to insert notification: %w", err)
	}

	return nil
}