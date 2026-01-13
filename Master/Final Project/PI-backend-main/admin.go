package npadmin

import (
	"time"
)

type AdminConfig struct {
	ID        bool      `db:"id" json:"id"`
	MBway     bool      `db:"mbway" json:"mbway"`
	MB        bool      `db:"mb" json:"mb"`
	CreatedAt time.Time `db:"created_at" json:"created_at"`
	UpdatedAt time.Time `db:"updated_at" json:"updated_at"`
}

type AdminService interface {
	UpdateConfig(*ConfigParams) (*AdminConfig, error)

	Get() (*AdminConfig, error)
}

type ConfigParams struct {
	MBway bool
	MB    bool
}
