package npadmin

import "time"

type SubscriptionStatus int8

const (
	SubscriptionExpired SubscriptionStatus = iota + 1
	SubscriptionValid
	SubscriptionOther
)

func (s SubscriptionStatus) String() string {
	switch s {
	case SubscriptionExpired:
		return "Expired"
	case SubscriptionValid:
		return "Valid"
	case SubscriptionOther:
		return "Other"
	default:
		return "Unknonwn"
	}
}

type Subscription struct {
	ID            int64      `db:"id" json:"id"`
	AccountID     string     `db:"account_id" json:"account_id"`
	Price         float64    `db:"price" json:"price"`
	StartDate     time.Time  `db:"start_date" json:"start_date"`
	EndDate       time.Time  `db:"end_date" json:"end_date"`
	CreatedAt     time.Time  `db:"created_at" json:"created_at"`
	UpdatedAt     time.Time  `db:"updated_at" json:"updated_at"`
	Invoice       NullString `db:"invoice_file" json:"invoice_file"`
	WarningSentAt *time.Time `db:"warning_sent_at" json:"warning_sent_at"`
}