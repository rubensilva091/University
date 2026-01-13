package npadmin

import (
	"database/sql"
	// "time" // REMOVED - Ensure time is imported if not already -> Already removed/managed
)

// DataService interface - REMOVED methods related to account status change
type DataService interface {
	// Added Queryable where DB interaction occurs
	AssociatesHistory(Queryable) ([]AssociatesPerMonth, error)
	SubscriptionsSituation(Queryable) (int64, int64, error)
	MonthlyIncome(Queryable) ([]MonthIncome, error)
	HistoricSubs(Queryable) (int64, int64, error)
	TotalAssociates(Queryable) (int64, error)

	FilteredAssociates(q Queryable, filter *FilterData) ([]AssociateSummary, int64, error)
	AssociateCompleteData(Queryable, AssociateFilter) (AssociateData, error) // Assumes GetProfileByEmail & SubscriptionHistory need Queryable
	GetProfileByEmail(Queryable, string) (*Profile, error)                   // Specific profile within tenant context
	CurrentSubscription(Queryable, int64) (*Subscription, error)
	SubscriptionHistory(Queryable, int64, Pagination) ([]Subscription, error)

	// ExecuteAccountDisable(Queryable, AssociateID) error // <-- REMOVIDO
	// ExecuteAccountVerify(Queryable, AssociateID) error  // <-- REMOVIDO
	// GetByEmail(Queryable, string) (*Account, error)     // <-- REMOVIDO (Pertence a AccountService)

	UploadInvoice(Queryable, string, int64, int64) error

	GetPrice(Queryable, int64, int64) (float64, error)
	GetPrices(Queryable, int64) ([]Prices, error)
	AllPrices(Queryable) ([]Prices, error)
	InsertPrice(Queryable, int64, int, float64) error
	DeletePrice(Queryable, int64, int) (bool, error)
	GetPeriods(Queryable) ([]int64, error)
	GetCategories(Queryable) ([]Categories, error)
}

// ... rest of the structs remain the same ...

type AssociatesPerMonth struct {
	Month            sql.NullTime `db:"month" json:"month"`
	NumberAssociates int          `db:"number_associates" json:"number_associates"`
}

// AssociateSummary - Needs MembershipRole field
type AssociateSummary struct {
	ID                 int          `db:"id" json:"id"`                           // Global Account ID
	Email              string       `db:"email" json:"email"`                       // Global Email
	Status             int          `db:"status_id" json:"status_id"`             // Global Status ID
	NIF                NullString   `db:"nif" json:"nif"`                           // From Profile (Tenant)
	Name               NullString   `db:"first_name" json:"first_name"`             // From Profile (Tenant)
	LastName           NullString   `db:"last_name" json:"last_name"`             // From Profile (Tenant)
	MembershipRole     string       `db:"membership_role" json:"role"`            // Role from Membership (Tenant) <-- ALTERADO/ADICIONADO
	// Role               string       `db:"role" json:"role"` // REMOVIDO ou ignorar se ainda vier da query antiga
	SubscriptionStatus string       `json:"subscription_status"`                  // Calculated (Tenant)
	SubsEndDate        sql.NullTime `db:"subscription_end_date" json:"subscription_end_date"` // From Subscription (Tenant)
}


type FilterData struct {
	Name      string             `json:"name"`
	Status    AccountStatus      `json:"status"`    // Global account status
	SubStatus SubscriptionStatus `json:"subStatus"` // Tenant subscription status
	Pagination
}

type AssociateID struct {
	Email string `json:"email" validate:"required,email"`
}

type AssociateFilter struct {
	AssociateID // Embeds Email
	Pagination
}

type AssociateData struct {
	Profile       Profile        `json:"profile"`       // Profile within tenant
	Subscriptions []Subscription `json:"subscriptions"` // Subscriptions within tenant
}

type Pagination struct {
	Page     int `json:"page" schema:"page"`
	PageSize int `json:"pageSize" schema:"pageSize"`
}

type Categories struct {
	Name        string     `db:"name" json:"name"`
	Description NullString `db:"description" json:"description"`
}

type Prices struct {
	Period   int     `db:"period" json:"period"`
	Price    float64 `db:"price" json:"price"`
	Category string  `db:"category" json:"category"` // Category Name
}


type AddPrice struct {
	Prices             // Embed Prices
	Category string `json:"category" validate:"required"` // Category Name
}

type DeletePrice struct {
	Category string `json:"category" validate:"required"` // Category Name
	Period   int    `json:"period" validate:"required"`
}

type AddInvoice struct {
	ID             int64  `json:"id" validate:"required"`              // Account ID Global
	SubscriptionID int64  `json:"subscriptionId" validate:"required"` // Subscription History ID (Tenant)
	InvoiceURL     string `json:"invoiceUrl" validate:"required,url"`
}

type AddSubscriptionPayment struct {
	AccountID int64   `json:"accountId" validate:"required"` // Account ID Global
	Period    int     `json:"period" validate:"required"`
	StartDate string  `json:"startDate" validate:"required"` // Formato YYYY-MM-DD
	Price     float64 `json:"price" validate:"required,gt=0"`
}

type MonthIncome struct {
	Income float64      `db:"income" json:"income"`
	Date   sql.NullTime `db:"month" json:"month"`
}