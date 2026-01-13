package npadmin

import (
	"time"

	"github.com/invisiblelab-dev/npadmin/crypto"
)

// AccountStatus represents the status of an account.
type AccountStatus int8

// Defines possible account statuses.
const (
	AccountPending AccountStatus = iota + 1
	AccountVerified
	AccountDisabled
)

// String returns the string representation of an AccountStatus.
func (a AccountStatus) String() string {
	switch a {
	case AccountPending:
		return "Pending"
	case AccountVerified:
		return "Verified"
	case AccountDisabled:
		return "Disabled"
	default:
		return "Unknown" // Corrected spelling
	}
}

// AccountStatusError is an error type representing an account status.
type AccountStatusError struct {
	Status AccountStatus
}

// Error returns the string representation of the AccountStatusError.
func (e AccountStatusError) Error() string {
	return e.Status.String()
}

// Account represents a user account in the system (NOW GLOBAL).
type Account struct {
	ID                int64           `db:"id" json:"-"`
	Email             string          `db:"email" json:"email"`
	StatusID          AccountStatus   `db:"status_id" json:"-"`
	CreatedAt         time.Time       `db:"created_at" json:"-"`
	UpdatedAt         time.Time       `db:"updated_at" json:"-"`
	StatusDescription string          `db:"status" json:"status"`
	Password          crypto.Password `json:"-"`
	FirstName         NullString      `json:"-"` // Note: These might move to Profile if truly tenant-specific
	LastName          NullString      `json:"-"` // Note: These might move to Profile if truly tenant-specific
	NIF               NullString      `json:"-"` // Note: These might move to Profile if truly tenant-specific
	TLM               NullString      `json:"-"` // Note: These might move to Profile if truly tenant-specific
	Role              string          `json:"-"` // Note: This Role might become global or move entirely to Membership
	Category          int64           `json:"-"` // Note: This might move to Profile if truly tenant-specific
	// TenantID          int64           `db:"tenant_id" json:"-"` // <-- REMOVED THIS LINE
}

// AccountService defines the interface for account-related operations.
type AccountService interface {
	// Account Registration
	Create(Queryable, *Account) (*crypto.Token, error)
	Update(Queryable, *Account) error // Updates global account data (e.g., status)
	Get(Queryable, int64) (*Account, error)
	GetByEmail(Queryable, string) (*Account, error)
	GetUnverifiedByEmail(Queryable, string) (*Account, error)
	GetByJwtID(Queryable, string) (*Account, error)
	GetCategory(Queryable, string) (int64, error)
	GetCategoryByID(Queryable, int64) (string, error)
	// Account Confirmation
	InitiateAccountConfirmation(Queryable, string) (*Account, *crypto.Token, error)
	ExecuteAccountConfirmation(Queryable, string) (*Account, int64, error)
	CreateEmailVerificationHash(*VerificationToken, ...func(*Transactional)) error
	// Account Reset
	ExecutePasswordReset(Queryable, *Account, crypto.Password) error
	// Email Update
	InitiateEmailUpdate(Queryable, int64, ResetEmailParams) (*crypto.Token, error)
	ValidateEmailUpdate(Queryable, string) error
	ExecuteEmailUpdate(Queryable, *Account, TokenParams) error

	// Admin Actions on Global Account Status (Moved from DataService)
	ExecuteAccountDisable(Queryable, AssociateID) error // Uses AssociateID from data.go
	ExecuteAccountVerify(Queryable, AssociateID) error  // Uses AssociateID from data.go
}

type ResetPasswordService interface {
	Update(Queryable, int64, crypto.Password) error
}

type AccountConfirmationService interface {
	CreateEmailVerificationHash(*VerificationToken, ...func(*Transactional)) error
	Verify(Queryable, string) (accountID int64, tenantID int64, err error)
	GetTokenByHash(Queryable, string) (*VerificationToken, error)
	GetTokenByID(Queryable, int64) (*VerificationToken, error)
}

type UpdateEmailService interface {
	CreateEmailKey(Queryable, int64, string, []byte, time.Time) error
	VerifyEmail(Queryable, string) (*VerificationEmailToken, error)
	GetEmailTokenByHash(Queryable, string) (*VerificationEmailToken, error)
	UpdateEmail(hash string, email string, opts ...func(*Transactional)) error
	DestroyEmail(id int64, opts ...func(*Transactional)) error
}

type AccountParams struct {
	Email     string          `validate:"required,email,email-uniqueness"`
	FirstName NullString      `validate:"required"`
	LastName  NullString      `validate:"required"`
	Category  string          `validate:"required"`
	NIF       NullString      `validate:"required,len=9"`
	TLM       NullString      `validate:"len=9|len=0"`
	Password  crypto.Password `validate:"required,min=8,has-numbers"`
}

type RecoverEmailParams struct {
	Email string `validate:"required,email"`
}

type ResetPasswordParams struct {
	Password             crypto.Password `validate:"required,min=8,has-symbols,has-numbers,eqfield=PasswordConfirmation"`
	PasswordConfirmation string          `validate:"required,min=8,has-symbols,has-numbers"`
}

type AccountView struct {
	Account  *Account
	Token    *crypto.Token
	Endpoint string
	Date     string
}

type ResetEmailParams struct {
	NewEmail        string `validate:"required,email,email-uniqueness,eqfield=NewEmailConfirm"`
	NewEmailConfirm string `validate:"required,email"`
	Password        string `validate:"required,min=8,has-numbers"`
}

type AccountCard struct {
	AssociateNumber int64
	Name            string
	Email           string
	Category        string
	QRCode          string
}
