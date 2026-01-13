package npadmin

import (
	"time"

	"github.com/invisiblelab-dev/npadmin/crypto"
)

// AuthenticationService interface remains the same
type AuthenticationService interface {
	CreatePasswordLogin(Queryable, AuthenticationParams) (*Account, error)
	CreateEmailLogin(Queryable, EmailAuthenticationParams) (*Account, *crypto.Token, error)
	ExecuteEmailLoginConfirmation(Queryable, string) (*Account, error)
	Destroy() error
	// <<< ADICIONE ESTA LINHA >>>
	CheckTenantMembership(Queryable, int64, int64) error 
}

// AuthenticationConfirmationService interface signature corrected
type AuthenticationConfirmationService interface {
	CreateEmailLoginHash(Queryable, int64, []byte, time.Time) error
	// --- CORRECTED Verify SIGNATURE HERE ---
	Verify(Queryable, string) (accountID int64, tenantID int64, err error) // Now returns tenantID
	// --- END CORRECTION ---
	GetTokenByHash(Queryable, string) (*VerificationToken, error)
}

// AuthenticationParams struct remains the same
type AuthenticationParams struct {
	Email    string `validate:"required"`
	Password string `validate:"required"`
}

// EmailAuthenticationParams struct remains the same
type EmailAuthenticationParams struct {
	Email string `validate:"required"`
}

// Authentication struct remains the same
type Authentication struct {
	Account
	Hash string
}

// AuthenticationView struct definition - Adjusted Account field type
type AuthenticationView struct {
	Account  *Account // Changed from *EmailAuthenticationParams to *Account
	Token    *crypto.Token
	Endpoint string
}