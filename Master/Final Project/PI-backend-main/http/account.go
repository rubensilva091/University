package http

import (
	"database/sql"
	"errors"
	"fmt"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/jmoiron/sqlx"
	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
	"github.com/invisiblelab-dev/npadmin/http/middleware"
	// Importar o pacote de validação para usar as novas funções
	"github.com/invisiblelab-dev/npadmin/validation"
	"github.com/skip2/go-qrcode"
)

const (
	// RoleAssociate represents the associate role.
	RoleAssociate = "associate"
	// RoleAdmin represents the admin role.
	RoleAdmin = "admin"
)


func (s *Server) registerAccountRoutes(r chi.Router) {
	// Reset Pass (Grupo Protegido)
	r.Route("/accounts", func(r chi.Router) {
		r.Use(middleware.JwtVerifier(crypto.ParseAuthorizationHeader))
		r.Use(middleware.JwtAuthorize(s.cfg.JwtPublicKey))
		r.Use(middleware.TenantTxMiddleware(s.db))
		r.Use(middleware.AccountCtx(s.AccountService))

		r.Put("/password/reset", s.handlePasswordReset)
	})

	// Rotas públicas que precisam de contexto de tenant
	r.With(middleware.TenantTxMiddleware(s.db)).Post("/accounts", s.handleAccountCreate)
	r.Get("/accounts/confirm", s.handleAccountConfirmation) // Gestão manual de TX/Tenant
	r.With(middleware.TenantTxMiddleware(s.db)).Post("/accounts/resend-confirmation", s.handleResendConfirmationEmail)
}

// handleAccountCreate - Handles User/Associate registration within a specific tenant.
// Creates global account (if needed), tenant profile, and tenant membership.
// Includes explicit NIF/TLM uniqueness checks within the tenant.
func (s *Server) handleAccountCreate(w http.ResponseWriter, r *http.Request) {
	var params npadmin.AccountParams // Params have email, names, category, NIF, TLM, password

	if err := DecodeJSON(w, r, &params); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}

	// Basic struct validation (format, required, GLOBAL email uniqueness)
	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}

	// Extract Queryable (transaction 'tx' with tenant_id defined) from context
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "database transaction missing"})
		return
	}

	// --- Tenant-Specific Uniqueness Checks ---
	logger := s.Log // Get logger

	// Check NIF uniqueness within the tenant
	if !validation.IsNIFUniqueInTenant(tx, params.NIF.String, logger) {
		errPayload := map[string]string{"nif": "nif-uniqueness-tenant"}
		s.JSON(w, r, http.StatusUnprocessableEntity, errPayload)
		return
	}

	// Check TLM uniqueness within the tenant (if provided)
	if params.TLM.Valid && params.TLM.String != "" {
		if !validation.IsTLMUniqueInTenant(tx, params.TLM.String, logger) {
			errPayload := map[string]string{"tlm": "tlm-uniqueness-tenant"}
			s.JSON(w, r, http.StatusUnprocessableEntity, errPayload)
			return
		}
	}
	// --- End Tenant-Specific Uniqueness Checks ---

	// Get Category ID within the current tenant
	categoryID, err := s.AccountService.GetCategory(tx, params.Category)
	if err != nil {
		s.LogError(r, fmt.Errorf("failed GetCategory in create: %w", err))
		if errors.Is(err, sql.ErrNoRows) {
			s.JSON(w, r, http.StatusUnprocessableEntity, envelope{"category": "invalid category for this tenant"})
		} else {
			s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "failed to validate category"})
		}
		return
	}

	// Prepare Account struct (data transferred to services)
	account := npadmin.Account{
		Email:     params.Email,
		StatusID:  npadmin.AccountPending,
		Password:  params.Password,
		FirstName: params.FirstName, // Used for Profile
		LastName:  params.LastName,  // Used for Profile
		NIF:       params.NIF,       // Used for Profile
		TLM:       params.TLM,       // Used for Profile
		Role:      RoleAssociate, // Used for Profile (maybe redundant?) -> Membership uses 'associate' explicitly
		Category:  categoryID,   // Used for Profile
	}

	// Call Create service (handles global account, profile, membership)
	token, err := s.AccountService.Create(tx, &account)
	if err != nil {
		s.LogError(r, fmt.Errorf("failed AccountService.Create: %w", err))
		status, _ := GetHTTPStatusFromStoreError(err)
		// Check for specific DB constraint errors
		if strings.Contains(err.Error(), "account_tenant_memberships_account_id_tenant_id_key") {
			status = http.StatusConflict
			s.JSON(w, r, status, envelope{"message": "user is already associated with this group"})
		} else if strings.Contains(err.Error(), "profiles_nif_tenant_id_idx") { // New NIF index
            status = http.StatusConflict
            s.JSON(w, r, status, envelope{"nif": "nif already exists in this group"})
		} else if strings.Contains(err.Error(), "profiles_tlm_tenant_id_idx") { // New TLM index
            status = http.StatusConflict
            s.JSON(w, r, status, envelope{"tlm": "tlm already exists in this group"})
		} else {
			s.JSON(w, r, status, envelope{"message": err.Error()}) // Generic error
		}
		return
	}

	// Send Confirmation Email
	accountView := npadmin.AccountView{ Account: &account, Token: token, Endpoint: s.buildURL("/api/v1/accounts/confirm", url.Values{"token": {token.Plaintext}})}
	s.Mailer.SendWelcomeEmail(account.Email, accountView)

	// Respond with global account info
	respAccount := map[string]interface{}{"email": account.Email, "status": account.StatusDescription}
	s.JSON(w, r, http.StatusOK, envelope{"account": respAccount})
}

// handleAccountConfirmation manages confirmation, activates global account, handles returned tenantID.
func (s *Server) handleAccountConfirmation(w http.ResponseWriter, r *http.Request) {
	var params npadmin.TokenParams
	if err := s.decoder.Decode(&params, r.URL.Query()); err != nil {
		s.LogError(r, fmt.Errorf("failed to decode confirmation params: %w", err))
		http.Redirect(w, r, s.cfg.WebApp.AccountErrorPage, http.StatusSeeOther)
		return
	}
	validationErr := s.validate.Struct(params)
	if validationErr != nil {
		s.LogError(r, fmt.Errorf("invalid confirmation params: %w", validationErr))
		http.Redirect(w, r, s.cfg.WebApp.AccountErrorPage, http.StatusSeeOther)
		return
	}

	dbx := sqlx.NewDb(s.db, "postgres")
	tx, err := dbx.BeginTxx(r.Context(), &sql.TxOptions{})
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to begin account confirmation transaction: %w", err))
		http.Redirect(w, r, s.cfg.WebApp.AccountErrorPage, http.StatusSeeOther)
		return
	}
	var committed bool = false
	defer func() {
		if !committed { _ = tx.Rollback() }
	}()

	account, tenantID, err := s.AccountService.ExecuteAccountConfirmation(tx, params.Token)
	if err != nil {
		// Log the specific error from the service
		s.LogError(r, fmt.Errorf("account confirmation service execution failed: %w", err))
		http.Redirect(w, r, s.cfg.WebApp.AccountErrorPage, http.StatusSeeOther)
		return
	}

	// Set RLS context within the transaction post-verification
	tenantIDStr := strconv.FormatInt(tenantID, 10)
	if _, err := tx.ExecContext(r.Context(), "SELECT set_config('myapp.tenant_id', $1, true)", tenantIDStr); err != nil {
		s.LogError(r, fmt.Errorf("failed to set tenant config in confirmation tx post-verify: %w", err))
		http.Redirect(w, r, s.cfg.WebApp.AccountErrorPage, http.StatusSeeOther)
		return
	}
	accountIDStr := strconv.FormatInt(account.ID, 10)
	if _, err := tx.ExecContext(r.Context(), "SELECT set_config('myapp.user_id', $1, true)", accountIDStr); err != nil {
		fmt.Printf("Warning: failed to set user config in confirmation tx post-verify: %v\n", err) // Log non-fatal error
	}

	// Fetch tenant-specific profile and category
	profile, errProfile := s.ProfileService.Get(tx, account.ID)
	if errProfile != nil {
		s.LogError(r, fmt.Errorf("failed to get profile after confirmation (tenant %d): %w", tenantID, errProfile))
		profile = nil // Ensure profile is nil if fetch failed
	}

	category := "N/A"
	if profile != nil {
		var catErr error
		category, catErr = s.AccountService.GetCategoryByID(tx, profile.Category)
		if catErr != nil {
			s.LogError(r, fmt.Errorf("failed to get category after confirmation (tenant %d): %w", tenantID, catErr))
			category = "Unknown"
		}
	} else {
		s.LogWarn(r, fmt.Errorf("skipped getting category because profile was nil for account ID %d in tenant %d", account.ID, tenantID))
	}

	// Commit transaction
	if err := tx.Commit(); err != nil {
		s.LogError(r, fmt.Errorf("failed to commit account confirmation transaction: %w", err))
		http.Redirect(w, r, s.cfg.WebApp.AccountErrorPage, http.StatusSeeOther) // Redirect on commit error
		return
	}
	committed = true

	// Generate QR Code (non-critical)
	qrURL := fmt.Sprintf("%s%d&tenant=%d", s.cfg.WebApp.ScanAssociatePage, account.ID, tenantID)
	qrCodePath := fmt.Sprintf("./.qrcodes/acc%d_ten%d.png", account.ID, tenantID)
	errQR := qrcode.WriteFile(qrURL, qrcode.Medium, 256, qrCodePath)
	if errQR != nil {
		s.LogError(r, fmt.Errorf("failed to generate QR code: %w", errQR)) // Log QR error
	}

	// Send Associate Card Email (if profile and QR code are ok)
	cardName := "N/A"
	if profile != nil {
		cardName = fmt.Sprintf("%s %s", profile.FirstName.String, profile.LastName.String)
	}

	if profile != nil && errQR == nil {
		s.Mailer.SendAssociateCard(account.Email, npadmin.AccountCard{
			AssociateNumber: account.ID, // Using global ID
			Category:        category,
			Email:           account.Email,
			QRCode:          qrCodePath,
			Name:            cardName,
		})
	} else {
		reason := "unknown"
		if profile == nil { reason = "profile is nil" } else
		if errQR != nil { reason = "QR generation error" }
		s.LogWarn(r, fmt.Errorf("skipped sending card email for ID %d, Tenant %d because %s", account.ID, tenantID, reason))
	}

	// Final Redirect on Success
	http.Redirect(w, r, s.cfg.WebApp.AccountSuccessPage, http.StatusSeeOther)
}


// handleResendConfirmationEmail busca conta global pendente e reenvia email.
func (s *Server) handleResendConfirmationEmail(w http.ResponseWriter, r *http.Request) {
	var params npadmin.RecoverEmailParams
	if err := DecodeJSON(w, r, &params); err != nil { s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()}); return }
	if err := s.validate.Struct(params); err != nil { s.JSON(w, r, http.StatusUnprocessableEntity, err); return }
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok { s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "database transaction missing"}); return }
	tenantIDStr, _ := r.Context().Value(middleware.CtxKeyTenantID).(string)
	tenantID, _ := strconv.ParseInt(tenantIDStr, 10, 64)

	// Use wrapper to ensure verification hash includes current tenantID
	account, token, err := s.initiateAccountConfirmationWithTenant(tx, params.Email, tenantID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) { s.JSON(w, r, http.StatusNotFound, envelope{"email": "No pending activation found"}); return }
		s.LogError(r, fmt.Errorf("failed InitiateAccountConfirmation on resend: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "failed to initiate confirmation resend"}); return
	}

	accountView := npadmin.AccountView{ Account: account, Token: token, Endpoint: s.buildURL("/api/v1/accounts/confirm", url.Values{"token": {token.Plaintext}})}
	s.Mailer.ResendWelcomeEmail(account.Email, accountView)
	respAccount := map[string]interface{}{"email": account.Email, "status": account.StatusDescription}
	s.JSON(w, r, http.StatusOK, envelope{"account": respAccount})
}

// initiateAccountConfirmationWithTenant is a helper to ensure tenantID is added to the verification hash.
func (s *Server) initiateAccountConfirmationWithTenant(q npadmin.Queryable, email string, tenantID int64) (*npadmin.Account, *crypto.Token, error) {
	account, err := s.AccountService.GetUnverifiedByEmail(q, email)
	if err != nil { return nil, nil, err }
	token, err := crypto.GenerateVerificationToken(3 * 24 * time.Hour)
	if err != nil { return nil, nil, fmt.Errorf("failed to generate verification token on resend") }
	// Create verification token WITH tenant ID
	verificationToken := npadmin.VerificationToken{ ID: account.ID, Hash: crypto.HexEncode(token.Hash), Deadline: token.Deadline, TenantID: npadmin.NewNullInt64(tenantID)}
	// Call service to store the hash (service handles DB insert/update)
	if err := s.AccountService.CreateEmailVerificationHash(&verificationToken, npadmin.WithTx(q)); err != nil {
		return nil, nil, fmt.Errorf("failed to create verification hash on resend: %w", err)
	}
	return account, token, nil
}

// handlePasswordReset operates on the global account obtained via AccountCtx.
func (s *Server) handlePasswordReset(w http.ResponseWriter, r *http.Request) {
	var params npadmin.ResetPasswordParams
	if err := DecodeJSON(w, r, &params); err != nil { s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()}); return }
	if err := s.validate.Struct(params); err != nil { s.JSON(w, r, http.StatusUnprocessableEntity, err); return }
	// Get global account from context (set by AccountCtx middleware)
	account, ok := r.Context().Value(middleware.CtxKeyAccount).(*npadmin.Account)
	if !ok || account == nil { s.JSON(w, r, http.StatusUnauthorized, "Unauthorized - Account context missing"); return }
	// Get transaction from context
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok { s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "database transaction missing"}); return }
	// Execute password reset on global account
	err := s.AccountService.ExecutePasswordReset(tx, account, params.Password)
	if err != nil { s.LogError(r, fmt.Errorf("password reset execution failed: %w", err)); s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "failed to reset password"}); return }
	s.JSON(w, r, http.StatusOK, envelope{"password": "changed"})
}

// GetAccountHandler (Example handler, adjust if needed)
// Fetches global account data. Needs tenant context (RLS via tx) if fetching profile data.
func GetAccountHandler(w http.ResponseWriter, r *http.Request) {
	var accountID int64
	// Get account ID from context (preferred)
	if idVal := r.Context().Value(middleware.CtxKeyUserID); idVal != nil { if id, ok := idVal.(int64); ok { accountID = id } }
	// Fallback to query param (if allowed by design)
	if accountID == 0 { if idStr := r.URL.Query().Get("id"); idStr != "" { if id, err := strconv.ParseInt(idStr, 10, 64); err == nil { accountID = id } } }
	if accountID == 0 { http.Error(w, "account id missing", http.StatusBadRequest); return }

	// Get transaction context
	txAny := r.Context().Value(middleware.CtxKeyTx)
	if tx, ok := txAny.(npadmin.Queryable); ok && tx != nil {
		// Fetch global account data
		row := tx.QueryRow("SELECT id, email FROM accounts WHERE id = $1", accountID)
		var id int64; var email string
		if err := row.Scan(&id, &email); err != nil {
			if errors.Is(err, sql.ErrNoRows) { http.Error(w, "not found", http.StatusNotFound) } else { http.Error(w, "internal server error", http.StatusInternalServerError) }
			return
		}
		// Example response
		fmt.Fprintf(w, "Global Account ID: %d, Email: %s", id, email)
		// If needed, fetch profile for current tenant using tx (RLS applied)
		// tenantIDStr, _ := r.Context().Value(middleware.CtxKeyTenantID).(string)
		// var profile npadmin.Profile
		// errProfile := tx.Get(&profile, "SELECT first_name, last_name FROM profiles WHERE id = $1", accountID) // RLS adds tenant filter
		// if errProfile == nil { fmt.Fprintf(w, ", Name in Tenant %s: %s %s", tenantIDStr, profile.FirstName.String, profile.LastName.String) }
		return
	}
	http.Error(w, "database transaction missing", http.StatusInternalServerError)
}