package http

import (
	"errors" // Import errors package
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/golang-jwt/jwt/v4"
	"github.com/invisiblelab-dev/npadmin" // Import npadmin
	"github.com/invisiblelab-dev/npadmin/crypto"
	"github.com/invisiblelab-dev/npadmin/http/middleware" // Use alias 'middleware'
)

func (s *Server) registerAuthorizationRoutes(r chi.Router) {
	r.Route("/refresh", func(r chi.Router) {
		r.Use(middleware.JwtVerifier(crypto.ParseAuthorizationHeader))
		r.Use(middleware.JwtAuthorize(s.cfg.JwtRefreshPublicKey)) // Use Refresh Public Key for refresh tokens
		// Apply TenantTxMiddleware as RotateRefreshToken accesses the DB
		r.Use(middleware.TenantTxMiddleware(s.db))
		r.Get("/", s.handleRefresh)
	})
}

func (s *Server) handleRefresh(w http.ResponseWriter, r *http.Request) {
	oldClaims, ok := r.Context().Value(middleware.CtxKeyJwtClaims).(*jwt.RegisteredClaims)
	if !ok {
		s.JSON(w, r, http.StatusUnauthorized, envelope{"token": "Unauthorized - Invalid claims"})
		return
	}

	// --- Token Generation (Doesn't need tx) ---
	//nolint:mnd // 24 hour refresh token TTL
	newRefreshToken, err := crypto.NewJwtToken(oldClaims.Subject, s.cfg.JwtRefreshPrivateKey, 24*time.Hour)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "Failed to generate refresh token"})
		return
	}

	newRefreshClaims, err := crypto.GetJwtClaims(s.cfg.JwtRefreshPublicKey, newRefreshToken)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "Failed to parse new refresh token claims"})
		return
	}

	//nolint:mnd // 30 minute token TTL
	newAuthToken, err := crypto.NewJwtToken(oldClaims.Subject, s.cfg.JwtPrivateKey, 30*time.Minute)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "Failed to generate auth token"})
		return
	}
	// --- End Token Generation ---

	// Extract Queryable (transaction) from context
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing in refresh handler"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "database transaction missing"})
		return
	}

	// Pass 'tx' to RotateRefreshToken
	rotated, err := s.AuthorizationService.RotateRefreshToken(tx, oldClaims.ID, newRefreshClaims.ID, newRefreshClaims.ExpiresAt.Time)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "Error rotating token"})
		return
	}
	if !rotated {
		// If rotation failed (e.g., old token ID not found), treat as unauthorized
		s.JSON(w, r, http.StatusUnauthorized, envelope{"token": "Unauthorized - Invalid refresh token"})
		return
	}

	s.JSON(w, r, http.StatusOK, envelope{"token": newAuthToken, "refreshToken": newRefreshToken})
}