package http

import (
	"database/sql"
	"errors"
	"fmt"
	"net/http"
	"net/url"
	"strconv" // <-- ADICIONADO
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/jmoiron/sqlx"
	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
	"github.com/invisiblelab-dev/npadmin/http/middleware" // Usar alias 'middleware'

	"golang.org/x/crypto/bcrypt" // Manter import bcrypt
)

func (s *Server) registerAuthenticationRoutes(r chi.Router) {
	// Aplicar TenantTxMiddleware às rotas que precisam de contexto de tenant e acesso BD
	r.With(middleware.TenantTxMiddleware(s.db)).Post("/password-login", s.handlePasswordLogin)
	r.With(middleware.TenantTxMiddleware(s.db)).Post("/email-login", s.handleEmailLogin)
	// handleEmailLoginConfirmation precisa de uma abordagem especial para transação/tenant
	r.Get("/email-login/confirm", s.handleEmailLoginConfirmation) // Remover middleware daqui, será gerido dentro do handler

	// Logout pode não precisar de DB, manter como está por agora.
	r.Post("/logout", s.handleLogout)
}

func (s *Server) handlePasswordLogin(w http.ResponseWriter, r *http.Request) {
	var params npadmin.AuthenticationParams

	if err := DecodeJSON(w, r, &params); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}

	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}

	// Extrair Queryable (transação) do contexto (já inclui tenant_id definido pelo middleware)
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "database transaction missing"})
		return
	}

	// --- INÍCIO DA MODIFICAÇÃO ---
	// Extrair tenantID (string) do contexto (definido pelo middleware)
	tenantIDStr, ok := r.Context().Value(middleware.CtxKeyTenantID).(string)
	if !ok || tenantIDStr == "" {
		// Isto não deve acontecer se o TenantTxMiddleware correu
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "tenant context missing"})
		return
	}

	tenantID, errConv := strconv.ParseInt(tenantIDStr, 10, 64)
	if errConv != nil {
		// Isto também não deve acontecer, pois o middleware já valida
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": "invalid tenant id format"})
		return
	}
	// --- FIM DA MODIFICAÇÃO ---

	// O tenant_id para a operação atual está implícito em 'tx' devido ao middleware

	// Chamar serviço que agora retorna conta global
	// NOTA: O login valida contra a conta global, MAS a operação ocorre no contexto do tenant
	// especificado no pedido (header X-Tenant-ID).
	account, err := s.AuthenticationService.CreatePasswordLogin(tx, params) // Retorna Account global
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) || errors.Is(err, bcrypt.ErrMismatchedHashAndPassword) || err.Error() == "password does not match" {
			s.JSON(w, r, http.StatusForbidden, envelope{"credentials": "invalid"})
		} else {
			s.LogError(r, err)
			s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		}
		return
	}

	// --- INÍCIO DA MODIFICAÇÃO ---
	// VERIFICAR SE A CONTA PERTENCE AO TENANT ATUAL
	err = s.AuthenticationService.CheckTenantMembership(tx, account.ID, tenantID)
	if err != nil {
		// O utilizador/pass estão corretos globalmente, mas não é membro deste tenant
		s.LogWarn(r, fmt.Errorf("login attempt from valid user (ID: %d) to tenant (ID: %d) where they are not a member", account.ID, tenantID))

		// Retornar um erro genérico de credenciais/proibido
		// Usamos StatusForbidden (403) porque a autenticação global *foi* válida,
		// mas a autorização para este tenant falhou.
		s.JSON(w, r, http.StatusForbidden, envelope{"credentials": "invalid user for this tenant"})
		return
	}
	// --- FIM DA MODIFICAÇÃO ---

	// --- Geração de Tokens (Globais) ---
	// REMOVIDO: Não incluir tenant_id nos claims por defeito
	// claims := map[string]interface{}{"tenant_id": account.TenantID} -> NÃO USAR tenant_id aqui
	claims := map[string]interface{}{} // Claims vazios ou apenas com info global se necessário

	// Auth Token (30 min TTL) - Subject é o email global
	token, err := crypto.NewJwtTokenWithClaims(account.Email, s.cfg.JwtPrivateKey, 30*time.Minute, claims) //
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}

	// Refresh Token (24 h TTL) - Subject é o email global
	refreshToken, err := crypto.NewJwtTokenWithClaims(account.Email, s.cfg.JwtRefreshPrivateKey, 24*time.Hour, claims) //
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}

	refreshClaims, err := crypto.GetJwtClaims(s.cfg.JwtRefreshPublicKey, refreshToken) //
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}
	// --- Fim Geração de Tokens ---

	// Armazenar Refresh Token associado ao ID da conta global (passar 'tx')
	// A tabela account_jwt_refresh_keys não tem tenant_id
	if err := s.AuthorizationService.StoreRefreshToken(tx, refreshClaims.ID, account.ID, refreshClaims.ExpiresAt.Time); err != nil { //
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}

	s.JSON(w, r, http.StatusOK, envelope{"token": token, "refreshToken": refreshToken})
}

func (s *Server) handleEmailLogin(w http.ResponseWriter, r *http.Request) {
	var params npadmin.EmailAuthenticationParams

	if err := DecodeJSON(w, r, &params); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}

	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}

	// Extrair Queryable (transação) do contexto (inclui tenant_id)
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "database transaction missing"})
		return
	}

	// Chamar serviço que retorna conta global e token
	// A verificação da conta (se existe e está ativa) pode ocorrer no contexto do tenant atual
	// se a query em CreateEmailLogin o fizer, ou pode ser global. A implementação atual busca globalmente.
	account, token, err := s.AuthenticationService.CreateEmailLogin(tx, params) //
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			s.JSON(w, r, http.StatusNotFound, envelope{"email": "account not found or not active"}) // Mensagem ajustada
		} else {
			s.LogError(r, err)
			s.JSON(w, r, http.StatusInternalServerError, err)
		}
		return
	}

	// Email não precisa de tx
	authenticationView := npadmin.AccountView{
		Account:  account, // Account global
		Token:    token,
		Endpoint: s.buildURL("/api/v1/email-login/confirm", url.Values{"token": {token.Plaintext}}),
	}
	s.Mailer.SendLoginEmail(params.Email, authenticationView) //

	s.JSON(w, r, http.StatusOK, envelope{"email": params.Email, "status": "sent"})
}

func (s *Server) handleEmailLoginConfirmation(w http.ResponseWriter, r *http.Request) {
	var params npadmin.TokenParams

	if err := s.decoder.Decode(&params, r.URL.Query()); err != nil {
		s.LogError(r, err)
		http.Redirect(w, r, s.cfg.WebApp.LoginErrorPage, http.StatusSeeOther)
		return
	}

	err := s.validate.Struct(params)
	if err != nil {
		s.LogError(r, err)
		http.Redirect(w, r, s.cfg.WebApp.LoginErrorPage, http.StatusSeeOther)
		return
	}

	dbx := sqlx.NewDb(s.db, "postgres")
	tx, err := dbx.BeginTxx(r.Context(), &sql.TxOptions{})
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to begin email login confirmation transaction: %w", err))
		http.Redirect(w, r, s.cfg.WebApp.LoginErrorPage, http.StatusSeeOther)
		return
	}
	committed := false
	defer func() {
		if !committed {
			_ = tx.Rollback()
		}
	}()

	account, err := s.AuthenticationService.ExecuteEmailLoginConfirmation(tx, params.Token)
	if err != nil {
		s.LogError(r, err)
		http.Redirect(w, r, s.cfg.WebApp.LoginErrorPage, http.StatusSeeOther)
		return
	}

	claims := map[string]interface{}{}
	token, err := crypto.NewJwtTokenWithClaims(account.Email, s.cfg.JwtPrivateKey, 30*time.Minute, claims)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "failed to generate auth token"})
		return
	}
	refreshToken, err := crypto.NewJwtTokenWithClaims(account.Email, s.cfg.JwtRefreshPrivateKey, 24*time.Hour, claims)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "failed to generate refresh token"})
		return
	}
	refreshClaims, err := crypto.GetJwtClaims(s.cfg.JwtRefreshPublicKey, refreshToken)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "failed to parse refresh token claims"})
		return
	}

	if err := s.AuthorizationService.StoreRefreshToken(tx, refreshClaims.ID, account.ID, refreshClaims.ExpiresAt.Time); err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "failed to store refresh token"})
		return
	}

	if err := tx.Commit(); err != nil {
		s.LogError(r, fmt.Errorf("failed to commit email login confirmation transaction: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "failed to finalize login"})
		return
	}
	committed = true

	// --- Lógica de Redirect ---
	// CORREÇÃO: Merged declaration and assignment (S1021 fix)
	redirectURL := s.cfg.WebApp.AssociateDashboard + "?refresh=" + url.QueryEscape(refreshToken) + "&token=" + url.QueryEscape(token) // <-- FIX HERE

	// Comentário: A lógica de decidir entre Admin e Associate Dashboard é mais complexa agora.

	http.Redirect(w, r, redirectURL, http.StatusSeeOther)
}

// handleLogout permanece simples por agora.
func (s *Server) handleLogout(w http.ResponseWriter, r *http.Request) {
	// Se precisar invalidar refresh tokens, precisaria:
	// 1. Extrair token/ID do pedido.
	// 2. Iniciar transação (poderia usar TenantTxMiddleware se fizer sentido para logout).
	// 3. Chamar um serviço para apagar/invalidar o token no DB.
	s.JSON(w, r, http.StatusOK, envelope{"message": "logged out"})
}