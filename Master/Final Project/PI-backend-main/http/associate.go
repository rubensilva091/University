package http

import (
	"database/sql"
	"errors"
	"fmt" // Importar fmt
	"net/http"
	"strconv" // Importar strconv
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
	"github.com/invisiblelab-dev/npadmin/http/middleware" // Usar alias
)

func (s *Server) registerAssociateRoutes(r chi.Router) {
	r.Route("/associate", func(r chi.Router) {
		// Ordem: JWT -> Tenant -> Account
		r.Use(middleware.JwtVerifier(crypto.ParseAuthorizationHeader))
		r.Use(middleware.JwtAuthorize(s.cfg.JwtPublicKey))
		r.Use(middleware.TenantTxMiddleware(s.db)) // Define tx, tenant_id, user_id
		r.Use(middleware.AccountCtx(s.AccountService)) // Define conta global e userID (usando tx)

		// dashboard do associado (opera no tenant atual)
		r.Get("/status", s.handleAssociateStatus)

		// quotas do associado (opera no tenant atual)
		r.Get("/subscription/history", s.handleAssociateSubscriptionHistory)

		// Scan de QRCode do associado por um admin (requer verificação de admin no tenant atual)
		r.Get("/scan", s.handleScanAssociateStatus)
	})
}

// handleAssociateStatus busca o status global da conta e os dados específicos (perfil, subscrições) do tenant atual.
func (s *Server) handleAssociateStatus(w http.ResponseWriter, r *http.Request) {
	var pagination npadmin.Pagination
	qs := r.URL.Query()
	if err := s.decoder.Decode(&pagination, qs); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"qs": "unsupported", "err": err})
		return
	}

	if err := s.validate.Struct(pagination); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}

	if pagination.PageSize == 0 {
		pagination.PageSize = 10
		pagination.Page = 1
	}

	// Obter conta global e ID do contexto (definidos por AccountCtx)
	account, okAcc := r.Context().Value(middleware.CtxKeyAccount).(*npadmin.Account) //
	accountID, okID := r.Context().Value(middleware.CtxKeyUserID).(int64)           //

	if !okAcc || account == nil || !okID || accountID == 0 {
		s.JSON(w, r, http.StatusUnauthorized, "Unauthorized - Account context missing or invalid")
		return
	}

	// Extrair Queryable (transação 'tx' com contexto RLS: tenant_id e user_id)
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable) //
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleAssociateStatus"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	accountStatus := account.StatusDescription // Status global
	var subscriptionStatus string
	var profile *npadmin.Profile // Perfil específico do tenant

	// Buscar o Perfil específico deste tenant usando 'tx' (RLS aplica tenant_id e user_id)
	profile, err := s.ProfileService.Get(tx, accountID) //
	if err != nil {
		// Se não houver perfil NESTE tenant, pode ser normal ou um erro.
		if errors.Is(err, sql.ErrNoRows) {
			// Utilizador existe globalmente mas não tem perfil neste tenant.
			// Retornar status global, email, mas sem perfil/subscrições? Ou erro 403/404?
			// Por agora, vamos retornar os dados globais e indicar falta de perfil.
			profile = nil // Garantir que é nil
			s.LogWarn(r, fmt.Errorf("profile not found for user %d in current tenant context", accountID))
		} else {
			// Outro erro ao buscar perfil
			s.LogError(r, fmt.Errorf("error fetching profile for user %d: %w", accountID, err))
			s.JSON(w, r, http.StatusInternalServerError, err)
			return
		}
	}

	// Buscar Histórico de Subscrições neste tenant usando 'tx' (RLS aplica tenant_id e user_id)
	subscriptionHistory, err := s.DataService.SubscriptionHistory(tx, accountID, pagination) //
	if err != nil && !errors.Is(err, sql.ErrNoRows) { // Ignorar ErrNoRows, significa apenas histórico vazio
		s.LogError(r, fmt.Errorf("error fetching subscription history for user %d: %w", accountID, err))
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}
	if errors.Is(err, sql.ErrNoRows) {
		subscriptionHistory = []npadmin.Subscription{} // Garantir slice vazia
	}

	// Buscar Subscrição Atual neste tenant usando 'tx' (RLS aplica tenant_id e user_id)
	currentSubscription, err := s.DataService.CurrentSubscription(tx, accountID) //
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			// Sem subscrição atual neste tenant
			subscriptionStatus = npadmin.SubscriptionOther.String()
			currentSubscription = nil // Garantir que é nil
		} else {
			// Outro erro
			s.LogError(r, fmt.Errorf("error fetching current subscription for user %d: %w", accountID, err))
			s.JSON(w, r, http.StatusInternalServerError, err)
			return
		}
	} else {
		// Determinar status com base na subscrição atual
		if currentSubscription.EndDate.Before(time.Now().UTC()) {
			subscriptionStatus = npadmin.SubscriptionExpired.String()
		} else {
			subscriptionStatus = npadmin.SubscriptionValid.String()
		}
	}

	// Montar a resposta
	response := envelope{
		"status":               accountStatus, // Status Global
		"email":                account.Email,   // Email Global
		"profile":              profile,         // Perfil (pode ser nil se não existir neste tenant)
		"subscription-status":  subscriptionStatus,
		"subscription-history": subscriptionHistory,
	}
	if currentSubscription != nil {
		response["subscription-details"] = currentSubscription
	}

	s.JSON(w, r, http.StatusOK, response)
}

// handleAssociateSubscriptionHistory busca o histórico do tenant atual (via RLS em tx).
func (s *Server) handleAssociateSubscriptionHistory(w http.ResponseWriter, r *http.Request) {
	var params npadmin.Pagination
	qs := r.URL.Query()
	if err := s.decoder.Decode(&params, qs); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"qs": "unsupported", "err": err})
		return
	}

	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}

	if params.PageSize == 0 {
		params.PageSize = 10
		params.Page = 1
	}

	// Obter ID global do utilizador do contexto
	accountID, okID := r.Context().Value(middleware.CtxKeyUserID).(int64) //
	if !okID || accountID == 0 {
		s.JSON(w, r, http.StatusUnauthorized, "Unauthorized - User ID context missing")
		return
	}

	// Extrair Queryable (transação 'tx' com contexto RLS)
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable) //
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleAssociateSubscriptionHistory"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	// Chamar serviço passando 'tx' e 'accountID' (RLS aplica tenant_id e user_id)
	subscriptions, err := s.DataService.SubscriptionHistory(tx, accountID, params) //
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			s.JSON(w, r, http.StatusOK, envelope{"subscription-history": []npadmin.Subscription{}}) // Lista vazia
		} else {
			s.LogError(r, err)
			s.JSON(w, r, http.StatusInternalServerError, err)
		}
		return
	}

	s.JSON(w, r, http.StatusOK, envelope{"subscription-history": subscriptions})
}

// handleScanAssociateStatus verifica status/subscrição do ID escaneado DENTRO do tenant atual do admin.
func (s *Server) handleScanAssociateStatus(w http.ResponseWriter, r *http.Request) {
	var params struct { // Definir struct localmente para os query params
		ID       int64  `schema:"id"` // ID do utilizador escaneado
		TenantID *int64 `schema:"tenant"` // Opcional: Tenant ID do QR Code (se incluído)
	}
	qs := r.URL.Query()
	if err := s.decoder.Decode(&params, qs); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"qs": "unsupported scan parameters", "err": err})
		return
	}

	// Validar ID escaneado (deve ser > 0)
	if params.ID <= 0 {
		s.JSON(w, r, http.StatusBadRequest, envelope{"id": "invalid scanned user ID"})
		return
	}

	// --- Verificação de Admin ---
	// Obter ID do admin autenticado e tenantID do contexto
	scannerID, okID := r.Context().Value(middleware.CtxKeyUserID).(int64)       //
	tenantIDStr, okTenant := r.Context().Value(middleware.CtxKeyTenantID).(string) //
	if !okID || scannerID == 0 || !okTenant || tenantIDStr == "" {
		s.LogError(r, errors.New("admin context missing in handleScanAssociateStatus"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error: admin context"})
		return
	}
	tenantID, _ := strconv.ParseInt(tenantIDStr, 10, 64)

	// Extrair Queryable (transação 'tx' com contexto RLS do admin)
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable) //
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleScanAssociateStatus"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error: db context"})
		return
	}

	// Verificar se scannerID é admin neste tenantID
	var scannerRole string
	queryRole := `SELECT role FROM account_tenant_memberships WHERE account_id = $1 AND tenant_id = $2`
	errRole := tx.Get(&scannerRole, queryRole, scannerID, tenantID)
	if errRole != nil || scannerRole != "admin" {
		if errors.Is(errRole, sql.ErrNoRows) || scannerRole != "admin" {
			s.JSON(w, r, http.StatusForbidden, "Forbidden - Admin role required for this tenant")
		} else {
			s.LogError(r, fmt.Errorf("error checking admin role for scanner %d in tenant %d: %w", scannerID, tenantID, errRole))
			s.JSON(w, r, http.StatusInternalServerError, "Internal server error checking permissions")
		}
		return
	}
	// --- Fim Verificação de Admin ---

	// Opcional: Validar se o tenantID do QR Code (params.TenantID) corresponde ao tenantID atual do admin
	if params.TenantID != nil && *params.TenantID != tenantID {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": fmt.Sprintf("QR Code tenant (%d) does not match current admin tenant (%d)", *params.TenantID, tenantID)})
		return
	}

	// Buscar dados do utilizador escaneado (params.ID) DENTRO do tenant atual (tenantID)
	// Buscar conta global (para email e status global)
	scannedAccount, err := s.AccountService.Get(tx, params.ID) // Usa tx mas busca global
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			s.JSON(w, r, http.StatusNotFound, "Scanned account not found globally")
		} else {
			s.LogError(r, fmt.Errorf("error fetching scanned account %d: %w", params.ID, err))
			s.JSON(w, r, http.StatusInternalServerError, err)
		}
		return
	}

	// Buscar perfil do utilizador escaneado DENTRO do tenant atual (RLS no serviço Get deve permitir admin ver outros users do mesmo tenant)
	scannedProfile, err := s.ProfileService.Get(tx, params.ID) //
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			// Utilizador existe globalmente mas não tem perfil neste tenant.
			s.JSON(w, r, http.StatusNotFound, fmt.Sprintf("User %d exists but has no profile in tenant %d", params.ID, tenantID))
		} else {
			s.LogError(r, fmt.Errorf("error fetching scanned profile for account %d in tenant %d: %w", params.ID, tenantID, err))
			s.JSON(w, r, http.StatusInternalServerError, err)
		}
		return
	}

	// Buscar subscrição atual do utilizador escaneado DENTRO do tenant atual (RLS no serviço deve permitir admin)
	var subscriptionStatus string
	currentSubscription, err := s.DataService.CurrentSubscription(tx, params.ID) //
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			subscriptionStatus = npadmin.SubscriptionOther.String()
			currentSubscription = nil // Garantir nil
		} else {
			s.LogError(r, fmt.Errorf("error fetching current subscription for scanned account %d in tenant %d: %w", params.ID, tenantID, err))
			s.JSON(w, r, http.StatusInternalServerError, err)
			return
		}
	} else {
		if currentSubscription.EndDate.Before(time.Now().UTC()) {
			subscriptionStatus = npadmin.SubscriptionExpired.String()
		} else {
			subscriptionStatus = npadmin.SubscriptionValid.String()
		}
	}

	// Montar resposta
	response := envelope{
		"status":              scannedAccount.StatusDescription, // Status Global
		"email":               scannedAccount.Email,           // Email Global
		"profile":             scannedProfile,                 // Perfil específico do Tenant
		"subscription-status": subscriptionStatus,             // Status da subscrição no Tenant
	}
	if currentSubscription != nil {
		response["subscription-details"] = currentSubscription // Detalhes da subscrição no Tenant
	}

	s.JSON(w, r, http.StatusOK, response)
}