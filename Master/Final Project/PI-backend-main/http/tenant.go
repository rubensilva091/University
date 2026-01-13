package http

import (
	//"context"
	"database/sql"
	"fmt"
	"net/http"
	//"strconv"
	"strings"

	"github.com/go-chi/chi/v5"
	// "github.com/golang-jwt/jwt/v4"
	"github.com/jmoiron/sqlx"
	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
	"github.com/invisiblelab-dev/npadmin/http/middleware"
)

// registerTenantRoutes regista as rotas para /tenants.
func (s *Server) registerTenantRoutes(r chi.Router) {

	// 1. Rotas PÚBLICAS (Qualquer pessoa pode aceder)
	r.Post("/tenants", s.handleTenantCreate)
	r.Get("/allTenants", s.handleGetAllTenants)

	// 2. Rotas PROTEGIDAS (Requerem Login)
	// Tudo o que estiver dentro deste bloco tem acesso ao User ID e à Transação DB
	r.With(
		middleware.JwtVerifier(crypto.ParseAuthorizationHeader),
		middleware.JwtAuthorize(s.cfg.JwtPublicKey),
		middleware.UserTxMiddleware(s.db),       // Cria a transação
		middleware.AccountCtx(s.AccountService), // Identifica o user
	).Route("/", func(r chi.Router) {
		
		// Listar tenants do utilizador
		r.Get("/userTenants", s.handleGetUserTenants)
		
		// Aderir a um tenant (AGORA ESTÁ PROTEGIDO)
		r.Post("/tenants/{tenantId}/join", s.handleTenantJoin) 
	})
}

// handleTenantCreate lida com o pedido de criação de um novo tenant e do seu admin.
func (s *Server) handleTenantCreate(w http.ResponseWriter, r *http.Request) {
	var params npadmin.TenantCreateParams

	// 1. Descodificar o JSON
	if err := DecodeJSON(w, r, &params); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}

	// 2. Validar a estrutura básica dos dados
	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}

	// 3. Iniciar Transação Manual
	dbx := sqlx.NewDb(s.db, "postgres")
	tx, err := dbx.BeginTxx(r.Context(), &sql.TxOptions{})
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to begin tenant creation transaction: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "database error"})
		return
	}
	committed := false
	defer func() {
		if !committed {
			_ = tx.Rollback()
		}
	}()

	// 4. Chamar o Serviço de Criação do Tenant
	tenantID, adminAccountID, err := s.TenantService.Create(tx, params)
	if err != nil {
		s.LogError(r, fmt.Errorf("TenantService.Create failed: %w", err))

		if strings.Contains(err.Error(), "domain") || strings.Contains(err.Error(), "tenants_domain_key") {
			s.JSON(w, r, http.StatusConflict, envelope{"domain": "domain is already taken"})
		} else if strings.Contains(err.Error(), "accounts_email_idx") {
			s.JSON(w, r, http.StatusConflict, envelope{"adminEmail": "email constraint failed"})
		} else if strings.Contains(err.Error(), "profiles_nif_tenant_id_idx") {
			s.JSON(w, r, http.StatusConflict, envelope{"adminNIF": "NIF already exists for this tenant"})
		} else {
			s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "failed to create tenant"})
		}
		return
	}

	// 5. Commit da Transação
	if err := tx.Commit(); err != nil {
		s.LogError(r, fmt.Errorf("failed to commit tenant creation transaction: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "database error during commit"})
		return
	}
	committed = true

	// 6. Responder com Sucesso
	s.JSON(w, r, http.StatusCreated, envelope{
		"tenantId":       tenantID,
		"adminAccountId": adminAccountID,
		"adminEmail":     params.AdminEmail,
		"status":         "created",
	})
}

// handleGetAllTenants retorna TODOS os tenants registados no sistema.
// Acessível em /api/v1/allTenants
func (s *Server) handleGetAllTenants(w http.ResponseWriter, r *http.Request) {
	// 1. Iniciar uma conexão simples (leitura)
	// Usamos uma conexão 'postgres' normal ou uma transação readonly.
	// Como é uma leitura global na tabela 'tenants', não usamos TenantTxMiddleware.
	dbx := sqlx.NewDb(s.db, "postgres")
	
	tx, err := dbx.BeginTxx(r.Context(), &sql.TxOptions{ReadOnly: true})
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to begin read transaction for all tenants: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "database error"})
		return
	}
	defer tx.Rollback()

	// 2. Chamar o serviço (GetAllTenants)
	// Este método deve estar definido na interface TenantService (verifique se atualizou o ficheiro tenant.go)
	tenants, err := s.TenantService.GetAllTenants(tx)
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to get all tenants: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "could not retrieve tenants"})
		return
	}

	// 3. Responder
	if tenants == nil {
		tenants = []npadmin.TenantSummary{}
	}

	s.JSON(w, r, http.StatusOK, envelope{"tenants": tenants})
}

// handleGetUserTenants lista os tenants do utilizador autenticado.
func (s *Server) handleGetUserTenants(w http.ResponseWriter, r *http.Request) {
	accountID, ok := r.Context().Value(middleware.CtxKeyUserID).(int64)
	if !ok || accountID == 0 {
		s.JSON(w, r, http.StatusUnauthorized, "Unauthorized - Account context missing")
		return
	}

	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, fmt.Errorf("database transaction missing in handleGetUserTenants"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	tenants, err := s.TenantService.GetUserTenants(tx, accountID)
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to get user tenants: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "could not retrieve tenant list"})
		return
	}

	if tenants == nil {
		tenants = []npadmin.TenantSummary{}
	}

	s.JSON(w, r, http.StatusOK, envelope{"tenants": tenants})
}

// handleTenantJoin permite a um utilizador autenticado aderir a um tenant existente.
func (s *Server) handleTenantJoin(w http.ResponseWriter, r *http.Request) {
	// 1. Validar autenticação (se a rota estiver bem protegida, isto nunca falha)
	accountID, ok := r.Context().Value(middleware.CtxKeyUserID).(int64)
	if !ok || accountID == 0 {
		s.JSON(w, r, http.StatusUnauthorized, envelope{"message": "Unauthorized"})
		return
	}

	// 2. Validar transação de base de dados
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, fmt.Errorf("database transaction missing"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "server error"})
		return
	}

	// 3. Obter ID do tenant
	tenantIDStr := chi.URLParam(r, "tenantId")
	var tenantID int64
	_, err := fmt.Sscan(tenantIDStr, &tenantID)
	if err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": "invalid tenant ID"})
		return
	}

	// 4. Executar a adesão
	err = s.TenantService.Join(tx, accountID, tenantID)
	if err != nil {
		if strings.Contains(err.Error(), "unique") || strings.Contains(err.Error(), "duplicate") {
			s.JSON(w, r, http.StatusConflict, envelope{"message": "already a member"})
			return
		}
		s.LogError(r, fmt.Errorf("failed to join tenant: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "failed to join association"})
		return
	}

	// 5. Sucesso
	s.JSON(w, r, http.StatusOK, envelope{"status": "joined", "tenantId": tenantID})
}