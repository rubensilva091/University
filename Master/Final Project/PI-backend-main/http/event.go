package http

import (
	"database/sql" // Necessário para TxOptions
	"errors"
	"fmt"
	"net/http"
	"strconv"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/jmoiron/sqlx" // Necessário para iniciar transação manual
	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
	"github.com/invisiblelab-dev/npadmin/http/middleware"
)

func (s *Server) registerEventRoutes(r chi.Router) {
	authMiddleware := chi.NewRouter()
	authMiddleware.Use(middleware.JwtVerifier(crypto.ParseAuthorizationHeader))
	authMiddleware.Use(middleware.JwtAuthorize(s.cfg.JwtPublicKey))
	authMiddleware.Use(middleware.TenantTxMiddleware(s.db))
	authMiddleware.Use(middleware.AccountCtx(s.AccountService))

	adminMiddleware := chi.NewRouter()
	adminMiddleware.Use(authMiddleware.Middlewares()...)
	adminMiddleware.Use(middleware.AdminCtx(s.AccountService, s.ProfileService))

	r.Route("/admin/events", func(r chi.Router) {
		r.Use(adminMiddleware.Middlewares()...)
		r.Get("/", s.handleListAdminEvents)
		r.Post("/", s.handleAdminCreateEvent)
		r.Get("/{eventID}", s.handleAdminGetEventDetails)
		r.Post("/{eventID}/definitions", s.handleAdminCreateTicketDefinition)
		r.Get("/{eventID}/tickets", s.handleAdminGetPurchasedTickets)
		r.Get("/stats", s.handleGetEventsStats)
	})

	r.Route("/events", func(r chi.Router) {
		r.Use(middleware.TenantTxMiddleware(s.db))
		r.Get("/", s.handleListAvailableEvents)     // Lista eventos do tenant atual
		r.Get("/{eventID}", s.handleGetPublicEventDetails)
	})

	// --- NOVA ROTA PÚBLICA GLOBAL ---
	// Lista TODOS os eventos de TODAS as organizações (para a Homepage/Search Bar)
	r.Get("/allEvents", s.handleGetAllEvents)

	r.Route("/my-tickets", func(r chi.Router) {
		r.Use(authMiddleware.Middlewares()...)
		r.Get("/", s.handleGetMyTickets)
		r.Get("/{ticketID}", s.handleGetMyTicketDetails)
	})

	r.Route("/purchase", func(r chi.Router) {
		r.Use(authMiddleware.Middlewares()...)
		r.Post("/", s.handleInitiateTicketPurchase)
	})
}

func (s *Server) getTxAndAccount(w http.ResponseWriter, r *http.Request) (npadmin.Queryable, *npadmin.Account, bool) {
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return nil, nil, false
	}
	account, ok := r.Context().Value(middleware.CtxKeyAccount).(*npadmin.Account)
	if !ok || account == nil || account.ID == 0 {
		s.JSON(w, r, http.StatusUnauthorized, "Unauthorized - Account context missing")
		return nil, nil, false
	}

	// Injetar user_id na sessão da DB para o RLS funcionar
	querySetUser := fmt.Sprintf("SELECT set_config('myapp.user_id', '%d', true)", account.ID)
	if _, err := tx.Exec(querySetUser); err != nil {
		s.LogError(r, fmt.Errorf("failed to set RLS user context: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return nil, nil, false
	}

	return tx, account, true
}

func (s *Server) getEventIDParam(w http.ResponseWriter, r *http.Request) (int64, bool) {
	eventIDStr := chi.URLParam(r, "eventID")
	eventID, err := strconv.ParseInt(eventIDStr, 10, 64)
	if err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": "invalid event ID format"})
		return 0, false
	}
	return eventID, true
}

// --- Handlers de Admin ---

func (s *Server) handleListAdminEvents(w http.ResponseWriter, r *http.Request) {
    tx, _, ok := s.getTxAndAccount(w, r)
    if !ok { return }

    // 1. Obter o Tenant ID do contexto de segurança (garantido pelo middleware)
    tenantIDStr, ok := r.Context().Value(middleware.CtxKeyTenantID).(string)
    if !ok || tenantIDStr == "" {
        s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "tenant context missing"})
        return
    }
    
    tenantID, _ := strconv.ParseInt(tenantIDStr, 10, 64)

    // 2. Chamar o serviço com o ID explícito
    events, err := s.EventService.ListAdminEvents(tx, tenantID)
    if err != nil {
        s.LogError(r, fmt.Errorf("list admin events error: %w", err))
        s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "error listing events"})
        return
    }

    s.JSON(w, r, http.StatusOK, envelope{"events": events})
}

func (s *Server) handleAdminCreateEvent(w http.ResponseWriter, r *http.Request) {
	tx, _, ok := s.getTxAndAccount(w, r)
	if !ok { return }

	tenantIDStr, ok := r.Context().Value(middleware.CtxKeyTenantID).(string)
	if !ok || tenantIDStr == "" {
		s.LogError(r, errors.New("tenant ID missing from context"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	tenantID, errConv := strconv.ParseInt(tenantIDStr, 10, 64)
	if errConv != nil {
		s.LogError(r, fmt.Errorf("invalid tenant ID format in context: %w", errConv))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	var params struct {
		Name        string    `json:"name" validate:"required"`
		Description string    `json:"description"`
		EventDate   time.Time `json:"eventDate" validate:"required"`
		Location    string    `json:"location"`
		ImageURL    string    `json:"imageUrl"`
	}
	if err := DecodeJSON(w, r, &params); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}
	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}

	event := &npadmin.Event{
		TenantID:    tenantID,
		Name:        params.Name,
		Description: params.Description,
		EventDate:   params.EventDate,
		Location:    params.Location,
		ImageURL:    params.ImageURL,
	}

	if err := s.EventService.CreateEvent(tx, event); err != nil {
		s.LogError(r, fmt.Errorf("failed to create event: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "could not create event"})
		return
	}
	s.JSON(w, r, http.StatusCreated, envelope{"event": event})
}

func (s *Server) handleAdminCreateTicketDefinition(w http.ResponseWriter, r *http.Request) {
	tx, _, ok := s.getTxAndAccount(w, r)
	if !ok { return }
	
	eventID, ok := s.getEventIDParam(w, r)
	if !ok { return }

	// --- CORREÇÃO: Extrair TenantID do contexto ---
	tenantIDStr, ok := r.Context().Value(middleware.CtxKeyTenantID).(string)
	if !ok || tenantIDStr == "" {
		s.LogError(r, errors.New("tenant ID missing from context"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	tenantID, errConv := strconv.ParseInt(tenantIDStr, 10, 64)
	if errConv != nil {
		s.LogError(r, fmt.Errorf("invalid tenant ID format in context: %w", errConv))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}
	// ---------------------------------------------

	var params struct {
		Name              string  `json:"name" validate:"required"`
		Price             float64 `json:"price" validate:"gte=0"`
		AvailableQuantity int     `json:"availableQuantity" validate:"gte=0"`
	}
	if err := DecodeJSON(w, r, &params); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}
	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}

	def := &npadmin.EventTicketDefinition{
		TenantID:          tenantID, // <--- Faltava passar isto aqui!
		EventID:           eventID,
		Name:              params.Name,
		Price:             params.Price,
		AvailableQuantity: params.AvailableQuantity,
	}

	if err := s.EventService.CreateTicketDefinition(tx, def); err != nil {
		s.LogError(r, fmt.Errorf("failed to create ticket definition: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "could not create ticket definition"})
		return
	}
	s.JSON(w, r, http.StatusCreated, envelope{"definition": def})
}

func (s *Server) handleAdminGetEventDetails(w http.ResponseWriter, r *http.Request) {
	tx, _, ok := s.getTxAndAccount(w, r)
	if !ok { return }
	eventID, ok := s.getEventIDParam(w, r)
	if !ok { return }

	event, definitions, err := s.EventService.GetAdminEventDetails(tx, eventID)
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to get admin event details: %w", err))
		s.JSON(w, r, http.StatusNotFound, envelope{"message": "event not found"})
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"event": event, "definitions": definitions})
}

func (s *Server) handleAdminGetPurchasedTickets(w http.ResponseWriter, r *http.Request) {
	tx, _, ok := s.getTxAndAccount(w, r)
	if !ok { return }
	eventID, ok := s.getEventIDParam(w, r)
	if !ok { return }

	tickets, err := s.EventService.GetAdminPurchasedTicketsForEvent(tx, eventID)
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to get purchased tickets: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "could not retrieve tickets"})
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"tickets": tickets})
}

// --- Handlers Públicos / Utilizador ---

// handleGetAllEvents (NOVO) - Lista TODOS os eventos globais
func (s *Server) handleGetAllEvents(w http.ResponseWriter, r *http.Request) {
	// 1. Iniciar Transação de Leitura Simples (Sem RLS de Tenant específico)
	dbx := sqlx.NewDb(s.db, "postgres")
	tx, err := dbx.BeginTxx(r.Context(), &sql.TxOptions{ReadOnly: true})
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to begin read transaction for all events: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "database error"})
		return
	}
	defer tx.Rollback()

	// 2. Chamar Serviço (Usa a função segura get_all_events_safe)
	events, err := s.EventService.GetAllEvents(tx)
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to list all events: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "could not list events"})
		return
	}

	// 3. Responder
	if events == nil {
		events = []npadmin.Event{}
	}
	s.JSON(w, r, http.StatusOK, envelope{"events": events})
}

func (s *Server) handleListAvailableEvents(w http.ResponseWriter, r *http.Request) {
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	events, err := s.EventService.ListAvailableEvents(tx)
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to list events: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "could not list events"})
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"events": events})
}

func (s *Server) handleGetPublicEventDetails(w http.ResponseWriter, r *http.Request) {
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}
	eventID, ok := s.getEventIDParam(w, r)
	if !ok { return }

	event, definitions, err := s.EventService.GetPublicEventDetails(tx, eventID)
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to get public event details: %w", err))
		s.JSON(w, r, http.StatusNotFound, envelope{"message": "event not found"})
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"event": event, "definitions": definitions})
}

func (s *Server) handleGetMyTickets(w http.ResponseWriter, r *http.Request) {
	tx, account, ok := s.getTxAndAccount(w, r)
	if !ok { return }

	tickets, err := s.EventService.GetMyTickets(tx, account.ID)
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to get my tickets: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "could not retrieve tickets"})
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"tickets": tickets})
}

func (s *Server) handleGetMyTicketDetails(w http.ResponseWriter, r *http.Request) {
	tx, account, ok := s.getTxAndAccount(w, r)
	if !ok { return }

	ticketIDStr := chi.URLParam(r, "ticketID")
	ticketID, err := strconv.ParseInt(ticketIDStr, 10, 64)
	if err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": "invalid ticket ID format"})
		return
	}

	ticket, err := s.EventService.GetMyTicketDetails(tx, ticketID, account.ID)
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to get my ticket details: %w", err))
		s.JSON(w, r, http.StatusNotFound, envelope{"message": "ticket not found or access denied"})
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"ticket": ticket})
}

func (s *Server) handleInitiateTicketPurchase(w http.ResponseWriter, r *http.Request) {
	tx, account, ok := s.getTxAndAccount(w, r)
	if !ok { return }

	var params struct {
		DefinitionID int64  `json:"definitionId" validate:"required,gt=0"`
		PaymentType  string `json:"paymentType" validate:"required,oneof=multibanco mbway"`
		MBWayTLM     string `json:"mbwayTlm,omitempty"`
	}
	if err := DecodeJSON(w, r, &params); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}
	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}
	if params.PaymentType == "mbway" && params.MBWayTLM == "" {
		s.JSON(w, r, http.StatusUnprocessableEntity, envelope{"message": "mbwayTlm is required for mbway payments"})
		return
	}

	purchaseResp, err := s.EventService.InitiateTicketPurchase(
		tx,
		account.ID,
		params.DefinitionID,
		params.PaymentType,
		params.MBWayTLM,
		account.Email,
	)

	if err != nil {
		s.LogError(r, fmt.Errorf("failed to initiate purchase: %w", err))
		if err.Error() == "tickets sold out" {
			s.JSON(w, r, http.StatusConflict, envelope{"message": "tickets sold out"})
		} else if err.Error() == "ticket type not found" {
			s.JSON(w, r, http.StatusNotFound, envelope{"message": "ticket type not found"})
		} else {
			s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "could not process purchase"})
		}
		return
	}

	s.JSON(w, r, http.StatusCreated, envelope{"purchaseData": purchaseResp})
}

func (s *Server) handleGetEventsStats(w http.ResponseWriter, r *http.Request) {
	tx, _, ok := s.getTxAndAccount(w, r)
	if !ok { return }

	tenantIDStr, ok := r.Context().Value(middleware.CtxKeyTenantID).(string)
	if !ok || tenantIDStr == "" {
		s.LogError(r, errors.New("tenant ID missing from context"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}
	tenantID, errConv := strconv.ParseInt(tenantIDStr, 10, 64)
	if errConv != nil {
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "invalid tenant id"})
		return
	}

	stats, err := s.EventService.GetEventsRevenueStats(tx, tenantID)
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to get stats: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "could not get stats"})
		return
	}

	s.JSON(w, r, http.StatusOK, envelope{"stats": stats})
}