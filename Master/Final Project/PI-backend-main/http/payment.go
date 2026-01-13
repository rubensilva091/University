package http

import (
	"database/sql"
	"errors"
	"fmt"
	"net/http"
	"strconv"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
	"github.com/invisiblelab-dev/npadmin/http/middleware" // Use alias
	"github.com/invisiblelab-dev/npadmin/payment"
)

func (s *Server) registerPaymentRoutes(r chi.Router) {
	// Rotas protegidas que precisam de acesso à BD
	r.Route("/payments", func(r chi.Router) {
		r.Use(middleware.JwtVerifier(crypto.ParseAuthorizationHeader))
		r.Use(middleware.JwtAuthorize(s.cfg.JwtPublicKey))
		
		// --- ORDEM CORRIGIDA ---
		// 1. TenantTxMiddleware: Inicia a transação (tx) e define o tenant_id + user_id na sessão RLS.
		r.Use(middleware.TenantTxMiddleware(s.db))
		// 2. AccountCtx: Usa a transação (tx) para buscar a conta global e colocá-la no contexto.
		r.Use(middleware.AccountCtx(s.AccountService))
		// --- FIM DA CORREÇÃO ---

		r.Get("/prices", s.handleAssociatePrices)
		r.Post("/multibanco", s.handleMultibancoPayment)
		r.Post("/mbway", s.handleMBWayPayment)
	})

	// Rotas públicas que precisam de acesso à BD (mas não de autenticação)
	// Estas usam o TenantTxMiddleware individualmente (correto).
	r.With(middleware.TenantTxMiddleware(s.db)).Get("/payments/periods", s.handleGetPeriods)
	r.With(middleware.TenantTxMiddleware(s.db)).Get("/payments/categories", s.handleGetCategories)
}

func (s *Server) handleMultibancoPayment(w http.ResponseWriter, r *http.Request) {
	var params npadmin.MultibancoParams
	if err := DecodeJSON(w, r, &params); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}

	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}

	// Obter conta do contexto (definida pelo AccountCtx)
	account, ok := r.Context().Value(middleware.CtxKeyAccount).(*npadmin.Account)
	if !ok {
		s.JSON(w, r, http.StatusUnauthorized, "Unauthorized - Account context missing")
		return
	}

	// Extrair transação 'tx' (definida pelo TenantTxMiddleware)
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleMultibancoPayment"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	// Obter perfil (dentro do RLS de 'tx')
	profile, err := s.ProfileService.Get(tx, account.ID)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err) // Pode ser NotFound se o perfil não existir neste tenant
		return
	}

	// Obter preço (dentro do RLS de 'tx')
	price, err := s.getPrice(tx, profile.Category, params.Period)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			s.JSON(w, r, http.StatusNotFound, envelope{"message": "failed to find price for given category/period"})
		} else {
			s.LogError(r, err)
			s.JSON(w, r, http.StatusInternalServerError, err)
		}
		return
	}

	// Obter próxima encomenda (dentro do RLS de 'tx')
	order, err := s.PaymentService.GetNextOrderNumber(tx)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}

	// Lógica do provider (não precisa de tx)
	reqMBRef := payment.ReqMBReference{
		MBKey:       s.Payment.MBKey,
		MBURL:       s.Payment.MBURL,
		Entidade:    s.Payment.Entidade,
		SubEntidade: s.Payment.SubEntidade,
		Amount:      price,
		AccountID:   account.ID,
		OrderID:     order,
	}
	mbReference, err := s.Payment.MBReference(reqMBRef)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}
	mbReference.OrderID = fmt.Sprint(order)

	// Inserir registo Multibanco (dentro do RLS de 'tx')
	// O serviço InsertMultibanco agora obtém o tenant_id de 'tx'
	err = s.PaymentService.InsertMultibanco(tx, mbReference, account.ID, params.Period)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}

	s.JSON(w, r, http.StatusOK,
		envelope{
			"reference": mbReference.Reference,
			"entity":    mbReference.Entity,
			"amount":    mbReference.Amount,
		},
	)
}

func (s *Server) handleMBWayPayment(w http.ResponseWriter, r *http.Request) {
	var params npadmin.MBWayParams
	if err := DecodeJSON(w, r, &params); err != nil {
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": err.Error()})
		return
	}

	if err := s.validate.Struct(params); err != nil {
		s.JSON(w, r, http.StatusUnprocessableEntity, err)
		return
	}

	account, ok := r.Context().Value(middleware.CtxKeyAccount).(*npadmin.Account)
	if !ok {
		s.JSON(w, r, http.StatusUnauthorized, "Unauthorized - Account context missing")
		return
	}

	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleMBWayPayment"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	profile, err := s.ProfileService.Get(tx, account.ID)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}

	price, err := s.getPrice(tx, profile.Category, params.Period)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			s.JSON(w, r, http.StatusNotFound, envelope{"message": "failed to find price for given category/period"})
		} else {
			s.LogError(r, err)
			s.JSON(w, r, http.StatusInternalServerError, err)
		}
		return
	}

	order, err := s.PaymentService.GetNextOrderNumber(tx)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}

	reqMBWayPaym := payment.ReqMBWayPayment{
		MBWayKey:    s.Payment.MBWayKey,
		Amount:      strconv.FormatFloat(price, 'f', 2, 64),
		Description: getPaymentDescription(profile.Category, params.Period),
		TLM:         params.TLM,
		Email:       account.Email,
		Channel:     "03",
		OrderID:     fmt.Sprint(order),
		MBWayURL:    s.Payment.MBWayURL,
	}
	mbWayPayment, err := s.Payment.MBWayPayment(reqMBWayPaym)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}

	status, ok := payment.ParsePaymentStatus(mbWayPayment.Status)
	if !ok || status != payment.Success {
		s.LogWarn(r, fmt.Errorf("MBWay payment request failed or pending with status: %s, description: %s", mbWayPayment.Status, mbWayPayment.MsgDescription))
		s.JSON(w, r, http.StatusBadRequest, envelope{"message": fmt.Sprintf("MBWay payment request failed: %s", mbWayPayment.MsgDescription)})
		return
	}
	mbWayPayment.OrderID = fmt.Sprint(order)

	// O serviço InsertMBWay agora obtém o tenant_id de 'tx'
	err = s.PaymentService.InsertMBWay(tx, mbWayPayment, account.ID, params.Period)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}

	s.JSON(w, r, http.StatusOK, envelope{"orderID": mbWayPayment.OrderID})
}

func getPaymentDescription(category int64, period int64) string {
	return fmt.Sprintf("Pagamento Quota JAM %d meses", period)
}

func (s *Server) getPrice(q npadmin.Queryable, categoryID int64, period int64) (float64, error) {
	price, err := s.DataService.GetPrice(q, categoryID, period)
	if err != nil {
		return 0, fmt.Errorf("failed to get price: %w", err)
	}
	return price, nil
}

func (s *Server) handleAssociatePrices(w http.ResponseWriter, r *http.Request) {
	account, ok := r.Context().Value(middleware.CtxKeyAccount).(*npadmin.Account)
	if !ok {
		s.JSON(w, r, http.StatusUnauthorized, "Unauthorized")
		return
	}

	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleAssociatePrices"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	profile, err := s.ProfileService.Get(tx, account.ID)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}

	category := profile.Category

	prices, err := s.DataService.GetPrices(tx, category)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}

	s.JSON(w, r, http.StatusOK, envelope{"prices": prices})
}

func (s *Server) handleGetPeriods(w http.ResponseWriter, r *http.Request) {
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleGetPeriods"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	periods, err := s.DataService.GetPeriods(tx)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"availablePeriodsInMonths": periods})
}

func (s *Server) handleGetCategories(w http.ResponseWriter, r *http.Request) {
	tx, ok := r.Context().Value(middleware.CtxKeyTx).(npadmin.Queryable)
	if !ok {
		s.LogError(r, errors.New("database transaction missing in handleGetCategories"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"message": "internal server error"})
		return
	}

	categories, err := s.DataService.GetCategories(tx)
	if err != nil {
		s.LogError(r, err)
		s.JSON(w, r, http.StatusInternalServerError, err)
		return
	}
	s.JSON(w, r, http.StatusOK, envelope{"availableCategories": categories})
}

// addSubscription (helper) - usa 'q' (com RLS)
func (s *Server) addSubscription(q npadmin.Queryable, accountID int64, period int64, price float64) error {
	var startDate time.Time
	currentSubscription, err := s.DataService.CurrentSubscription(q, accountID)
	if err != nil {
		if !errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("failed to get current subscription: %w", err)
		}
		startDate = time.Now().UTC()
	} else {
		if currentSubscription.EndDate.Before(time.Now().UTC()) {
			startDate = time.Now().UTC()
		} else {
			startDate = currentSubscription.EndDate.Add(1 * time.Second)
		}
	}

	endDate := startDate.AddDate(0, int(period), 0)

	err = s.PaymentService.SetPaymentSuccessful(q, accountID, price, startDate, endDate)
	if err != nil {
		return fmt.Errorf("failed to set payment successful: %w", err)
	}
	return nil
}

// addSubscriptionManually (helper) - usa 'q' (com RLS)
func (s *Server) addSubscriptionManually(q npadmin.Queryable, accountID int64, period int64, price float64, startDate time.Time) error {
	currentSubscription, err := s.DataService.CurrentSubscription(q, accountID)
	if err != nil {
		if !errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("failed to get current subscription: %w", err)
		}
	}

	endDate := startDate.AddDate(0, int(period), 0)

	err = s.PaymentService.InsertSubscriptionsHistory(q, accountID, price, startDate, endDate)
	if err != nil {
		return fmt.Errorf("failed to insert subscription history: %w", err)
	}

	if currentSubscription == nil || currentSubscription.EndDate.Before(endDate) {
		err = s.PaymentService.InsertCurrentSubscription(q, accountID, price, startDate, endDate)
		if err != nil {
			return fmt.Errorf("failed to insert/update current subscription: %w", err)
		}
	}
	return nil
}