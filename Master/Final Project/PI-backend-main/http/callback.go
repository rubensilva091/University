package http

import (
	"database/sql"
	"errors"
	"fmt"
	"net/http"
	"strconv"
	"strings"

	"github.com/go-chi/chi/v5"
	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/pdf"
	"github.com/jmoiron/sqlx"
	"go.uber.org/zap"
)

func (s *Server) registerCallbackRoutes(r chi.Router) {
	r.Get("/callback", s.handleCallback)
}

// handleCallback determina qual handler específico chamar.
func (s *Server) handleCallback(w http.ResponseWriter, r *http.Request) {
	var mbParams npadmin.MBCallbackParams
	var mbwayParams npadmin.MBWayCallbackParams

	qs := r.URL.Query()
	if err := s.decoder.Decode(&mbParams, qs); err == nil {
		if mbParams.Entity != "" && mbParams.Reference != "" && mbParams.Amount != "" && mbParams.Key != "" {
			s.handleMBCallback(w, r, mbParams)
			return
		}
	}

	if err := s.decoder.Decode(&mbwayParams, qs); err == nil {
		if mbwayParams.RequestID != "" && mbwayParams.Amount != "" && mbwayParams.Key != "" {
			s.handleMBWayCallback(w, r, mbwayParams)
			return
		}
	}

	s.LogWarn(r, fmt.Errorf("could not parse callback params: %s", qs.Encode()))
	s.JSON(w, r, http.StatusBadRequest, envelope{"error": "callback parameters could not be parsed or matched"})
}

func (s *Server) handleMBCallback(w http.ResponseWriter, r *http.Request, params npadmin.MBCallbackParams) {
	// 1. Iniciar Transação
	dbx := sqlx.NewDb(s.db, "postgres")
	tx, err := dbx.BeginTxx(r.Context(), &sql.TxOptions{})
	if err != nil {
		s.LogError(r, fmt.Errorf("failed to begin MB callback transaction: %w", err))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"error": "internal server error"})
		return
	}
	var committed bool = false
	defer func() {
		if !committed {
			_ = tx.Rollback()
		}
	}()

	// 2. Validar Chave (Anti-phishing)
	if !s.validatePhishingKey(params.Key) {
		s.LogWarn(r, fmt.Errorf("invalid anti-phishing key in MB callback: %s", params.Key))
		s.JSON(w, r, http.StatusUnprocessableEntity, envelope{"error": "invalid key"})
		return
	}

	// 3. Validar Status do Pagamento
	if strings.ToLower(params.Status) != "pago" && strings.ToLower(params.Status) != "paid" {
		s.LogWarn(r, fmt.Errorf("MB callback received non-paid status: %s", params.Status))
		s.JSON(w, r, http.StatusOK, envelope{"message": "payment not successful"})
		return
	}

	// 4. TENTAR PRIMEIRO: Isto é um Bilhete de Evento?
	ticket, errTicket := s.EventService.ConfirmTicketByReference(tx, params.Reference)
	if errTicket == nil {
		s.Log.Info("Multibanco Callback processed for Event Ticket", zap.Int64("ticketID", ticket.ID))

		if err := tx.Commit(); err != nil {
			s.LogError(r, fmt.Errorf("failed to commit MB callback transaction for Event Ticket: %w", err))
			s.JSON(w, r, http.StatusInternalServerError, envelope{"error": "internal server error during commit"})
			return
		}
		committed = true

		// --- SUCESSO: Enviar PDF ---
		// Removemos o 'dbx' daqui. A função cria a sua própria transação.
		go s.sendTicketEmail(ticket)
		// ---------------------------

		s.JSON(w, r, http.StatusOK, envelope{"message": "callback processed successfully for event ticket"})
		return
	}

	// 5. SE NÃO FOR UM BILHETE, TENTAR: Isto é uma Quota?
	accountID, period, tenantID, errQuota := s.PaymentService.MultibancoPaid(tx, params.Reference)
	if errQuota == nil {
		s.Log.Info("Multibanco Callback processed for Quota", zap.Int64("accountID", accountID), zap.Int64("tenantID", tenantID))

		tenantIDStr := strconv.FormatInt(tenantID, 10)
		accountIDStr := strconv.FormatInt(accountID, 10)
		
		// Set Tenant Context
		if _, err := tx.Exec(fmt.Sprintf("SELECT set_config('myapp.tenant_id', '%s', true)", tenantIDStr)); err != nil {
			s.LogError(r, fmt.Errorf("failed to set tenant context for quota: %w", err))
			s.JSON(w, r, http.StatusInternalServerError, envelope{"error": "internal security error"})
			return
		}
		// Set User Context
		if _, err := tx.Exec(fmt.Sprintf("SELECT set_config('myapp.user_id', '%s', true)", accountIDStr)); err != nil {
			s.LogError(r, fmt.Errorf("failed to set user context for quota: %w", err))
			s.JSON(w, r, http.StatusInternalServerError, envelope{"error": "internal security error"})
			return
		}

		price, _ := strconv.ParseFloat(params.Amount, 64)
		err = s.addSubscription(tx, accountID, period, price)
		if err != nil {
			s.LogError(r, fmt.Errorf("failed to add subscription after MB payment (ref: %s): %w", params.Reference, err))
			s.JSON(w, r, http.StatusOK, envelope{"message": "processed but subscription update failed"})
			return
		}

		acc, err := s.AccountService.Get(tx, accountID)
		if err == nil {
			authenticationView := npadmin.AccountView{Endpoint: s.cfg.WebApp.AssociateDashboard}
			s.Mailer.SendPaymentSuccessful(acc.Email, authenticationView)
		}

		if err := tx.Commit(); err != nil {
			s.LogError(r, fmt.Errorf("failed to commit MB callback transaction for Quota: %w", err))
			s.JSON(w, r, http.StatusInternalServerError, envelope{"error": "internal server error during commit"})
			return
		}
		committed = true
		s.JSON(w, r, http.StatusOK, envelope{"message": "callback processed successfully for quota"})
		return
	}

	s.Log.Warn("MB callback ref matched no ticket and no quota",
		zap.String("reference", params.Reference),
		zap.Error(errTicket),
		zap.Error(errQuota))
	s.JSON(w, r, http.StatusOK, envelope{"message": "reference not found or already processed"})
}

func (s *Server) handleMBWayCallback(w http.ResponseWriter, r *http.Request, params npadmin.MBWayCallbackParams) {
	dbx := sqlx.NewDb(s.db, "postgres")
	tx, err := dbx.BeginTxx(r.Context(), &sql.TxOptions{})
	if err != nil {
		s.LogError(r, errors.New("failed to begin MBWay callback transaction"))
		s.JSON(w, r, http.StatusInternalServerError, envelope{"error": "internal server error"})
		return
	}
	var committed bool = false
	defer func() {
		if !committed {
			_ = tx.Rollback()
		}
	}()

	if !s.validatePhishingKey(params.Key) {
		s.LogWarn(r, fmt.Errorf("invalid anti-phishing key in MBWay callback: %s", params.Key))
		s.JSON(w, r, http.StatusUnprocessableEntity, envelope{"error": "invalid key"})
		return
	}

	if strings.ToLower(params.Status) != "pago" && strings.ToLower(params.Status) != "paid" {
		s.LogWarn(r, fmt.Errorf("MBWay callback received non-paid status: %s", params.Status))
		s.JSON(w, r, http.StatusOK, envelope{"message": "payment not successful"})
		return
	}

	ticket, errTicket := s.EventService.ConfirmTicketByRequestID(tx, params.RequestID)
	if errTicket == nil {
		s.Log.Info("MBWay Callback processed for Event Ticket", zap.Int64("ticketID", ticket.ID))

		if err := tx.Commit(); err != nil {
			s.LogError(r, fmt.Errorf("failed to commit MBWay callback transaction for Event Ticket: %w", err))
			s.JSON(w, r, http.StatusInternalServerError, envelope{"error": "internal server error during commit"})
			return
		}
		committed = true

		// --- SUCESSO: Enviar PDF ---
		go s.sendTicketEmail(ticket)
		// ---------------------------

		s.JSON(w, r, http.StatusOK, envelope{"message": "callback processed successfully for event ticket"})
		return
	}

	accountID, period, tenantID, errQuota := s.PaymentService.MBWayPaid(tx, params.RequestID)
	if errQuota == nil {
		s.Log.Info("MBWay Callback processed for Quota", zap.Int64("accountID", accountID), zap.Int64("tenantID", tenantID))

		tenantIDStr := strconv.FormatInt(tenantID, 10)
		accountIDStr := strconv.FormatInt(accountID, 10)
		if _, err := tx.Exec(fmt.Sprintf("SELECT set_config('myapp.tenant_id', '%s', true)", tenantIDStr)); err != nil {
			s.LogError(r, fmt.Errorf("failed to set tenant context for mbway quota: %w", err))
			s.JSON(w, r, http.StatusInternalServerError, envelope{"error": "internal security error"})
			return
		}
		if _, err := tx.Exec(fmt.Sprintf("SELECT set_config('myapp.user_id', '%s', true)", accountIDStr)); err != nil {
			s.LogError(r, fmt.Errorf("failed to set user context for mbway quota: %w", err))
			s.JSON(w, r, http.StatusInternalServerError, envelope{"error": "internal security error"})
			return
		}

		price, _ := strconv.ParseFloat(params.Amount, 64)
		err = s.addSubscription(tx, accountID, period, price)
		if err != nil {
			s.LogError(r, fmt.Errorf("failed to add subscription after MBWay payment (reqID: %s): %w", params.RequestID, err))
			s.JSON(w, r, http.StatusOK, envelope{"message": "processed but subscription update failed"})
			return
		}

		acc, err := s.AccountService.Get(tx, accountID)
		if err == nil {
			data := map[string]string{"status": "paid"}
			_ = s.sendNotification(npadmin.PushNotification{Channel: fmt.Sprintf("%d-mbway", accountID), Data: data})
			authenticationView := npadmin.AccountView{Endpoint: s.cfg.WebApp.AssociateDashboard}
			s.Mailer.SendPaymentSuccessful(acc.Email, authenticationView)
		}

		if err := tx.Commit(); err != nil {
			s.LogError(r, fmt.Errorf("failed to commit MBWay callback transaction for Quota: %w", err))
			s.JSON(w, r, http.StatusInternalServerError, envelope{"error": "internal server error during commit"})
			return
		}
		committed = true
		s.JSON(w, r, http.StatusOK, envelope{"message": "callback processed successfully for quota"})
		return
	}

	s.Log.Warn("MBWay callback reqID matched no ticket and no quota",
		zap.String("request_id", params.RequestID),
		zap.Error(errTicket),
		zap.Error(errQuota))
	s.JSON(w, r, http.StatusOK, envelope{"message": "request id not found or already processed"})
}

func (s *Server) validatePhishingKey(key string) bool {
	return key == s.Payment.AntiPhishingKey
}

func (s *Server) sendNotification(n npadmin.PushNotification) error {
	err := s.PusherClient.Trigger(n.Channel, "mbway", n.Data)
	if err != nil {
		s.Log.Error("failed to trigger push notification", zap.Error(err), zap.String("channel", n.Channel))
		return fmt.Errorf("failed to trigger push notification: %w", err)
	}
	s.Log.Info("Push notification triggered", zap.String("channel", n.Channel))
	return nil
}

// --- Helper: Gerar e Enviar PDF do Bilhete ---
// Agora cria a sua própria transação para garantir que a conexão tem o contexto RLS
func (s *Server) sendTicketEmail(ticket *npadmin.PurchasedTicket) {
	// 1. Iniciar Nova Transação Dedicada (Leitura)
	dbx := sqlx.NewDb(s.db, "postgres")
	tx, err := dbx.Beginx()
	if err != nil {
		s.Log.Error("failed to begin tx for ticket email", zap.Error(err))
		return
	}
	defer tx.Rollback() // Rollback automático no final (é só leitura)

	// 2. Configurar o Contexto RLS *nesta* transação
	_, errRls := tx.Exec(fmt.Sprintf(
		"SELECT set_config('myapp.tenant_id', '%d', true), set_config('myapp.user_id', '%d', true)",
		ticket.TenantID, ticket.AccountID,
	))
	if errRls != nil {
		s.Log.Error("failed to set RLS context for ticket email", zap.Error(errRls))
		return
	}

	// 3. Obter Email da Conta (Global) - Usa 'tx'
	account, err := s.AccountService.Get(tx, ticket.AccountID)
	if err != nil {
		s.Log.Error("failed to get account for ticket email", zap.Error(err))
		return
	}

	// 4. Obter Nome do Perfil (Tenant Scoped) - Usa 'tx'
	profile, err := s.ProfileService.Get(tx, ticket.AccountID)
	purchaserName := "Cliente"
	if err == nil && profile != nil {
		purchaserName = fmt.Sprintf("%s %s", profile.FirstName.String, profile.LastName.String)
	}

	// 5. Obter Detalhes do Evento (Tenant Scoped) - Usa 'tx'
	event, _, err := s.EventService.GetPublicEventDetails(tx, ticket.EventID)
	if err != nil {
		s.Log.Error("failed to get event for ticket email", zap.Error(err))
		return
	}

	// 6. Gerar PDF
	pdfPath, err := pdf.GenerateTicketPDF(ticket, event.Name, purchaserName)
	if err != nil {
		s.Log.Error("failed to generate ticket PDF", zap.Error(err))
		return
	}

	// 7. Enviar Email
	data := map[string]string{
		"EventName":     event.Name,
		"EventDate":     event.EventDate.Format("02/01/2006 15:04"),
		"EventLocation": event.Location,
		"TicketID":      strconv.FormatInt(ticket.ID, 10),
		"PurchaserName": purchaserName,
	}
	s.Mailer.SendEventTicketEmail(account.Email, data, pdfPath)
}