package postgres

import (
	"database/sql"
	"errors"
	"fmt"
	"strconv"

	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
	"github.com/invisiblelab-dev/npadmin/payment"
)

type EventService struct {
	db             *DB
	PaymentService npadmin.PaymentService
	PaymentConfig  payment.Payment
}

func NewEventService(db *DB, ps npadmin.PaymentService, pc payment.Payment) *EventService {
	return &EventService{
		db:             db,
		PaymentService: ps,
		PaymentConfig:  pc,
	}
}

func getRlsContext(q npadmin.Queryable) (tenantID int64, userID int64, err error) {
	if err := q.QueryRow("SELECT current_setting('myapp.tenant_id', true)::bigint").Scan(&tenantID); err != nil {
		return 0, 0, fmt.Errorf("failed to retrieve current tenant ID from RLS session: %w", err)
	}
	if err := q.QueryRow("SELECT current_setting('myapp.user_id', true)::bigint").Scan(&userID); err != nil {
		return 0, 0, fmt.Errorf("failed to retrieve current user ID from RLS session: %w", err)
	}
	return tenantID, userID, nil
}

// --- Funções de Admin e Leitura ---

func (s EventService) CreateEvent(q npadmin.Queryable, event *npadmin.Event) error {
	// Remover estas linhas que causavam o bug:
	// tenantID, _, err := getRlsContext(q)
	// if err != nil { return err }
	// event.TenantID = tenantID 

	// Validar segurança básica
	if event.TenantID == 0 {
		return fmt.Errorf("tenant ID is required to create an event")
	}

	query := `
		INSERT INTO events (tenant_id, name, description, event_date, location, image_url)
		VALUES ($1, $2, $3, $4, $5, $6)
		RETURNING id, created_at, updated_at`

	err := q.QueryRow(query, event.TenantID, event.Name, event.Description, event.EventDate, event.Location, event.ImageURL).
		Scan(&event.ID, &event.CreatedAt, &event.UpdatedAt)
	if err != nil {
		return fmt.Errorf("failed to create event in database: %w", err)
	}
	return nil
}

func (s EventService) CreateTicketDefinition(q npadmin.Queryable, def *npadmin.EventTicketDefinition) error {
	
	if def.TenantID == 0 {
		return fmt.Errorf("tenant ID is required to create ticket definition")
	}

	query := `
		INSERT INTO event_ticket_definitions (tenant_id, event_id, name, price, available_quantity)
		VALUES ($1, $2, $3, $4, $5)
		RETURNING id, created_at`

	err := q.QueryRow(query, def.TenantID, def.EventID, def.Name, def.Price, def.AvailableQuantity).
		Scan(&def.ID, &def.CreatedAt)
	if err != nil {
		return fmt.Errorf("failed to create ticket definition: %w", err)
	}
	return nil
}

func (s EventService) GetAdminEventDetails(q npadmin.Queryable, eventID int64) (*npadmin.Event, []npadmin.EventTicketDefinition, error) {
	var event npadmin.Event
	err := q.Get(&event, "SELECT * FROM events WHERE id = $1", eventID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil, fmt.Errorf("event not found or access denied")
		}
		return nil, nil, fmt.Errorf("failed to get event: %w", err)
	}

	var definitions []npadmin.EventTicketDefinition
	err = q.Select(&definitions, "SELECT * FROM event_ticket_definitions WHERE event_id = $1 ORDER BY price", eventID)
	if err != nil {
		return nil, nil, fmt.Errorf("failed to get ticket definitions: %w", err)
	}

	return &event, definitions, nil
}

func (s EventService) GetAdminPurchasedTicketsForEvent(q npadmin.Queryable, eventID int64) ([]npadmin.PurchasedTicket, error) {
	var tickets []npadmin.PurchasedTicket
	query := `
		SELECT 
			pt.*,
			e.name AS event_name, 
			etd.name AS ticket_name, 
			etd.price AS ticket_price,
			a.email AS purchaser_email
		FROM purchased_tickets pt
		JOIN events e ON pt.event_id = e.id
		JOIN event_ticket_definitions etd ON pt.definition_id = etd.id
		JOIN accounts a ON pt.account_id = a.id
		WHERE pt.event_id = $1
		ORDER BY pt.created_at DESC`

	err := q.Select(&tickets, query, eventID)
	if err != nil {
		return nil, fmt.Errorf("failed to get purchased tickets for event: %w", err)
	}
	return tickets, nil
}

func (s EventService) ListAvailableEvents(q npadmin.Queryable) ([]npadmin.Event, error) {
	var events []npadmin.Event
	
	// CORREÇÃO: JOIN com tenants para obter o nome
	query := `
		SELECT e.*, t.name as tenant_name
		FROM events e
		JOIN tenants t ON e.tenant_id = t.id
		WHERE e.event_date >= now() 
		-- A RLS já filtra pelo tenant atual em 'e', mas o JOIN garante dados corretos
		ORDER BY e.event_date ASC
	`
	
	err := q.Select(&events, query)
	if err != nil {
		return nil, fmt.Errorf("failed to list available events: %w", err)
	}
	return events, nil
}

func (s EventService) GetPublicEventDetails(q npadmin.Queryable, eventID int64) (*npadmin.Event, []npadmin.EventTicketDefinition, error) {
	return s.GetAdminEventDetails(q, eventID)
}

func (s EventService) GetMyTickets(q npadmin.Queryable, accountID int64) ([]npadmin.PurchasedTicket, error) {
	var tickets []npadmin.PurchasedTicket
	query := `
		SELECT 
			pt.*,
			e.name AS event_name, 
			e.event_date AS event_date,
			etd.name AS ticket_name, 
			etd.price AS ticket_price
		FROM purchased_tickets pt
		JOIN events e ON pt.event_id = e.id
		JOIN event_ticket_definitions etd ON pt.definition_id = etd.id
		WHERE pt.account_id = $1
		ORDER BY e.event_date DESC`

	err := q.Select(&tickets, query, accountID)
	if err != nil {
		return nil, fmt.Errorf("failed to get user tickets: %w", err)
	}
	return tickets, nil
}

func (s EventService) GetMyTicketDetails(q npadmin.Queryable, ticketID int64, accountID int64) (*npadmin.PurchasedTicket, error) {
	var ticket npadmin.PurchasedTicket
	query := `
		SELECT 
			pt.*,
			e.name AS event_name, 
			e.event_date AS event_date,
			etd.name AS ticket_name, 
			etd.price AS ticket_price
		FROM purchased_tickets pt
		JOIN events e ON pt.event_id = e.id
		JOIN event_ticket_definitions etd ON pt.definition_id = etd.id
		WHERE pt.id = $1 AND pt.account_id = $2`

	err := q.Get(&ticket, query, ticketID, accountID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, fmt.Errorf("ticket not found or access denied")
		}
		return nil, fmt.Errorf("failed to get ticket details: %w", err)
	}
	return &ticket, nil
}

// --- Funções de Pagamento e Confirmação ---

func (s EventService) InitiateTicketPurchase(q npadmin.Queryable, accountID int64, definitionID int64, paymentType string, mbwayTlm string, userEmail string) (*npadmin.PurchaseResponse, error) {

	tenantID, userID, err := getRlsContext(q)
	if err != nil {
		return nil, err
	}
	if userID != accountID {
		return nil, fmt.Errorf("user ID mismatch in purchase")
	}

	var def npadmin.EventTicketDefinition
	err = q.Get(&def, "SELECT * FROM event_ticket_definitions WHERE id = $1 AND tenant_id = $2", definitionID, tenantID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, fmt.Errorf("ticket type not found")
		}
		return nil, fmt.Errorf("database error getting ticket definition: %w", err)
	}

	if def.AvailableQuantity <= 0 {
		return nil, fmt.Errorf("tickets sold out")
	}

	orderID, err := s.PaymentService.GetNextOrderNumber(q)
	if err != nil {
		return nil, fmt.Errorf("could not get order number: %w", err)
	}

	ticket := &npadmin.PurchasedTicket{
		TenantID:       tenantID,
		EventID:        def.EventID,
		DefinitionID:   definitionID,
		AccountID:      accountID,
		Status:         "pending",
		PaymentOrderID: npadmin.NewNullInt64(orderID),
	}

	queryInsert := `
		INSERT INTO purchased_tickets (tenant_id, event_id, definition_id, account_id, status, payment_order_id)
		VALUES ($1, $2, $3, $4, $5, $6)
		RETURNING id, created_at, updated_at`

	err = q.QueryRow(
		queryInsert,
		ticket.TenantID, ticket.EventID, ticket.DefinitionID, ticket.AccountID, ticket.Status, ticket.PaymentOrderID,
	).Scan(&ticket.ID, &ticket.CreatedAt, &ticket.UpdatedAt)
	if err != nil {
		return nil, fmt.Errorf("failed to create pending ticket: %w", err)
	}

	resp := &npadmin.PurchaseResponse{Ticket: ticket, PaymentType: paymentType}
	amountStr := fmt.Sprintf("%.2f", def.Price)

	if paymentType == "multibanco" {
		reqMBRef := payment.ReqMBReference{
			Entidade:    s.PaymentConfig.Entidade,
			SubEntidade: s.PaymentConfig.SubEntidade,
			OrderID:     orderID,
			Amount:      def.Price,
		}
		mbRef, err := s.PaymentConfig.MBReference(reqMBRef)
		if err != nil {
			return nil, fmt.Errorf("failed to generate MB reference: %w", err)
		}

		_, err = q.Exec("UPDATE purchased_tickets SET payment_reference = $1 WHERE id = $2", mbRef.Reference, ticket.ID)
		if err != nil { /* Aviso */
		}

		resp.Entity = mbRef.Entity
		resp.Reference = mbRef.Reference
		resp.Amount = mbRef.Amount

	} else if paymentType == "mbway" {
		reqMBWay := payment.ReqMBWayPayment{
			XMLNS:       "https://www.ifthenpay.com/",
			MBWayKey:    s.PaymentConfig.MBWayKey,
			Channel:     "03",
			OrderID:     strconv.FormatInt(orderID, 10),
			Amount:      amountStr,
			TLM:         mbwayTlm,
			Email:       userEmail,
			Description: fmt.Sprintf("Bilhete %d", ticket.ID),
		}
		mbwResp, err := s.PaymentConfig.MBWayPayment(reqMBWay)
		if err != nil {
			return nil, fmt.Errorf("failed to create MBWay request: %w", err)
		}
		if mbwResp.Status != "000" {
			return nil, fmt.Errorf("MBWay request failed (%s): %s", mbwResp.Status, mbwResp.MsgDescription)
		}

		_, err = q.Exec("UPDATE purchased_tickets SET payment_request_id = $1 WHERE id = $2", mbwResp.RequestID, ticket.ID)
		if err != nil { /* Aviso */
		}

		resp.MBWayRequestID = mbwResp.RequestID
		resp.Amount = amountStr
	} else {
		return nil, fmt.Errorf("payment type '%s' not supported", paymentType)
	}

	return resp, nil
}

func (s EventService) ConfirmTicketByReference(q npadmin.Queryable, reference string) (*npadmin.PurchasedTicket, error) {
	var ticket npadmin.PurchasedTicket

	// 1. Buscar bilhete usando Função Segura (Bypass RLS para encontrar o Tenant)
	querySelect := `SELECT * FROM get_ticket_by_reference_safe($1)`
	err := q.Get(&ticket, querySelect, reference)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, fmt.Errorf("pending ticket not found for this reference")
		}
		return nil, fmt.Errorf("failed to find ticket by reference: %w", err)
	}

	// 2. Configurar RLS (Necessário para o UPDATE funcionar)
	_, err = q.Exec(fmt.Sprintf(
		"SELECT set_config('myapp.tenant_id', '%d', true), set_config('myapp.user_id', '%d', true)",
		ticket.TenantID, ticket.AccountID,
	))
	if err != nil {
		return nil, fmt.Errorf("failed to set RLS context: %w", err)
	}

	// 3. Atualizar estado para PAID
	qrToken := crypto.GetRandomID()
	queryUpdate := `
		UPDATE purchased_tickets 
		SET status = 'paid', 
			qr_code_token = $1,
			updated_at = now() at time zone 'utc'
		WHERE id = $2
		RETURNING updated_at`

	err = q.QueryRow(queryUpdate, qrToken, ticket.ID).Scan(&ticket.UpdatedAt)
	if err != nil {
		return nil, fmt.Errorf("failed to update ticket status: %w", err)
	}
	ticket.Status = "paid"
	ticket.QRCodeToken = npadmin.NewNullString(qrToken)

	// 4. Decrementar Stock usando a Função de Segurança
	_, err = q.Exec("SELECT decrement_ticket_stock($1, $2)", ticket.DefinitionID, ticket.TenantID)
	if err != nil {
		return nil, fmt.Errorf("failed to decrement stock (sold out?): %w", err)
	}

	return &ticket, nil
}

func (s EventService) ConfirmTicketByRequestID(q npadmin.Queryable, requestID string) (*npadmin.PurchasedTicket, error) {
	var ticket npadmin.PurchasedTicket

	// 1. Buscar bilhete usando Função Segura
	querySelect := `SELECT * FROM get_ticket_by_request_id_safe($1)`
	err := q.Get(&ticket, querySelect, requestID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, fmt.Errorf("pending ticket not found for this request ID")
		}
		return nil, fmt.Errorf("failed to find ticket by request ID: %w", err)
	}

	// 2. Configurar RLS
	_, err = q.Exec(fmt.Sprintf(
		"SELECT set_config('myapp.tenant_id', '%d', true), set_config('myapp.user_id', '%d', true)",
		ticket.TenantID, ticket.AccountID,
	))
	if err != nil {
		return nil, fmt.Errorf("failed to set RLS context: %w", err)
	}

	// 3. Atualizar estado
	qrToken := crypto.GetRandomID()
	queryUpdate := `
		UPDATE purchased_tickets 
		SET status = 'paid', 
			qr_code_token = $1,
			updated_at = now() at time zone 'utc'
		WHERE id = $2
		RETURNING updated_at`

	err = q.QueryRow(queryUpdate, qrToken, ticket.ID).Scan(&ticket.UpdatedAt)
	if err != nil {
		return nil, fmt.Errorf("failed to update ticket status: %w", err)
	}
	ticket.Status = "paid"
	ticket.QRCodeToken = npadmin.NewNullString(qrToken)

	// 4. Decrementar Stock
	_, err = q.Exec("SELECT decrement_ticket_stock($1, $2)", ticket.DefinitionID, ticket.TenantID)
	if err != nil {
		return nil, fmt.Errorf("failed to decrement stock (sold out?): %w", err)
	}

	return &ticket, nil
}

// GetAllEvents (NOVO) lista TODOS os eventos (ignorando RLS) via função segura.
// Usado para a listagem global/pública de eventos.
func (s EventService) GetAllEvents(q npadmin.Queryable) ([]npadmin.Event, error) {
	// 1. Buscar os eventos brutos (sem nome do tenant)
	var events []npadmin.Event
	queryEvents := `SELECT * FROM get_all_events_safe()` // A tua função SQL simples
	err := q.Select(&events, queryEvents)
	if err != nil {
		return nil, fmt.Errorf("failed to list all events via safe function: %w", err)
	}

	// Se não houver eventos, retornamos já para evitar trabalho extra
	if len(events) == 0 {
		return events, nil
	}

	// 2. "Hidratar" os nomes dos tenants (Workaround para não mexer em Migrations)
	// Vamos buscar ID e Nome de todos os tenants para mapear.
	// Nota: Isto assume que a tabela 'tenants' é legível nesta transação.
	type TenantSimple struct {
		ID   int64  `db:"id"`
		Name string `db:"name"`
	}
	var tenants []TenantSimple
	// Buscamos apenas o necessário
	if err := q.Select(&tenants, "SELECT id, name FROM tenants"); err != nil {
		// Se falhar (ex: erro de permissão), não faz mal, retornamos os eventos sem nome
		// Mas logamos o erro internamente se possível, ou apenas ignoramos para não partir a app.
		fmt.Println("Warning: failed to fetch tenant names for enrichment:", err)
		return events, nil
	}

	// 3. Criar um mapa para acesso rápido: ID -> Nome
	tenantMap := make(map[int64]string)
	for _, t := range tenants {
		tenantMap[t.ID] = t.Name
	}

	// 4. Preencher o campo TenantName em cada evento
	for i := range events {
		if name, ok := tenantMap[events[i].TenantID]; ok {
			events[i].TenantName = name
		}
	}

	return events, nil
}

// GetEventsRevenueStats retorna o Top 5 eventos com mais receita
func (s EventService) GetEventsRevenueStats(q npadmin.Queryable, tenantID int64) ([]npadmin.EventRevenueStats, error) {
	var stats []npadmin.EventRevenueStats
	
	// Query otimizada: Soma o preço dos bilhetes PAGOS ('paid') agrupados por evento
	query := `
		SELECT 
			e.name as event_name,
			COALESCE(SUM(etd.price), 0) as revenue
		FROM events e
		LEFT JOIN purchased_tickets pt ON e.id = pt.event_id AND pt.status = 'paid'
		LEFT JOIN event_ticket_definitions etd ON pt.definition_id = etd.id
		WHERE e.tenant_id = $1
		GROUP BY e.id, e.name
		HAVING COALESCE(SUM(etd.price), 0) > 0
		ORDER BY revenue DESC
		LIMIT 5`

	err := q.Select(&stats, query, tenantID)
	if err != nil {
		return nil, fmt.Errorf("failed to get event revenue stats: %w", err)
	}
	
	// Retorna array vazio em vez de null se não houver dados
	if stats == nil {
		stats = []npadmin.EventRevenueStats{}
	}
	return stats, nil
}

func (s EventService) ListAdminEvents(q npadmin.Queryable, tenantID int64) ([]npadmin.Event, error) {
    var events []npadmin.Event
    
    // Query blindada: só traz eventos deste tenantID
    query := `
        SELECT * FROM events 
        WHERE tenant_id = $1
        ORDER BY event_date DESC`

    err := q.Select(&events, query, tenantID)
    if err != nil {
        return nil, fmt.Errorf("failed to list admin events: %w", err)
    }
    
    if events == nil {
        events = []npadmin.Event{}
    }
    return events, nil
}