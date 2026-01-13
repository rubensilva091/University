package npadmin

import "time"

// Event representa um evento criado pela associação (Admin).
type Event struct {
	ID          int64     `db:"id" json:"id"`
	TenantID    int64     `db:"tenant_id" json:"tenantId"`
	Name        string    `db:"name" json:"name"`
	Description string    `db:"description" json:"description"`
	EventDate   time.Time `db:"event_date" json:"eventDate"`
	Location    string    `db:"location" json:"location"`
	ImageURL    string    `db:"image_url" json:"imageUrl"`
	CreatedAt   time.Time `db:"created_at" json:"createdAt"`
	UpdatedAt   time.Time `db:"updated_at" json:"updatedAt"`
	TenantName string `json:"tenantName" db:"tenant_name"`
}

// EventTicketDefinition define os tipos de bilhete e preços para um evento (Admin).
type EventTicketDefinition struct {
	ID                int64     `db:"id" json:"id"`
	TenantID          int64     `db:"tenant_id" json:"tenantId"`
	EventID           int64     `db:"event_id" json:"eventId"`
	Name              string    `db:"name" json:"name"`
	Price             float64   `db:"price" json:"price"`
	AvailableQuantity int       `db:"available_quantity" json:"availableQuantity"`
	CreatedAt         time.Time `db:"created_at" json:"createdAt"`

	// Campos extra para joins
	EventName string `db:"event_name" json:"eventName,omitempty"`
}

// PurchasedTicket é um bilhete individual comprado por um utilizador.
type PurchasedTicket struct {
	ID               int64      `db:"id" json:"id"`
	TenantID         int64      `db:"tenant_id" json:"tenantId"`
	EventID          int64      `db:"event_id" json:"eventId"`
	DefinitionID     int64      `db:"definition_id" json:"definitionId"`
	AccountID        int64      `db:"account_id" json:"accountId"`
	Status           string     `db:"status" json:"status"` // 'pending', 'paid', 'cancelled'
	PaymentOrderID   NullInt64  `db:"payment_order_id" json:"paymentOrderId,omitempty"`
	PaymentReference NullString `db:"payment_reference" json:"paymentReference,omitempty"`
	PaymentRequestID NullString `db:"payment_request_id" json:"paymentRequestId,omitempty"`
	QRCodeToken      NullString `db:"qr_code_token" json:"qrCodeToken,omitempty"`
	CreatedAt        time.Time  `db:"created_at" json:"createdAt"`
	UpdatedAt        time.Time  `db:"updated_at" json:"updatedAt"`

	// Campos extra para mostrar ao utilizador (via joins)
	EventName       string    `db:"event_name" json:"eventName,omitempty"`
	EventDate       time.Time `db:"event_date" json:"eventDate,omitempty"`
	TicketName      string    `db:"ticket_name" json:"ticketName,omitempty"`
	TicketPrice     float64   `db:"ticket_price" json:"ticketPrice,omitempty"`
	PurchaserEmail  string    `db:"purchaser_email" json:"purchaserEmail,omitempty"`
}

// PurchaseResponse é a resposta da API ao iniciar a compra, contendo dados para o pagamento.
type PurchaseResponse struct {
	Ticket         *PurchasedTicket `json:"ticket"`
	PaymentType    string           `json:"paymentType"` // "multibanco" ou "mbway"
	Entity         string           `json:"entity,omitempty"`
	Reference      string           `json:"reference,omitempty"`
	Amount         string           `json:"amount,omitempty"`
	MBWayRequestID string           `json:"mbWayRequestId,omitempty"`
}

// EventRevenueStats define a estrutura para o gráfico de faturação (Admin)
type EventRevenueStats struct {
	EventName string  `db:"event_name" json:"eventName"`
	Revenue   float64 `db:"revenue" json:"revenue"`
}

// EventService define a interface para o módulo de eventos.
type EventService interface {
	// --- Funções de Admin ---
	CreateEvent(q Queryable, event *Event) error
	CreateTicketDefinition(q Queryable, def *EventTicketDefinition) error
	GetAdminEventDetails(q Queryable, eventID int64) (*Event, []EventTicketDefinition, error)
	GetAdminPurchasedTicketsForEvent(q Queryable, eventID int64) ([]PurchasedTicket, error)
	ListAdminEvents(q Queryable, tenantID int64) ([]Event, error)

	// --- Funções Públicas / Utilizador ---
	ListAvailableEvents(q Queryable) ([]Event, error)
	GetPublicEventDetails(q Queryable, eventID int64) (*Event, []EventTicketDefinition, error)
	GetMyTickets(q Queryable, accountID int64) ([]PurchasedTicket, error)
	GetMyTicketDetails(q Queryable, ticketID int64, accountID int64) (*PurchasedTicket, error)

	// --- Funções de Pagamento ---
	InitiateTicketPurchase(q Queryable, accountID int64, definitionID int64, paymentType string, mbwayTlm string, userEmail string) (*PurchaseResponse, error)
	ConfirmTicketByReference(q Queryable, reference string) (*PurchasedTicket, error) // Multibanco
	ConfirmTicketByRequestID(q Queryable, requestID string) (*PurchasedTicket, error) // MBWay

	// --- NOVA FUNÇÃO ---
	GetAllEvents(q Queryable) ([]Event, error)
	GetEventsRevenueStats(q Queryable, tenantID int64) ([]EventRevenueStats, error)
}