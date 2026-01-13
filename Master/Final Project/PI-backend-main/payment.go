package npadmin

import (
	"time"

	"github.com/invisiblelab-dev/npadmin/payment"
)

type MultibancoParams struct {
	Period int64 `json:"period"`
}

type MBWayParams struct {
	Period int64  `json:"period"`
	TLM    string `json:"tlm"`
}

type PaymentService interface {
	SetPaymentSuccessful(Queryable, int64, float64, time.Time, time.Time) error // Added Queryable
	InsertSubscriptionsHistory(Queryable, int64, float64, time.Time, time.Time) error // Added Queryable
	InsertCurrentSubscription(Queryable, int64, float64, time.Time, time.Time) error // Added Queryable
	InsertMultibanco(Queryable, payment.MBReference, int64, int64) error // Added Queryable
	InsertMBWay(Queryable, payment.MBWayPayment, int64, int64) error // Added Queryable

	// --- ASSINATURAS MODIFICADAS ---
	// Agora retornam (accountID, period, tenantID, error)
	MultibancoPaid(Queryable, string) (int64, int64, int64, error) // Added Queryable, Added tenantID return
	MBWayPaid(Queryable, string) (int64, int64, int64, error) // Added Queryable, Added tenantID return
	// --- FIM DAS MODIFICAÇÕES ---

	GetMBWayStatus(Queryable, string, int64) (bool, error) // Added Queryable

	GetNextOrderNumber(Queryable) (int64, error) // Added Queryable
}

type MBCallbackParams struct {
	Key             string `schema:"chave"`
	Amount          string `schema:"valor"`
	Entity          string `schema:"entidade"`
	Reference       string `schema:"referencia"`
	PaymentDateTime string `schema:"datahorapag"`
	Status          string `schema:"estado"`
}

type MBWayCallbackParams struct {
	Key             string `schema:"chave"`
	Reference       string `schema:"referencia"`
	RequestID       string `schema:"idpedido"`
	Amount          string `schema:"valor"`
	PaymentDatetime string `schema:"datahorapag"`
	Status          string `schema:"estado"`
}

type MBWayStatusParams struct {
	OrderID string `json:"orderID"`
}