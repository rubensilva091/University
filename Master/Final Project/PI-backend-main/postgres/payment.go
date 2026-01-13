package postgres

import (
	"database/sql"
	"errors"
	"fmt"
	"strconv"
	"time"

	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/payment"
)

type PaymentService struct {
	db *DB
}

// ExpiringSubscription representa uma subscrição prestes a caducar
type ExpiringSubscription struct {
	ID        int64     `db:"id"`
	AccountID int64     `db:"account_id"`
	Email     string    `db:"email"`
	EndDate   time.Time `db:"end_date"`
}

func NewPaymentService(db *DB) *PaymentService {
	return &PaymentService{db: db}
}

// SetPaymentSuccessful (e os métodos abaixo) já usam 'q'.
// A RLS em 'q' garante que estas operações só afetam o tenant
// definido no contexto RLS (que é definido no handler de callback).
func (p PaymentService) SetPaymentSuccessful(q npadmin.Queryable, accountID int64, price float64, startDate time.Time, endDate time.Time) error {
	err := p.InsertSubscriptionsHistory(q, accountID, price, startDate, endDate)
	if err != nil {
		return err
	}
	err = p.InsertCurrentSubscription(q, accountID, price, startDate, endDate)
	if err != nil {
		return err
	}
	return nil
}

// InsertSubscriptionsHistory usa 'q' (com RLS).
func (p PaymentService) InsertSubscriptionsHistory(q npadmin.Queryable, accountID int64, price float64, startDate time.Time, endDate time.Time) error {
	
	// 1. Obter o tenant_id da sessão (tal como fizemos na CurrentSubscription)
	var tenantID int64
	if err := q.QueryRow("SELECT current_setting('myapp.tenant_id', true)::bigint").Scan(&tenantID); err != nil {
		return fmt.Errorf("failed to retrieve current tenant ID for history: %w", err)
	}

	// 2. Query atualizada para incluir tenant_id
	historyQuery := `
		INSERT INTO subscriptions_history(account_id, tenant_id, start_date, end_date, price)
		VALUES ($1, $2, $3, $4, $5)
	`
	
	args := []any{accountID, tenantID, startDate, endDate, price}
	
	if _, err := q.Exec(historyQuery, args...); err != nil {
		return fmt.Errorf("failed to store subscription history: %w", err)
	}
	return nil
}

// InsertCurrentSubscription usa 'q' (com RLS).
func (p PaymentService) InsertCurrentSubscription(q npadmin.Queryable, accountID int64, price float64, startDate time.Time, endDate time.Time) error {
	// 1. Obter o tenant_id da sessão RLS (tal como já fazemos noutras funções)
	var tenantID int64
	if err := q.QueryRow("SELECT current_setting('myapp.tenant_id', true)::bigint").Scan(&tenantID); err != nil {
		return fmt.Errorf("failed to retrieve current tenant ID for subscription: %w", err)
	}

	// 2. Query corrigida:
	// - Inclui tenant_id no INSERT
	// - ON CONFLICT agora usa (account_id, tenant_id) para suportar múltiplas associações
	currentQuery := `
	INSERT INTO subscriptions (account_id, tenant_id, start_date, end_date, price)
	VALUES ($1, $2, $3, $4, $5)
	ON CONFLICT (account_id, tenant_id)
	DO UPDATE SET
		updated_at = now() at time zone 'utc',
		start_date = EXCLUDED.start_date,
		end_date = EXCLUDED.end_date,
		price = EXCLUDED.price,
		warning_sent_at = NULL -- Reset aviso de expiração na renovação
	`
	
	args := []any{accountID, tenantID, startDate, endDate, price}
	
	if _, err := q.Exec(currentQuery, args...); err != nil {
		return fmt.Errorf("failed to store current subscription: %w", err)
	}
	return nil
}

// InsertMultibanco AGORA GRAVA O tenant_id.
func (p PaymentService) InsertMultibanco(q npadmin.Queryable, mb payment.MBReference, accountID int64, period int64) error {
	// Obter o tenant_id do contexto RLS (definido pelo middleware em http/payment.go)
	var tenantID int64
	if err := q.QueryRow("SELECT current_setting('myapp.tenant_id', true)::bigint").Scan(&tenantID); err != nil {
		return fmt.Errorf("failed to retrieve current tenant ID from RLS session for InsertMultibanco: %w", err)
	}

	query := `
	INSERT INTO multibanco_payments (entity, reference, amount, order_id, account_id, period, tenant_id)
	VALUES ($1, $2, $3, $4, $5, $6, $7)
	`
	insertOrder := "INSERT INTO orders (id) VALUES ($1)"

	order, err := strconv.ParseInt(mb.OrderID, 10, 64)
	if err != nil {
		return fmt.Errorf("failed get order number: %w", err)
	}
	if _, err := q.Exec(insertOrder, order); err != nil {
		return fmt.Errorf("failed to insert order number: %w", err)
	}

	args := []any{mb.Entity, mb.Reference, mb.Amount, order, accountID, period, tenantID}
	if _, err := q.Exec(query, args...); err != nil {
		return fmt.Errorf("failed to insert multibanco reference: %w", err)
	}
	return nil
}

// MultibancoPaid AGORA RETORNA O tenant_id.
func (p PaymentService) MultibancoPaid(q npadmin.Queryable, reference string) (int64, int64, int64, error) {
	paymentStatusQuery := "SELECT payment_status from multibanco_payments WHERE reference = $1"
	query := `UPDATE multibanco_payments SET payment_status = TRUE
				WHERE reference = $1
				RETURNING account_id, period, tenant_id`

	var accountID int64
	var period int64
	var tenantID int64
	var paymentStatus bool

	err := q.Get(&paymentStatus, paymentStatusQuery, reference)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return 0, 0, 0, fmt.Errorf("payment reference not found")
		}
		return 0, 0, 0, fmt.Errorf("failed to get payment status: %w", err)
	}
	if paymentStatus {
		return 0, 0, 0, errors.New("payment status already updated")
	}

	err = q.QueryRow(query, reference).Scan(&accountID, &period, &tenantID)
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed to set payment as true: %w", err)
	}

	return accountID, period, tenantID, nil
}

// InsertMBWay AGORA GRAVA O tenant_id.
func (p PaymentService) InsertMBWay(q npadmin.Queryable, payment payment.MBWayPayment, accountID int64, period int64) error {
	var tenantID int64
	if err := q.QueryRow("SELECT current_setting('myapp.tenant_id', true)::bigint").Scan(&tenantID); err != nil {
		return fmt.Errorf("failed to retrieve current tenant ID from RLS session for InsertMBWay: %w", err)
	}

	insertOrder := "INSERT INTO orders (id) VALUES ($1)"
	insertPayment := `
	INSERT INTO mbway_payments (amount, order_date, request_id, order_id, account_id, period, tenant_id)
	VALUES ($1, $2, $3, $4, $5, $6, $7)
	`

	order, err := strconv.ParseInt(payment.OrderID, 10, 64)
	if err != nil {
		return fmt.Errorf("failed get order number: %w", err)
	}
	layout := "02-01-2006 15:04:05"
	orderDateTime, err := time.Parse(layout, payment.Date)
	if err != nil {
		return fmt.Errorf("failed to parse MBWay payment date '%s': %w", payment.Date, err)
	}

	if _, err := q.Exec(insertOrder, order); err != nil {
		return fmt.Errorf("failed to insert order number: %w", err)
	}

	args := []any{payment.Amount, orderDateTime, payment.RequestID, order, accountID, period, tenantID}
	if _, err := q.Exec(insertPayment, args...); err != nil {
		return fmt.Errorf("failed to insert mbway reference: %w", err)
	}
	return nil
}

// MBWayPaid AGORA RETORNA O tenant_id.
func (p PaymentService) MBWayPaid(q npadmin.Queryable, reqID string) (int64, int64, int64, error) {
	paymentStatusQuery := "SELECT payment_status from mbway_payments WHERE request_id = $1"
	query := `UPDATE mbway_payments SET payment_status = TRUE
				WHERE request_id = $1
				RETURNING account_id, period, tenant_id`

	var accountID int64
	var period int64
	var tenantID int64
	var paymentStatus bool

	err := q.Get(&paymentStatus, paymentStatusQuery, reqID)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return 0, 0, 0, fmt.Errorf("payment request ID not found")
		}
		return 0, 0, 0, fmt.Errorf("failed to get payment status: %w", err)
	}
	if paymentStatus {
		return 0, 0, 0, errors.New("payment status already updated")
	}

	err = q.QueryRow(query, reqID).Scan(&accountID, &period, &tenantID)
	if err != nil {
		return 0, 0, 0, fmt.Errorf("failed to set payment as true: %w", err)
	}

	return accountID, period, tenantID, nil
}

// GetMBWayStatus usa 'q' (com RLS).
func (p PaymentService) GetMBWayStatus(q npadmin.Queryable, orderID string, accountID int64) (bool, error) {
	query := "SELECT payment_status from mbway_payments WHERE order_id = $1 AND account_id = $2"
	var status bool
	if err := q.Get(&status, query, orderID, accountID); err != nil {
		return false, fmt.Errorf("failed to get order status: %w", err)
	}
	return status, nil
}

// GetNextOrderNumber usa 'q' (com RLS).
func (p PaymentService) GetNextOrderNumber(q npadmin.Queryable) (int64, error) {
	var order int64
	query := "select nextval('orders_id_seq')"
	if err := q.QueryRow(query).Scan(&order); err != nil {
		return 0, fmt.Errorf("failed to increment order number (check RLS on orders table/sequence?): %w", err)
	}
	return order, nil
}

// GetExpiringSubscriptions procura subscrições que expiram nos próximos 'days' dias
// e que ainda não têm o aviso de expiração enviado.
func (p PaymentService) GetExpiringSubscriptions(q npadmin.Queryable, days int) ([]ExpiringSubscription, error) {
	// Seleciona subscrições onde a end_date é menor que (agora + dias), maior que agora (não expiradas),
	// e onde o warning_sent_at é NULL.
	query := `
		SELECT s.id, s.account_id, s.end_date, a.email
		FROM subscriptions s
		JOIN accounts a ON s.account_id = a.id
		WHERE s.end_date <= (NOW() + ($1 || ' days')::INTERVAL)
		  AND s.end_date > NOW()
		  AND s.warning_sent_at IS NULL
	`

	var subs []ExpiringSubscription
	// sqlx Select lida com o scan para a struct
	if err := q.Select(&subs, query, days); err != nil {
		return nil, fmt.Errorf("failed to get expiring subscriptions: %w", err)
	}
	return subs, nil
}

// MarkWarningSent atualiza a data de envio do aviso para a subscrição
func (p PaymentService) MarkWarningSent(q npadmin.Queryable, id int64) error {
	query := `UPDATE subscriptions SET warning_sent_at = NOW() WHERE id = $1`
	if _, err := q.Exec(query, id); err != nil {
		return fmt.Errorf("failed to mark warning as sent for subscription %d: %w", id, err)
	}
	return nil
}