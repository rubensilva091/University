package postgres

import (
	"database/sql"
	"errors" // Importar errors se ainda não estiver
	"fmt"
	"strings"
	"time"

	"github.com/invisiblelab-dev/npadmin"
	// sqlx não é necessário aqui
)

type DataService struct {
	db *DB
}

func NewDataService(db *DB) *DataService {
	return &DataService{db: db}
}

// AssociatesHistory busca histórico DENTRO do tenant atual.
func (s DataService) AssociatesHistory(q npadmin.Queryable) ([]npadmin.AssociatesPerMonth, error) {
	query := `SELECT DATE_TRUNC('MONTH', p.updated_at) AS month,
	                 COUNT(p.id) AS number_associates
	          FROM profiles p
	          JOIN accounts a ON p.id = a.id
	          WHERE a.status_id = $1
              AND p.tenant_id = current_setting('myapp.tenant_id')::bigint
              AND p.id != current_setting('myapp.user_id', true)::bigint -- Esconder o Admin
	          GROUP BY DATE_TRUNC('MONTH', p.updated_at)`

	history := make([]npadmin.AssociatesPerMonth, 0)
	err := q.Select(&history, query, npadmin.AccountVerified)
	if err != nil {
		return nil, fmt.Errorf("failed to execute/scan associates history: %w", err)
	}
	return history, nil
}

// SubscriptionsSituation busca situação DENTRO do tenant atual.
func (s DataService) SubscriptionsSituation(q npadmin.Queryable) (int64, int64, error) {
	query := `SELECT end_date >= now() at time zone 'utc' as active,
	                 COUNT(id) AS total_subscriptions
	          FROM subscriptions
              WHERE tenant_id = current_setting('myapp.tenant_id')::bigint
              AND account_id != current_setting('myapp.user_id', true)::bigint -- Esconder o Admin
	          GROUP BY active`
	
    results := []struct {
		Active bool  `db:"active"`
		Total  int64 `db:"total_subscriptions"`
	}{}
	err := q.Select(&results, query)
	if err != nil {
		return 0, 0, fmt.Errorf("failed to execute/scan subscriptions situation: %w", err)
	}
	var active, inactive int64
	for _, res := range results {
		if res.Active { active = res.Total } else { inactive = res.Total }
	}
	return active, inactive, nil
}

// MonthlyIncome busca rendimento DENTRO do tenant atual.
func (s DataService) MonthlyIncome(q npadmin.Queryable) ([]npadmin.MonthIncome, error) {
	query := `SELECT SUM(price) as income, DATE_TRUNC('month', created_at) as month
	          FROM subscriptions_history
              WHERE tenant_id = current_setting('myapp.tenant_id')::bigint
              AND account_id != current_setting('myapp.user_id', true)::bigint -- Esconder o Admin
	          GROUP BY DATE_TRUNC('month', created_at)`
	
    incomeHistory := make([]npadmin.MonthIncome, 0)
	err := q.Select(&incomeHistory, query)
	if err != nil {
		return nil, fmt.Errorf("failed to execute/scan income history: %w", err)
	}
	return incomeHistory, nil
}

// HistoricSubs busca situação DENTRO do tenant atual.
func (s DataService) HistoricSubs(q npadmin.Queryable) (int64, int64, error) {
	query := `
		WITH tenant_payers AS (
			SELECT DISTINCT sh.account_id
			FROM subscriptions_history sh
            WHERE sh.tenant_id = current_setting('myapp.tenant_id')::bigint
		),
		tenant_active_profiles AS (
			SELECT p.id as account_id
			FROM profiles p
			JOIN accounts a ON p.id = a.id
			WHERE a.status_id != $1
            AND p.tenant_id = current_setting('myapp.tenant_id')::bigint
            AND p.id != current_setting('myapp.user_id', true)::bigint -- Esconder o Admin
		)
		SELECT (tp.account_id IS NOT NULL) AS payer, count(tap.account_id) as amount
		FROM tenant_active_profiles tap
		LEFT JOIN tenant_payers tp ON tap.account_id = tp.account_id
		GROUP BY payer;
	`
	results := []struct { Payer bool `db:"payer"`; Amount int64 `db:"amount"` }{}
	err := q.Select(&results, query, npadmin.AccountPending)
	if err != nil {
		return 0, 0, fmt.Errorf("failed to execute/scan historic subs: %w", err)
	}
	var havePaid, neverPaid int64
	for _, res := range results {
		if res.Payer { havePaid = res.Amount } else { neverPaid = res.Amount }
	}
	return havePaid, neverPaid, nil
}

// TotalAssociates conta perfis verificados DENTRO do tenant atual.
func (s DataService) TotalAssociates(q npadmin.Queryable) (int64, error) {
	query := `SELECT count(p.id) as totalVerifiedAssociates
	          FROM profiles p
	          JOIN accounts a ON p.id = a.id
	          WHERE a.status_id = $1
              AND p.tenant_id = current_setting('myapp.tenant_id')::bigint
              AND p.id != current_setting('myapp.user_id', true)::bigint`
	
    var totalVerifiedAssociates int64
	err := q.Get(&totalVerifiedAssociates, query, npadmin.AccountVerified)
	if err != nil {
		return 0, fmt.Errorf("failed to count total verified associates: %w", err)
	}
	return totalVerifiedAssociates, nil
}

// FilteredAssociates busca associados filtrados DENTRO do tenant atual (via RLS).
func (s DataService) FilteredAssociates(q npadmin.Queryable, filter *npadmin.FilterData) ([]npadmin.AssociateSummary, int64, error) {
	// 1. Construção da Query Base (Comum à listagem e à contagem)
	baseQuery := `
		FROM profiles p
		JOIN accounts a ON p.id = a.id
		LEFT JOIN subscriptions s ON s.account_id = p.id
		LEFT JOIN account_tenant_memberships atm ON atm.account_id = p.id AND atm.tenant_id = p.tenant_id
	`
	
	var conditions []string
	args := []any{}

	// --- FILTROS ---
	
	// A. Segurança: Forçar tenant atual e EXCLUIR O PRÓPRIO UTILIZADOR
	conditions = append(conditions, "p.tenant_id = current_setting('myapp.tenant_id')::bigint")
	conditions = append(conditions, "p.id != current_setting('myapp.user_id', true)::bigint") // <-- AQUI ESTÁ A CORREÇÃO PARA TE ESCONDER

	// B. Filtros do UI
	if filter.Status >= 1 {
		args = append(args, filter.Status)
		conditions = append(conditions, fmt.Sprintf("a.status_id = $%d", len(args)))
	}
	if filter.Name != "" {
		namePattern := "%" + filter.Name + "%"
		args = append(args, namePattern)
		argNum := len(args)
		conditions = append(conditions, fmt.Sprintf("(p.first_name ILIKE $%d OR p.last_name ILIKE $%d OR a.email ILIKE $%d OR p.nif ILIKE $%d)", argNum, argNum, argNum, argNum))
	}
	subStatusCondition := subscriptionStatusQuery(filter.SubStatus)
	if subStatusCondition != "" { 
		conditions = append(conditions, subStatusCondition) 
	}

	// Montar cláusula WHERE
	whereClause := ""
	if len(conditions) > 0 {
		whereClause = " WHERE " + strings.Join(conditions, " AND ")
	}

	// 2. Executar Contagem Total (Query Rápida)
	var total int64
	countQuery := "SELECT count(p.id) " + baseQuery + whereClause
	// Usamos os mesmos argumentos de filtro
	if err := q.Get(&total, countQuery, args...); err != nil {
		return nil, 0, fmt.Errorf("failed to count filtered associates: %w", err)
	}

	// 3. Executar Query de Dados (Com Paginação)
	dataQuery := `
		SELECT a.id, a.email, a.status_id, p.nif, p.first_name, p.last_name,
		       COALESCE(atm.role, 'unknown') as membership_role,
		       s.end_date as subscription_end_date
	` + baseQuery + whereClause + " ORDER BY p.last_name, p.first_name"

	if filter.Page > 0 && filter.PageSize > 0 {
		limit := filter.PageSize
		offset := (filter.Page - 1) * filter.PageSize
		// Adicionar argumentos de paginação
		args = append(args, limit, offset)
		dataQuery += fmt.Sprintf(" LIMIT $%d OFFSET $%d", len(args)-1, len(args))
	}

	filteredAssociates := make([]npadmin.AssociateSummary, 0)
	if err := q.Select(&filteredAssociates, dataQuery, args...); err != nil {
		return nil, 0, fmt.Errorf("failed to list filtered associates: %w", err)
	}

	// Pós-processamento
	for i := range filteredAssociates {
		sub := &filteredAssociates[i]
		if sub.SubsEndDate.Valid && sub.SubsEndDate.Time.After(time.Now().UTC()) {
			sub.SubscriptionStatus = npadmin.SubscriptionValid.String()
		} else if sub.SubsEndDate.Valid {
			sub.SubscriptionStatus = npadmin.SubscriptionExpired.String()
		} else {
			sub.SubscriptionStatus = npadmin.SubscriptionOther.String()
		}
	}

	return filteredAssociates, total, nil
}


// AssociateCompleteData busca dados DENTRO do tenant atual (via RLS).
func (s DataService) AssociateCompleteData(q npadmin.Queryable, filter npadmin.AssociateFilter) (npadmin.AssociateData, error) {
	profile, err := s.GetProfileByEmail(q, filter.Email)
	if err != nil {
		return npadmin.AssociateData{}, fmt.Errorf("failed to query profile data by email '%s' (in current tenant): %w", filter.Email, err)
	}
	subscriptions, err := s.SubscriptionHistory(q, profile.ID, filter.Pagination)
	if err != nil && !errors.Is(err, sql.ErrNoRows) {
		return npadmin.AssociateData{}, fmt.Errorf("failed to query subscription history for account %d (in current tenant): %w", profile.ID, err)
	}
	associateData := npadmin.AssociateData{Profile: *profile, Subscriptions: subscriptions}
	return associateData, nil
}

// GetProfileByEmail busca perfil DENTRO do tenant atual (via RLS).
func (s DataService) GetProfileByEmail(q npadmin.Queryable, email string) (*npadmin.Profile, error) {
	var profile npadmin.Profile
	// CORREÇÃO: Especificar colunas explicitamente para evitar erro com coluna 'role' fantasma
	query := `
		SELECT p.id, p.tenant_id, p.first_name, p.last_name, 
		       p.nif, p.tlm, p.category_id, p.created_at, p.updated_at
		FROM profiles p 
		JOIN accounts a ON p.id = a.id 
		WHERE a.email = $1
	`
	if err := q.Get(&profile, query, email); err != nil {
		return nil, fmt.Errorf("failed to fetch profile by email '%s' (in current tenant context): %w", email, err)
	}
	return &profile, nil
}

// SubscriptionHistory busca histórico DENTRO do tenant atual (via RLS).
func (s DataService) SubscriptionHistory(q npadmin.Queryable, id int64, pagination npadmin.Pagination) ([]npadmin.Subscription, error) {
	queryBuilder := strings.Builder{}
	
	// CORREÇÃO: Adicionado filtro "AND tenant_id = ..."
	queryBuilder.WriteString(`
		SELECT id, account_id, start_date, end_date, price, invoice_file, created_at, updated_at 
		FROM subscriptions_history 
		WHERE account_id = $1 
		  AND tenant_id = current_setting('myapp.tenant_id', true)::bigint
		ORDER BY end_date DESC 
	`)
	
	args := []any{id}
	if pagination.Page > 0 && pagination.PageSize > 0 {
		limit := pagination.PageSize
		offset := (pagination.Page - 1) * pagination.PageSize
		queryBuilder.WriteString(`LIMIT $2 OFFSET $3`)
		args = append(args, limit, offset)
	}
	
	query := queryBuilder.String()
	subscriptions := make([]npadmin.Subscription, 0)
	err := q.Select(&subscriptions, query, args...)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) { return []npadmin.Subscription{}, nil }
		return nil, fmt.Errorf("failed to execute/scan associate subscription history query for account %d: %w", id, err)
	}
	return subscriptions, nil
}

// CurrentSubscription busca subscrição atual DENTRO do tenant atual (via RLS).
func (d DataService) CurrentSubscription(q npadmin.Queryable, accountID int64) (*npadmin.Subscription, error) {
	// CORREÇÃO: Adicionar filtro por tenant_id para garantir unicidade
	// A RLS já deve filtrar, mas o q.Get pode falhar se a RLS não estiver ativa ou se retornar mais que uma linha por algum motivo.
	// O mais seguro é garantir que a query é específica.
	query := `
		SELECT id, account_id, start_date, end_date, price, invoice_file, created_at, updated_at 
		FROM subscriptions 
		WHERE account_id = $1 
		  AND tenant_id = current_setting('myapp.tenant_id', true)::bigint
	`
	var sub npadmin.Subscription
	if err := q.Get(&sub, query, accountID); err != nil {
		return nil, err
	}
	return &sub, nil
}

// REMOVIDO: ExecuteAccountDisable (Movido para AccountService)
/*
func (s DataService) ExecuteAccountDisable(q npadmin.Queryable, email npadmin.AssociateID) error { ... }
*/

// REMOVIDO: ExecuteAccountVerify (Movido para AccountService)
/*
func (s DataService) ExecuteAccountVerify(q npadmin.Queryable, email npadmin.AssociateID) error { ... }
*/

// REMOVIDO: GetByEmail (Pertence a AccountService)
/*
func (s DataService) GetByEmail(q npadmin.Queryable, email string) (*npadmin.Account, error) { ... }
*/

// UploadInvoice atualiza 'subscriptions_history' DENTRO do tenant atual (RLS).
func (s DataService) UploadInvoice(q npadmin.Queryable, fileURL string, accountID int64, subscriptionID int64) error {
	var returnedID int64
	
	// CORREÇÃO: Adicionado filtro "AND tenant_id = ..." para garantir que só altera o registo da associação atual
	query := `
		UPDATE subscriptions_history 
		SET invoice_file = $1, updated_at = now() at time zone 'utc' 
		WHERE id = $2 
		  AND account_id = $3 
		  AND tenant_id = current_setting('myapp.tenant_id', true)::bigint
		RETURNING id
	`
	
	if err := q.QueryRow(query, fileURL, subscriptionID, accountID).Scan(&returnedID); err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("failed to update subscription invoice: record not found or wrong tenant")
		}
		return fmt.Errorf("failed to update subscription invoice file: %w", err)
	}
	return nil
}

// GetPrice busca preço DENTRO do tenant atual (RLS).
func (s DataService) GetPrice(q npadmin.Queryable, categoryID int64, period int64) (float64, error) {
	var price float64
	query := `SELECT price FROM prices WHERE category_id = $1 and period = $2` // RLS aplicada
	if err := q.Get(&price, query, categoryID, period); err != nil {
		return 0, fmt.Errorf("failed to fetch price by categoryID %d and period %d (in current tenant context): %w", categoryID, period, err)
	}
	return price, nil
}

// GetPrices busca preços DENTRO do tenant atual (RLS).
func (s DataService) GetPrices(q npadmin.Queryable, categoryID int64) ([]npadmin.Prices, error) {
	query := `SELECT period, price FROM prices WHERE category_id = $1` // RLS aplicada
	prices := make([]npadmin.Prices, 0)
	err := q.Select(&prices, query, categoryID)
	if err != nil {
		return nil, fmt.Errorf("failed to execute/scan prices for category %d (in current tenant): %w", categoryID, err)
	}
	return prices, nil
}

// AllPrices busca todos os preços DENTRO do tenant atual (RLS).
func (s DataService) AllPrices(q npadmin.Queryable) ([]npadmin.Prices, error) {
	query := `SELECT p.period, p.price, ac.name as category FROM prices p JOIN associate_category ac ON ac.id = p.category_id` // RLS aplicada a p e ac
	prices := make([]npadmin.Prices, 0)
	err := q.Select(&prices, query)
	if err != nil {
		return nil, fmt.Errorf("failed to execute/scan all prices (in current tenant): %w", err)
	}
	return prices, nil
}

// InsertPrice insere/atualiza preço DENTRO do tenant atual (RLS).
func (s DataService) InsertPrice(q npadmin.Queryable, categoryID int64, period int, price float64) error {
	var tenantID int64
	if err := q.QueryRow("SELECT current_setting('myapp.tenant_id', true)::bigint").Scan(&tenantID); err != nil {
		return fmt.Errorf("failed to retrieve current tenant ID from RLS session for InsertPrice: %w", err)
	}
	query := `INSERT INTO prices (category_id, period, price, tenant_id) VALUES ($1, $2, $3, $4) ON CONFLICT (tenant_id, category_id, period) DO UPDATE SET price = EXCLUDED.price, updated_at = now() at time zone 'utc'`
	args := []any{categoryID, period, price, tenantID}
	if _, err := q.Exec(query, args...); err != nil {
		return fmt.Errorf("failed to insert/update price (cat %d, period %d, tenant %d): %w", categoryID, period, tenantID, err)
	}
	return nil
}

// DeletePrice apaga preço DENTRO do tenant atual (RLS).
func (s DataService) DeletePrice(q npadmin.Queryable, categoryID int64, period int) (bool, error) {
	query := `DELETE FROM prices WHERE category_id = $1 and period = $2` // RLS aplicada
	args := []any{categoryID, period}
	result, err := q.Exec(query, args...)
	if err != nil {
		return false, fmt.Errorf("failed to delete price (cat %d, period %d, current tenant): %w", categoryID, period, err)
	}
	affected, err := result.RowsAffected()
	if err != nil {
		return false, fmt.Errorf("failed to fetch affected rows for delete price (cat %d, period %d): %w", categoryID, period, err)
	}
	return affected > 0, nil
}

// GetPeriods busca períodos distintos DENTRO do tenant atual (RLS).
func (s DataService) GetPeriods(q npadmin.Queryable) ([]int64, error) {
	query := `SELECT DISTINCT period FROM prices ORDER BY period` // RLS aplicada
	periods := make([]int64, 0)
	err := q.Select(&periods, query)
	if err != nil {
		return nil, fmt.Errorf("failed to execute/scan distinct periods query (current tenant): %w", err)
	}
	return periods, nil
}

// GetCategories busca categorias distintas DENTRO do tenant atual (RLS).
func (s DataService) GetCategories(q npadmin.Queryable) ([]npadmin.Categories, error) {
	query := `SELECT name, description FROM associate_category ORDER BY name` // RLS aplicada
	categories := make([]npadmin.Categories, 0)
	err := q.Select(&categories, query)
	if err != nil {
		return nil, fmt.Errorf("failed to execute/scan categories query (current tenant): %w", err)
	}
	return categories, nil
}

// subscriptionStatusQuery (função auxiliar)
func subscriptionStatusQuery(s npadmin.SubscriptionStatus) string {
	switch s {
	case npadmin.SubscriptionExpired: return " s.end_date < now() at time zone 'utc' "
	case npadmin.SubscriptionValid:   return " s.end_date >= now() at time zone 'utc' "
	case npadmin.SubscriptionOther:   return " s.end_date IS NULL "
	default:                          return ""
	}
}