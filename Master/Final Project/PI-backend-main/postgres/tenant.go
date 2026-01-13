package postgres

import (
	"database/sql"
	"errors"
	"fmt"
	"strconv"
	"strings"

	"github.com/invisiblelab-dev/npadmin"
)

// TenantService implementa a interface npadmin.TenantService.
type TenantService struct {
	db *DB
}

// NewTenantService cria uma nova instância de TenantService.
func NewTenantService(db *DB) *TenantService {
	return &TenantService{db: db}
}

// Create cria um novo tenant e o seu utilizador admin inicial dentro de uma transação (fornecida por 'q').
// postgres/tenant.go

func (s TenantService) Create(q npadmin.Queryable, params npadmin.TenantCreateParams) (tenantID int64, adminAccountID int64, err error) {

	// 1. Inserir o novo Tenant (MANTÉM)
	tenantQuery := `INSERT INTO tenants (name, domain) VALUES ($1, $2) RETURNING id`
	var domain sql.NullString
	if params.Domain != "" {
		domain = sql.NullString{String: params.Domain, Valid: true}
	}

	err = q.QueryRow(tenantQuery, params.TenantName, domain).Scan(&tenantID)
	if err != nil {
		if strings.Contains(err.Error(), "tenants_domain_key") {
			return 0, 0, fmt.Errorf("domain '%s' is already taken", params.Domain)
		}
		return 0, 0, fmt.Errorf("failed to create tenant: %w", err)
	}

	// 2. Inserir Admin (MANTÉM)
	accountQuery := `
		INSERT INTO accounts (email, status_id)
		VALUES ($1, $2)
		ON CONFLICT (email) DO NOTHING
		RETURNING id`

	errScan := q.QueryRow(accountQuery, strings.ToLower(params.AdminEmail), npadmin.AccountVerified).Scan(&adminAccountID)
	if errScan != nil {
		if errors.Is(errScan, sql.ErrNoRows) {
			errGet := q.Get(&adminAccountID, "SELECT id FROM accounts WHERE email = $1", strings.ToLower(params.AdminEmail))
			if errGet != nil {
				return 0, 0, fmt.Errorf("failed to retrieve existing admin account ID: %w", errGet)
			}
		} else {
			return 0, 0, fmt.Errorf("failed to add new admin account entry: %w", errScan)
		}
	}

	// 3. RLS Context (MANTÉM)
	tenantIDStr := strconv.FormatInt(tenantID, 10)
	q.Exec("SELECT set_config('myapp.tenant_id', $1, true)", tenantIDStr)
	adminAccountIDStr := strconv.FormatInt(adminAccountID, 10)
	q.Exec("SELECT set_config('myapp.user_id', $1, true)", adminAccountIDStr)

	// 4. Password (MANTÉM)
	passwordQuery := `
		INSERT INTO account_password_hashes (id, hash)
		VALUES ($1, $2)
		ON CONFLICT (id) DO UPDATE SET hash = EXCLUDED.hash`
	q.Exec(passwordQuery, adminAccountID, params.AdminPassword.Hash)

	// 5. Memberships (MANTÉM)
	membershipQuery := `INSERT INTO account_tenant_memberships (account_id, tenant_id, role) VALUES ($1, $2, $3)`
	q.Exec(membershipQuery, adminAccountID, tenantID, "admin")
	
	systemTenantID := 1
	if int(tenantID) != systemTenantID {
		q.Exec(membershipQuery, adminAccountID, systemTenantID, "associate") 
	}

	// --- 6. CRIAÇÃO FIXA DE CATEGORIAS E PREÇOS (6 e 12 meses) ---
	
	defaults := []struct {
		Name        string
		Desc        string
		Price12Meses float64
		Price6Meses  float64
	}{
		{"Criança",   "Até aos 12 anos",             0.0,  0.0},
		{"Jovem",     "Dos 13 aos 17 anos",          10.0, 6.0},  // Ex: 10€ ano, 6€ semestre
		{"Estudante", "Comprovativo escolar necessário", 15.0, 8.0},
		{"Adulto",    "Categoria geral (+18)",       25.0, 13.0},
		{"Idoso",     "Maiores de 65 anos",          15.0, 8.0},
	}

	var adminCategoryID int64

	catInsertQuery := `INSERT INTO associate_category (tenant_id, name, description) VALUES ($1, $2, $3) RETURNING id`
	priceInsertQuery := `INSERT INTO prices (category_id, tenant_id, period, price) VALUES ($1, $2, $3, $4)`

	for _, def := range defaults {
		var newCatID int64
		
		// A. Criar Categoria
		if err := q.QueryRow(catInsertQuery, tenantID, def.Name, def.Desc).Scan(&newCatID); err != nil {
			return 0, 0, fmt.Errorf("failed to create category '%s': %w", def.Name, err)
		}

		// B. Criar Preço 12 Meses
		q.Exec(priceInsertQuery, newCatID, tenantID, 12, def.Price12Meses)

		// C. Criar Preço 6 Meses (Se não for grátis ou se quiseres registar grátis também)
		// Aqui inserimos sempre para garantir que a opção existe
		q.Exec(priceInsertQuery, newCatID, tenantID, 6, def.Price6Meses)

		if def.Name == "Adulto" {
			adminCategoryID = newCatID
		}
	}

	if adminCategoryID == 0 { adminCategoryID = 1 } // Fallback

	// 7. Perfil Admin (MANTÉM)
	if _, err := strconv.Atoi(params.AdminNIF); err != nil { return 0, 0, fmt.Errorf("bad NIF") }

	profileQuery := `
		INSERT INTO profiles (id, first_name, last_name, nif, category_id, tenant_id)
		VALUES ($1, $2, $3, $4, $5, $6)`

	argsProfile := []any{
		adminAccountID,
		npadmin.NewNullString(params.AdminFirstName),
		npadmin.NewNullString(params.AdminLastName),
		npadmin.NewNullString(params.AdminNIF),
		adminCategoryID,
		tenantID,
	}

	if _, err := q.Exec(profileQuery, argsProfile...); err != nil {
		return 0, 0, fmt.Errorf("failed to add admin profile: %w", err)
	}

	return tenantID, adminAccountID, nil
}

// GetUserTenants retorna os tenants onde o utilizador é membro.
func (s TenantService) GetUserTenants(q npadmin.Queryable, accountID int64) ([]npadmin.TenantSummary, error) {
	var tenants []npadmin.TenantSummary
	query := `
		SELECT t.id, t.name, t.domain, atm.role
		FROM tenants t
		JOIN account_tenant_memberships atm ON t.id = atm.tenant_id
		WHERE atm.account_id = $1
		ORDER BY t.name
	`
	if err := q.Select(&tenants, query, accountID); err != nil {
		return nil, fmt.Errorf("failed to fetch user tenants for account %d: %w", accountID, err)
	}

	return tenants, nil
}

// GetAllTenants retorna TODOS os tenants do sistema (ID, Nome, Domínio).
func (s TenantService) GetAllTenants(q npadmin.Queryable) ([]npadmin.TenantSummary, error) {
	var tenants []npadmin.TenantSummary
	query := `SELECT id, name, domain FROM tenants WHERE id != 1 ORDER BY name ASC`
	
	if err := q.Select(&tenants, query); err != nil {
		return nil, fmt.Errorf("failed to fetch all tenants: %w", err)
	}

	return tenants, nil
}

// EM: PI-backend/postgres/tenant.go

func (s *TenantService) Join(tx npadmin.Queryable, accountID int64, tenantID int64) error {
    // CORREÇÃO: Mudar 'member' para 'associate'
    query := `
        INSERT INTO account_tenant_memberships (account_id, tenant_id, role, created_at, updated_at)
        VALUES ($1, $2, 'associate', NOW(), NOW())
    `
    _, err := tx.Exec(query, accountID, tenantID)
    return err
}