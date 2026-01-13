package postgres

import (
	"database/sql"
	"errors"
	"fmt"
	"strconv"
	"strings" // Importar strings

	"github.com/invisiblelab-dev/npadmin"
	"github.com/invisiblelab-dev/npadmin/crypto"
)

// Definir o ID do tenant "default". A migração 20251024220534 garante que
// o tenant 'default' (localhost) é criado. Sendo BIGSERIAL, o ID dele deve ser 1.
const DefaultTenantID int64 = 1

// EnsureFixedCategories cria as categorias estáticas do sistema
func EnsureFixedCategories(db *DB) error {
	categories := []struct {
		Name string
		Desc string
	}{
		{"Criança", "Até aos 12 anos"},
		{"Jovem", "Dos 13 aos 17 anos"},
		{"Estudante", "Dos 18 aos 25 anos, estudante"},
		{"Adulto", "Categoria geral (+18)"},
		{"Idoso", "Maiores de 65 anos"},
	}

	return Transaction(db, func(tx npadmin.Queryable) error {
		query := `
			INSERT INTO associate_category (tenant_id, name, description)
			VALUES ($1, $2, $3)
			ON CONFLICT (name, tenant_id) DO NOTHING
		`
		// Usamos o DefaultTenantID (1) para "segurar" estas categorias globais
		for _, cat := range categories {
			if _, err := tx.Exec(query, DefaultTenantID, cat.Name, cat.Desc); err != nil {
				return fmt.Errorf("failed to seed fixed category '%s': %w", cat.Name, err)
			}
		}
		return nil
	})
}

// CreateAdmin garante que o administrador default (do config) existe E
// está associado como 'admin' ao tenant default (ID 1).
func CreateAdmin(adminEmail string, nif string, db *DB) error {
	if err := EnsureFixedCategories(db); err != nil {
        return fmt.Errorf("setup: failed to ensure fixed categories: %w", err)
    }
	
	var account npadmin.Account
	account.Email = adminEmail
	// account.Role = "admin" // O 'Role' já não é guardado diretamente aqui
	account.Category = 1 // Categoria default (assumindo 'jovem' ou 'normal' ID 1)
	account.StatusID = npadmin.AccountVerified // Admin é verificado por defeito
	if nif == "" {
		account.NIF.String = "000000000" // Default NIF se vazio
	} else {
		account.NIF.String = nif
	}
	account.NIF.Valid = true
	account.TLM = npadmin.NullString{} // TLM vazio por defeito
	account.FirstName = npadmin.NewNullString("Admin")
	account.LastName = npadmin.NewNullString("User")


	randomPassword, err := crypto.RandomPassword()
	if err != nil {
		return fmt.Errorf("failed to generate random password: %w", err)
	}
	account.Password = randomPassword

	// 1. Verificar se a conta GLOBAL já existe
	query := "SELECT id FROM accounts WHERE email = $1"
	var adminAccountID int64
	//nolint:noctx // Context not needed for this startup query
	err = db.QueryRow(query, adminEmail).Scan(&adminAccountID)

	// 2. Se a conta global já existe (err == nil)
	if err == nil {
		// A conta existe. Apenas garantir que tem perfil E membership de admin no tenant default.
		
		errTx := Transaction(db, func(tx npadmin.Queryable) error {
			// *** INÍCIO DA CORREÇÃO RLS ***
			// Definir Contexto RLS para garantir que as inserções/atualizações funcionam
			tenantIDStr := strconv.FormatInt(DefaultTenantID, 10)
			if _, err := tx.Exec("SELECT set_config('myapp.tenant_id', $1, true)", tenantIDStr); err != nil {
				return fmt.Errorf("failed to set tenant config in existing admin setup: %w", err)
			}
			accountIDStr := strconv.FormatInt(adminAccountID, 10)
			if _, err := tx.Exec("SELECT set_config('myapp.user_id', $1, true)", accountIDStr); err != nil {
				return fmt.Errorf("failed to set user config in existing admin setup: %w", err)
			}
			// *** FIM DA CORREÇÃO RLS ***

			// Garantir Perfil no tenant 1 (IGNORAR CONFLITO se já existir)
			// *** INÍCIO DA CORREÇÃO ON CONFLICT ***
			// A PK da tabela profiles é 'id', não '(id, tenant_id)'.
			profileQuery := `
				INSERT INTO profiles (id, first_name, last_name, nif, tlm, category_id, tenant_id) 
				VALUES ($1, $2, $3, $4, $5, $6, $7)
				ON CONFLICT (id) DO NOTHING` // Ignora se a conta já tem um perfil (em *qualquer* tenant)
			// *** FIM DA CORREÇÃO ON CONFLICT ***
			
			argsProfile := []any{adminAccountID, account.FirstName, account.LastName, account.NIF, account.TLM, account.Category, DefaultTenantID}
			if _, err := tx.Exec(profileQuery, argsProfile...); err != nil {
				return fmt.Errorf("failed to ensure admin profile entry: %w", err)
			}

			// Garantir Membership de Admin no tenant 1 (ATUALIZAR se já existir)
			membershipQuery := `
				INSERT INTO account_tenant_memberships (account_id, tenant_id, role)
				VALUES ($1, $2, $3)
				ON CONFLICT (account_id, tenant_id) DO UPDATE SET role = EXCLUDED.role` // Garante que é 'admin'

			argsMembership := []any{adminAccountID, DefaultTenantID, "admin"}
			if _, errMembership := tx.Exec(membershipQuery, argsMembership...); errMembership != nil {
				return fmt.Errorf("failed to ensure admin membership for existing account: %w", errMembership)
			}
			return nil
		})
		
		if errTx != nil {
			return fmt.Errorf("failed transaction for ensuring existing admin: %w", errTx)
		}
		// Admin global já existia, perfil e membership garantidos.
		return nil
	}

	// 3. Se a conta não existe (sql.ErrNoRows), criar tudo.
	if !errors.Is(err, sql.ErrNoRows) {
		// Erro inesperado ao verificar conta
		return fmt.Errorf("failed to check existing account: %w", err)
	}

	// Definir as queries corretas para criação
	accountQuery := `
		INSERT INTO accounts (email, status_id)
		VALUES ($1, $2)
		RETURNING id, (SELECT name FROM account_status WHERE id = $2)`

	passwordQuery := "INSERT INTO account_password_hashes (id, hash) VALUES ($1, $2)"

	// CORRIGIDO: Remover 'role', Adicionar 'tenant_id'
	profileQuery := `INSERT INTO profiles (id, first_name, last_name, nif, tlm, category_id, tenant_id) 
	VALUES ($1, $2, $3, $4, $5, $6, $7)`

	// ADICIONADO: Query de Membership
	membershipQuery := `INSERT INTO account_tenant_memberships (account_id, tenant_id, role)
	VALUES ($1, $2, $3)`

	// Executar a criação transacional
	err = Transaction(db, func(tx npadmin.Queryable) error {
		// Criar conta global
		args := []any{account.Email, account.StatusID}
		if err := tx.QueryRow(accountQuery, args...).Scan(&account.ID, &account.StatusDescription); err != nil {
			return fmt.Errorf("failed to add new admin account entry: %w", err)
		}

		// Criar hash da password
		args = []any{account.ID, account.Password.Hash}
		if _, err := tx.Exec(passwordQuery, args...); err != nil {
			return fmt.Errorf("failed to add new admin password entry: %w", err)
		}

		// *** INÍCIO DA CORREÇÃO RLS ***
		// Definir Contexto RLS antes de inserir em profiles/memberships
		tenantIDStr := strconv.FormatInt(DefaultTenantID, 10)
		if _, err := tx.Exec("SELECT set_config('myapp.tenant_id', $1, true)", tenantIDStr); err != nil {
			return fmt.Errorf("failed to set tenant config in new admin setup: %w", err)
		}
		accountIDStr := strconv.FormatInt(account.ID, 10) // Usar account.ID que acabámos de obter
		if _, err := tx.Exec("SELECT set_config('myapp.user_id', $1, true)", accountIDStr); err != nil {
			return fmt.Errorf("failed to set user config in new admin setup: %w", err)
		}
		// *** FIM DA CORREÇÃO RLS ***

		// Validar NIF (opcional, mas mantido)
		if _, err := strconv.Atoi(account.NIF.String); err != nil {
			return fmt.Errorf("NIF should be number: %w", err)
		}

		// Criar perfil para o tenant default (ID 1)
		// CORRIGIDO: Usar DefaultTenantID, remover account.Role
		argsProfile := []any{account.ID, account.FirstName, account.LastName, account.NIF, account.TLM, account.Category, DefaultTenantID}
		if _, err := tx.Exec(profileQuery, argsProfile...); err != nil {
			if strings.Contains(err.Error(), "profiles_nif_tenant_id_idx") {
				return fmt.Errorf("failed to add admin profile: NIF '%s' already exists in default tenant: %w", account.NIF.String, err)
			}
			return fmt.Errorf("failed to add admin profile entry: %w", err)
		}

		// ADICIONADO: Criar membership para o tenant default
		argsMembership := []any{account.ID, DefaultTenantID, "admin"}
		if _, err := tx.Exec(membershipQuery, argsMembership...); err != nil {
			return fmt.Errorf("failed to add admin membership entry: %w", err)
		}

		return nil
	})
	if err != nil {
		return fmt.Errorf("failed to create admin transaction: %w", err)
	}
	return nil
}