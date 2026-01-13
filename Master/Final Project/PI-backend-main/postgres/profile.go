package postgres

import (
	"errors"
	"fmt"

	"github.com/invisiblelab-dev/npadmin"
	"github.com/lib/pq"
)

type ProfileService struct {
	db *DB
}

func NewProfileService(db *DB) *ProfileService {
	return &ProfileService{db: db}
}

// Get busca o perfil do utilizador (id global) DENTRO do tenant atual (definido por RLS em 'q').
func (s ProfileService) Get(q npadmin.Queryable, id int64) (*npadmin.Profile, error) {
	var profile npadmin.Profile
	// A RLS na tabela 'profiles' deve filtrar pelo tenant_id = current_setting('myapp.tenant_id')
	query := `SELECT id, first_name, last_name, nif, tlm, category_id, tenant_id, created_at, updated_at FROM profiles WHERE id = $1`
	// Usar 'q' que já tem o contexto RLS (tenant_id)
	if err := q.Get(&profile, query, id); err != nil {
		// Tratar sql.ErrNoRows ou outros erros
		return nil, fmt.Errorf("failed to fetch profile by id %d (in current tenant context): %w", id, err)
	}
	// O tenant_id será preenchido na struct pelo Get, confirmando o contexto
	return &profile, nil
}

// GetByEmail busca o perfil associado a um email DENTRO do tenant atual (definido por RLS em 'q').
func (s ProfileService) GetByEmail(q npadmin.Queryable, email string) (*npadmin.Profile, error) {
	var profile npadmin.Profile
	// Query junta accounts (global) com profiles (tenant-specific via RLS)
	// A RLS em 'profiles' filtra pelo tenant_id = current_setting('myapp.tenant_id')
	query := `
		SELECT p.id, p.first_name, p.last_name, p.nif, p.tlm, p.category_id, p.tenant_id, p.created_at, p.updated_at
		FROM profiles p
		JOIN accounts a ON p.id = a.id
		WHERE a.email = $1` // Filtra pelo email global
	// Usar 'q' que já tem o contexto RLS (tenant_id)
	if err := q.Get(&profile, query, email); err != nil {
		// Tratar sql.ErrNoRows ou outros erros
		return nil, fmt.Errorf("failed to fetch profile by email '%s' (in current tenant context): %w", email, err)
	}
	return &profile, nil
}

// Create cria uma entrada na tabela 'profiles'. Assume que 'profile.TenantID' está preenchido corretamente.
// A RLS garantirá que a inserção só funciona se profile.TenantID corresponder ao contexto.
func (s ProfileService) Create(q npadmin.Queryable, profile *npadmin.Profile) error {
	// Query para inserir perfil, incluindo id (account_id), tenant_id e outros campos.
	// Role foi removido da struct Profile.
	query := `INSERT INTO profiles (id, first_name, last_name, nif, tlm, category_id, tenant_id)
	          VALUES ($1, $2, $3, $4, $5, $6, $7)`

	// Verificar se o TenantID está definido na struct
	if profile.TenantID == 0 {
		return errors.New("cannot create profile: TenantID is missing")
	}

	args := []any{
		profile.ID,
		profile.FirstName,
		profile.LastName,
		profile.NIF,
		profile.TLM,
		profile.Category,
		profile.TenantID, // Usar o TenantID da struct
	}

	// Usar 'q' que tem o contexto RLS. A política WITH CHECK deve validar o tenant_id.
	_, err := q.Exec(query, args...)
	if err != nil {
		// Verificar erros específicos (e.g., violação de constraint, RLS check falhou)
		return fmt.Errorf("failed to create profile entry for account %d, tenant %d: %w", profile.ID, profile.TenantID, err)
	}
	return nil
}

// GetAsUser busca dados globais da conta. Não depende do tenant RLS.
func (s ProfileService) GetAsUser(q npadmin.Queryable, id int64) (*npadmin.User, error) {
	var user npadmin.User
	// Query busca apenas da tabela global 'accounts'
	query := `SELECT id, email, created_at FROM accounts WHERE id = $1`

	// Usar 'q' (embora o contexto RLS não seja relevante aqui)
	if err := q.Get(&user, query, id); err != nil {
		return nil, fmt.Errorf("failed to get 'as user' by id %d: %w", id, err)
	}
	return &user, nil
}

// GetAsUsers busca dados globais de múltiplas contas. Não depende do tenant RLS.
func (s ProfileService) GetAsUsers(q npadmin.Queryable, ids ...int64) ([]npadmin.User, error) {
	var users []npadmin.User
	// Query busca apenas da tabela global 'accounts'
	query := `SELECT id, email, created_at FROM accounts WHERE id = ANY ($1)`

	// Usar 'q' (embora o contexto RLS não seja relevante aqui)
	if err := q.Select(&users, query, pq.Array(ids)); err != nil {
		return nil, fmt.Errorf("failed to fetch users by ids: %w", err)
	}
	return users, nil
}

// UpdateProfile atualiza dados do perfil DENTRO do tenant atual (definido por RLS em 'q').
func (s ProfileService) UpdateProfile(q npadmin.Queryable, newProfile npadmin.ProfileUpdate) error {
	// Query atualiza 'profiles'. A RLS garante que só atualiza a linha correta para o 'id' (account_id)
	// DENTRO do tenant_id = current_setting('myapp.tenant_id').
	// Role foi removido. NIF uniqueness precisa ser tratado na validação ou aqui (mais complexo).
	query := `UPDATE profiles
	          SET first_name = $1, last_name = $2, tlm = $3, nif = $4, updated_at = now() at time zone 'utc'
	          WHERE id = $5` // A RLS adiciona implicitamente 'AND tenant_id = ...'
	args := []any{
		newProfile.FirstName,
		newProfile.LastName,
		newProfile.TLM,
		newProfile.NIF,
		newProfile.ID, // ID da conta global
	}

	// Usar 'q' que tem o contexto RLS
	result, err := q.Exec(query, args...)
	if err != nil {
		// Verificar erros específicos (e.g., violação de constraint de NIF se for única por tenant)
		return fmt.Errorf("failed to update profile for account %d (in current tenant context): %w", newProfile.ID, err)
	}
	affected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("failed to fetch affected rows for profile update (account %d): %w", newProfile.ID, err)
	}

	if affected == 0 {
		// Isto pode acontecer se o utilizador não tiver perfil neste tenant (RLS falhou) ou ID inválido.
		return fmt.Errorf("profile update failed: no profile found for account %d in the current tenant context (or ID invalid)", newProfile.ID)
	}
	if affected > 1 {
		// Isto não devia acontecer com a RLS e PK corretas.
		return fmt.Errorf("profile update error: unexpected number of rows affected (%d) for account %d", affected, newProfile.ID)
	}

	return nil
}

// ChangeRole foi REMOVIDO. A lógica agora está em UpdateMembershipRole.
// func (s ProfileService) ChangeRole(...) { ... }

// UpdateMembershipRole atualiza o 'role' na tabela 'account_tenant_memberships'.
// Esta operação precisa de privilégios (e.g., ser admin do tenant). A RLS na tabela memberships pode controlar isto.
func (s ProfileService) UpdateMembershipRole(q npadmin.Queryable, accountID int64, tenantID int64, newRole string) error {
	// Validar newRole? (e.g., 'admin' ou 'associate')
	if newRole != "admin" && newRole != "associate" {
		return fmt.Errorf("invalid role specified: %s", newRole)
	}

	// Query atualiza a tabela de memberships.
	// A RLS nesta tabela pode impedir a operação se o utilizador atual (myapp.user_id) não for admin do tenant (myapp.tenant_id).
	// A query em si especifica account_id e tenant_id a serem atualizados.
	query := `UPDATE account_tenant_memberships
	          SET role = $1, updated_at = now() at time zone 'utc'
	          WHERE account_id = $2 AND tenant_id = $3`
	args := []any{newRole, accountID, tenantID}

	// Usar 'q' que tem o contexto RLS (user_id e tenant_id do *ator* da ação)
	result, err := q.Exec(query, args...)
	if err != nil {
		// Verificar se o erro foi devido à RLS (permissão negada) ou outro erro
		return fmt.Errorf("failed to update membership role for account %d, tenant %d: %w", accountID, tenantID, err)
	}
	affected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("failed to fetch affected rows for membership role update (account %d, tenant %d): %w", accountID, tenantID, err)
	}

	if affected == 0 {
		// Significa que a combinação account_id/tenant_id não existe, ou a RLS impediu a atualização.
		return fmt.Errorf("membership role update failed: membership not found for account %d, tenant %d (or permission denied by RLS)", accountID, tenantID)
	}
	if affected > 1 {
		// Não deve acontecer devido à UNIQUE constraint.
		return fmt.Errorf("membership role update error: unexpected number of rows affected (%d) for account %d, tenant %d", affected, accountID, tenantID)
	}

	return nil
}


// Destroy remove a entrada do perfil DENTRO do tenant atual (definido por RLS em 'q').
// NÃO remove a conta global nem outras memberships.
func (s ProfileService) Destroy(q npadmin.Queryable, profile npadmin.Profile) error {
	// Query para apagar de 'profiles'. A RLS garante que só apaga DENTRO do tenant atual.
	query := `DELETE FROM profiles WHERE id = $1` // RLS adiciona 'AND tenant_id = ...'

	// Usar 'q' que tem o contexto RLS
	result, err := q.Exec(query, profile.ID)
	if err != nil {
		return fmt.Errorf("failed to delete profile for account %d (in current tenant context): %w", profile.ID, err)
	}
	affected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("failed to fetch affected rows for profile deletion (account %d): %w", profile.ID, err)
	}
	if affected == 0 {
		return fmt.Errorf("profile deletion failed: no profile found for account %d in the current tenant context", profile.ID)
	}

	// Considerar também apagar a entrada em 'account_tenant_memberships'? Ou só o perfil?
	// Por agora, só apaga o perfil.

	return nil
	// return errors.New("not implemented") // Remover esta linha
}