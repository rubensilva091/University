package npadmin

import (
	"time"
)

// Profile representa os dados de um utilizador DENTRO de um tenant específico.
type Profile struct {
	// ID deve corresponder ao account_id global, mas a linha na tabela profiles é específica do tenant.
	// No entanto, a chave primária da tabela profiles ainda é 'id' que referencia accounts(id).
	// A combinação (id, tenant_id) seria única logicamente, mas a PK é só 'id'.
	// Vamos precisar de ajustar a query de criação/atualização para garantir que o tenant_id correto é usado.
	ID        int64      `db:"id" json:"id"` // Corresponde ao Account ID global
	FirstName NullString `db:"first_name" json:"firstName"`
	LastName  NullString `db:"last_name" json:"lastName"`
	CreatedAt time.Time  `db:"created_at" json:"createdAt"`
	UpdatedAt time.Time  `db:"updated_at" json:"updatedAt"`
	// Role foi movido para account_tenant_memberships
	// Role      string     `db:"role" json:"role"` // REMOVIDO
	NIF      NullString `db:"nif" json:"nif"` // A unicidade do NIF pode precisar ser por tenant? (Atual validação é global)
	TLM      NullString `db:"tlm" json:"tlm"`
	Category int64      `db:"category_id" json:"category_id"`
	TenantID int64      `db:"tenant_id" json:"-"` // Mantido e crucial para RLS e lógica
}

// User continua a representar a informação global básica da conta.
type User struct {
	ID        int64     `db:"id"`
	Email     string    `db:"email"`
	CreatedAt time.Time `db:"created_at"`
}

// ProfileService interface precisa ser ajustada.
type ProfileService interface {
	// Get agora precisa implicitamente do tenant_id (via RLS) e do account_id (id).
	Get(Queryable, int64) (*Profile, error)
	// GetByEmail pode tornar-se ambíguo. É melhor buscar o Account global por email,
	// e depois buscar o Profile específico do tenant usando o account_id e o tenant_id do contexto.
	// Vamos manter por agora, mas a implementação terá de considerar o tenant_id via RLS.
	GetByEmail(Queryable, string) (*Profile, error)
	// GetAsUser busca dados globais, assinatura ok.
	GetAsUser(Queryable, int64) (*User, error)
	// GetAsUsers busca dados globais, assinatura ok.
	GetAsUsers(Queryable, ...int64) ([]User, error)
	// Create agora precisa explicitamente do tenant_id, provavelmente passado como argumento ou obtido via RLS.
	// A struct Profile já tem TenantID, que pode ser preenchido antes de chamar Create.
	Create(Queryable, *Profile) error
	// UpdateProfile precisa do ID (account_id) e implicitamente do tenant_id (RLS).
	UpdateProfile(Queryable, ProfileUpdate) error
	// ChangeRole foi movido para account_tenant_memberships, deve ser removido daqui.
	// ChangeRole(Queryable, int64, string) error // REMOVIDO
	// Destroy precisa do ID (account_id) e implicitamente do tenant_id (RLS).
	Destroy(Queryable, Profile) error

	// Nova função para gerir roles na tabela de memberships
	UpdateMembershipRole(Queryable, int64, int64, string) error // accountID, tenantID, newRole
}

// ProfileUpdate precisa do ID (account_id) para saber qual perfil atualizar.
// O tenant_id será aplicado via RLS. Email removido, não se atualiza aqui.
type ProfileUpdate struct {
	ID        int64      // Account ID global
	FirstName NullString `json:"firstName"`
	LastName  NullString `json:"lastName"`
	NIF       NullString `json:"nif"` // Validar unicidade por tenant na implementação
	TLM       NullString `json:"tlm"`
	// Email     string // REMOVIDO - Email é global, atualizado via AccountService
}

// ProfileUpdateParams para validação do request. Email removido.
type ProfileUpdateParams struct {
	// ID não vem no request, é obtido do user autenticado ou path/query param
	FirstName NullString `validate:"required"`
	LastName  NullString `validate:"required"`
	NIF       NullString `validate:"required,len=9"` // Validar unicidade por tenant na implementação
	TLM       NullString `validate:"len=9|len=0"`
	// Email string // REMOVIDO
}