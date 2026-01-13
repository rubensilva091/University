package npadmin

import "github.com/invisiblelab-dev/npadmin/crypto"

// Tenant representa um grupo não lucrativo no sistema.
// (Esta struct já deve existir devido às migrações, mas certifique-se que está completa)
// type Tenant struct {
//     ID        int64      `db:"id"`
//     UUID      string     `db:"uuid"` // Usar string para UUID
//     Name      string     `db:"name"`
//     Domain    NullString `db:"domain"` // Usar NullString se for opcional
//     Plan      string     `db:"plan"`
//     Metadata  []byte     `db:"metadata"` // Ou usar json.RawMessage ou similar
//     CreatedAt time.Time  `db:"created_at"`
//     UpdatedAt time.Time  `db:"updated_at"`
// }

// TenantCreateParams define os dados necessários para criar um novo tenant E o seu primeiro admin.
type TenantCreateParams struct {
	// Dados do Tenant
	TenantName string `json:"tenantName" validate:"required"`
	Domain     string `json:"domain"` // Opcional, talvez validar formato se fornecido

	// Dados do Administrador Inicial
	AdminFirstName string          `json:"adminFirstName" validate:"required"`
	AdminLastName  string          `json:"adminLastName" validate:"required"`
	AdminEmail     string          `json:"adminEmail" validate:"required,email"` // Validação de unicidade global será feita pelo serviço
	AdminNIF       string          `json:"adminNif" validate:"required,len=9"`   // Validação de unicidade por tenant será feita pelo serviço
	AdminPassword  crypto.Password `json:"adminPassword" validate:"required,min=8,has-numbers"`
	// AdminTLM       string          `json:"adminTlm" validate:"len=9|len=0"` // TLM Opcional?
}

// TenantSummary (NOVO) define a informação básica de um tenant para listar.
type TenantSummary struct {
	ID     int64      `db:"id" json:"id"`
	Name   string     `db:"name" json:"name"`
	Domain NullString `db:"domain" json:"domain"`
	Role   string     `db:"role" json:"role"`
}

// TenantService define a interface para operações relacionadas com tenants.
type TenantService interface {
	// Create cria um novo tenant e o seu utilizador admin inicial.
	// Retorna o ID do novo tenant e o ID da conta do admin.
	// Executa dentro de uma transação manual, pois não há contexto RLS inicial.
	Create(q Queryable, params TenantCreateParams) (tenantID int64, adminAccountID int64, err error)

	// GetUserTenants busca todos os tenants aos quais um utilizador (global accountID) pertence.
	// Esta query não deve ser restringida pela RLS de um *único* tenant.
	GetUserTenants(q Queryable, accountID int64) ([]TenantSummary, error)

	// GetAllTenants (NOVO) busca TODOS os tenants registados no sistema.
	// Útil para search bars públicas ou listagens globais.
	GetAllTenants(q Queryable) ([]TenantSummary, error)

	Join(tx Queryable, accountID int64, tenantID int64) error
}