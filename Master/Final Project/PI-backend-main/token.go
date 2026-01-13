package npadmin

import "time"

// VerificationToken struct AGORA inclui TenantID (NullInt64)
type VerificationToken struct {
	ID        int64     `db:"id"`
	Hash      string    `db:"hash"`
	CreatedAt time.Time `db:"created_at"`
	UpdatedAt time.Time `db:"updated_at"`
	Deadline  time.Time `db:"deadline"`
	TenantID  NullInt64 `db:"tenant_id"` // <-- ALTERADO: Usar NullInt64
}

type TokenParams struct {
	Token string `validate:"required,len=32"`
}

// VerificationEmailToken também atualizado para consistência (embora a migração não o tenha adicionado ainda)
type VerificationEmailToken struct {
	ID        int64     `db:"id"`
	Email     string    `db:"email"`
	Hash      string    `db:"hash"`
	CreatedAt time.Time `db:"created_at"`
	UpdatedAt time.Time `db:"updated_at"`
	Deadline  time.Time `db:"deadline"`
	TenantID  NullInt64 `db:"tenant_id"` // <-- ALTERADO: Usar NullInt64 para consistência
}