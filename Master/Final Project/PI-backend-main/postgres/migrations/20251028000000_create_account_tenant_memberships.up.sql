-- Criar a tabela para associar contas a tenants e definir papéis específicos por tenant
CREATE TABLE IF NOT EXISTS account_tenant_memberships (
    id BIGSERIAL PRIMARY KEY,
    account_id BIGINT NOT NULL REFERENCES accounts(id) ON DELETE CASCADE, -- Chave estrangeira para a conta global
    tenant_id BIGINT NOT NULL REFERENCES tenants(id) ON DELETE CASCADE,   -- Chave estrangeira para o tenant
    role TEXT NOT NULL CHECK (role IN ('admin', 'associate')),           -- Papel do utilizador DENTRO deste tenant
    created_at TIMESTAMP NOT NULL DEFAULT (now() AT TIME ZONE 'utc'),
    updated_at TIMESTAMP NOT NULL DEFAULT (now() AT TIME ZONE 'utc'),

    -- Garantir que um utilizador só tem uma entrada por tenant
    UNIQUE (account_id, tenant_id)
);

-- Índices para otimizar pesquisas
CREATE INDEX IF NOT EXISTS idx_account_tenant_memberships_account_id ON account_tenant_memberships (account_id);
CREATE INDEX IF NOT EXISTS idx_account_tenant_memberships_tenant_id ON account_tenant_memberships (tenant_id);

-- Ativar RLS para esta nova tabela (política será definida noutra migração)
ALTER TABLE account_tenant_memberships ENABLE ROW LEVEL SECURITY;
ALTER TABLE account_tenant_memberships FORCE ROW LEVEL SECURITY; 