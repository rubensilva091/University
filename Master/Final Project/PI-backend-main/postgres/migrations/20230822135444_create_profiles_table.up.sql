CREATE TABLE IF NOT EXISTS profiles (
    id bigint PRIMARY KEY REFERENCES accounts(id),
    tenant_id BIGINT REFERENCES tenants(id) ON DELETE CASCADE, -- Added
    first_name text,
    last_name text,
    nif text NOT NULL,
    tlm text,
    role text,
    category_id integer NOT NULL REFERENCES associate_category (id),
    created_at timestamp(0) NOT NULL DEFAULT (NOW() at time zone ('utc')),
    updated_at timestamp(0) NOT NULL DEFAULT (NOW() at time zone ('utc'))
);

-- Scoped Unique Indexes
CREATE UNIQUE INDEX profiles_nif_tenant_id_idx ON profiles (nif, tenant_id);
CREATE UNIQUE INDEX profiles_tlm_tenant_id_idx ON profiles (tlm, tenant_id);
CREATE INDEX idx_profiles_tenant_idx ON profiles (tenant_id);