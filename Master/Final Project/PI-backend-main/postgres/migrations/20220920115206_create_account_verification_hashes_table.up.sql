CREATE TABLE IF NOT EXISTS account_verification_hashes (
    id bigint PRIMARY KEY REFERENCES accounts(id),
    tenant_id BIGINT REFERENCES tenants(id) ON DELETE SET NULL, -- ADD THIS
    hash text NOT NULL,
    deadline timestamp(0) NOT NULL,
    created_at timestamp(0) NOT NULL DEFAULT (NOW() at time zone ('utc')),
    updated_at timestamp(0) NOT NULL DEFAULT (NOW() at time zone ('utc'))
);

CREATE INDEX idx_account_verification_hashes_tenant_id ON account_verification_hashes (tenant_id);