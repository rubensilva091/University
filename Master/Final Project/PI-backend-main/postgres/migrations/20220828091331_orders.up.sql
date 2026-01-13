CREATE TABLE orders (
    id bigserial PRIMARY KEY,
    -- Add the tenant_id directly here
    tenant_id BIGINT REFERENCES tenants(id) ON DELETE CASCADE,
    created_at timestamp(0) NOT NULL DEFAULT (now() AT TIME ZONE 'utc'::text),
    updated_at timestamp(0) NOT NULL DEFAULT (now() AT TIME ZONE 'utc'::text)
);
-- Add the index immediately
CREATE INDEX idx_orders_tenant_id ON orders (tenant_id);