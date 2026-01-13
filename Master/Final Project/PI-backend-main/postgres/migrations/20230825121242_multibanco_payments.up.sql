
CREATE TABLE multibanco_payments (
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    tenant_id BIGINT REFERENCES tenants(id) ON DELETE CASCADE,
    entity text NOT NULL,
    reference text NOT NULL,
    amount text NOT NULL,
    payment_status boolean NOT NULL DEFAULT false,
    order_id bigint REFERENCES orders(id) NOT NULL,
    account_id bigint REFERENCES accounts(id) NOT NULL,
    period integer NOT NULL,
    created_at timestamp(0) NOT NULL DEFAULT (now() AT TIME ZONE 'utc'::text),
    updated_at timestamp(0) NOT NULL DEFAULT (now() AT TIME ZONE 'utc'::text)
);

-- CORRECT: Index for the multibanco table
CREATE INDEX idx_multibanco_payments_tenant_id ON multibanco_payments (tenant_id);