
CREATE TABLE mbway_payments (
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    amount text NOT NULL,
    tenant_id BIGINT REFERENCES tenants(id) ON DELETE CASCADE,
    order_date timestamp(0) NOT NULL,
    payment_status boolean NOT NULL DEFAULT false,
    request_id text NOT NULL UNIQUE,
    order_id bigint REFERENCES orders(id) NOT NULL,
    account_id bigint REFERENCES accounts(id) NOT NULL,
    period integer NOT NULL,
    created_at timestamp(0) NOT NULL DEFAULT (now() AT TIME ZONE 'utc'::text),
    updated_at timestamp(0) NOT NULL DEFAULT (now() AT TIME ZONE 'utc'::text)
);

-- CORRECT: Index for the mbway table
CREATE INDEX idx_mbway_payments_tenant_id ON mbway_payments (tenant_id);