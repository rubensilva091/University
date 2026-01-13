CREATE TABLE IF NOT EXISTS prices (
    category_id integer NOT NULL REFERENCES associate_category(id),
    tenant_id BIGINT REFERENCES tenants(id) ON DELETE CASCADE, -- ADD THIS
    period integer NOT NULL,
    price NUMERIC(7,2) NOT NULL,
    created_at timestamp(0) NOT NULL DEFAULT (NOW() at time zone ('utc')),
    updated_at timestamp(0) NOT NULL DEFAULT (NOW() at time zone ('utc')),
    PRIMARY KEY (category_id, period)
);

-- ADD THIS INDEX
CREATE INDEX idx_prices_tenant_idx ON prices (tenant_id);