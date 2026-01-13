CREATE TABLE
  IF NOT EXISTS purchased_tickets (
    id BIGSERIAL PRIMARY KEY,
    tenant_id BIGINT NOT NULL REFERENCES tenants (id) ON DELETE CASCADE,
    event_id BIGINT NOT NULL REFERENCES events (id) ON DELETE CASCADE,
    definition_id BIGINT NOT NULL REFERENCES event_ticket_definitions (id) ON DELETE CASCADE,
    account_id BIGINT NOT NULL REFERENCES accounts (id) ON DELETE CASCADE, -- O comprador
    
    status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending', 'paid', 'cancelled')),
    
    -- Colunas para ligar ao vosso sistema de pagamentos
    payment_order_id BIGINT UNIQUE, -- ID da tabela 'orders'
    payment_reference TEXT,          -- Referência Multibanco
    payment_request_id TEXT,         -- ID do pedido MBWay
    
    qr_code_token TEXT UNIQUE,     -- Token único para o QR Code
    
    created_at TIMESTAMP NOT NULL DEFAULT (now () AT TIME ZONE 'utc'),
    updated_at TIMESTAMP NOT NULL DEFAULT (now () AT TIME ZONE 'utc')
  );

CREATE INDEX IF NOT EXISTS idx_purchased_tickets_tenant_id ON purchased_tickets (tenant_id);
CREATE INDEX IF NOT EXISTS idx_purchased_tickets_account_id ON purchased_tickets (account_id);
CREATE INDEX IF NOT EXISTS idx_purchased_tickets_event_id ON purchased_tickets (event_id);
CREATE INDEX IF NOT EXISTS idx_purchased_tickets_order_id ON purchased_tickets (payment_order_id);

ALTER TABLE purchased_tickets ENABLE ROW LEVEL SECURITY;
ALTER TABLE purchased_tickets FORCE ROW LEVEL SECURITY;