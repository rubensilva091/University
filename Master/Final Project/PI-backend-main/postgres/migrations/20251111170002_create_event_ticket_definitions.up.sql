CREATE TABLE
  IF NOT EXISTS event_ticket_definitions (
    id BIGSERIAL PRIMARY KEY,
    tenant_id BIGINT NOT NULL REFERENCES tenants (id) ON DELETE CASCADE,
    event_id BIGINT NOT NULL REFERENCES events (id) ON DELETE CASCADE,
    name TEXT NOT NULL,
    price NUMERIC(10, 2) NOT NULL,
    available_quantity INT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT (now () AT TIME ZONE 'utc'),
    UNIQUE (event_id, name) -- Um evento não pode ter dois tipos de bilhete com o mesmo nome
  );

CREATE INDEX IF NOT EXISTS idx_event_ticket_definitions_tenant_id ON event_ticket_definitions (tenant_id);
CREATE INDEX IF NOT EXISTS idx_event_ticket_definitions_event_id ON event_ticket_definitions (event_id);

ALTER TABLE event_ticket_definitions ENABLE ROW LEVEL SECURITY;
ALTER TABLE event_ticket_definitions FORCE ROW LEVEL SECURITY;