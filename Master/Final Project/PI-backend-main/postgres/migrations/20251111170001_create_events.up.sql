CREATE TABLE
  IF NOT EXISTS events (
    id BIGSERIAL PRIMARY KEY,
    tenant_id BIGINT NOT NULL REFERENCES tenants (id) ON DELETE CASCADE,
    name TEXT NOT NULL,
    description TEXT,
    event_date TIMESTAMP WITH TIME ZONE NOT NULL,
    location TEXT,
    image_url TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT (now () AT TIME ZONE 'utc'),
    updated_at TIMESTAMP NOT NULL DEFAULT (now () AT TIME ZONE 'utc')
  );

CREATE INDEX IF NOT EXISTS idx_events_tenant_id ON events (tenant_id);
CREATE INDEX IF NOT EXISTS idx_events_event_date ON events (event_date);

ALTER TABLE events ENABLE ROW LEVEL SECURITY;
ALTER TABLE events FORCE ROW LEVEL SECURITY;