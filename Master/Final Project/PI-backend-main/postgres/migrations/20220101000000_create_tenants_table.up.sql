CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS tenants (
  id BIGSERIAL PRIMARY KEY,
  uuid UUID DEFAULT gen_random_uuid() UNIQUE,
  name TEXT NOT NULL,
  domain TEXT UNIQUE,
  plan TEXT DEFAULT 'free',
  metadata JSONB DEFAULT '{}'::jsonb,
  created_at TIMESTAMP NOT NULL DEFAULT (now() AT TIME ZONE 'utc'),
  updated_at TIMESTAMP NOT NULL DEFAULT (now() AT TIME ZONE 'utc')
);

-- Default tenant
INSERT INTO tenants (name, domain, plan)
SELECT 'default','localhost','free'
WHERE NOT EXISTS (SELECT 1 FROM tenants WHERE name = 'default');