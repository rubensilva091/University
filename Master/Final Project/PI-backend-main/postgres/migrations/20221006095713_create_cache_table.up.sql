
CREATE TABLE
  IF NOT EXISTS caches (
    id bigint PRIMARY KEY REFERENCES accounts (id),
    entries jsonb,
    created_at timestamp(0) NOT NULL DEFAULT (NOW () at time zone ('utc')),
    updated_at timestamp(0) NOT NULL DEFAULT (NOW () at time zone ('utc'))
  );

