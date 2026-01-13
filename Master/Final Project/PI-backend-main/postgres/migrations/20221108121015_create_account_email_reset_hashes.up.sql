
CREATE TABLE
  IF NOT EXISTS account_email_reset_hashes (
    id bigint PRIMARY KEY REFERENCES accounts(id),
    hash text NOT NULL,
    email text NOT NULL,
    deadline timestamp(0) NOT NULL,
    created_at timestamp(0) NOT NULL DEFAULT (NOW() at time zone ('utc')),
    updated_at timestamp(0) NOT NULL DEFAULT (NOW() at time zone ('utc'))
  );
