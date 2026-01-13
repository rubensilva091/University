

CREATE TABLE
  IF NOT EXISTS account_jwt_refresh_keys (
    id uuid PRIMARY KEY NOT NULL,
    account_id bigint REFERENCES accounts(id) NOT NULL,
    deadline timestamp(0) NOT NULL,
    created_at timestamp(0) NOT NULL DEFAULT (NOW() at time zone ('utc'))
  );


