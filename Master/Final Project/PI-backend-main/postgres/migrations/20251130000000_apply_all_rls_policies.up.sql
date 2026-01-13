-- migration: 20251130000000_apply_all_rls_policies.up.sql

-- 1. Helper Function: is_session_admin
-- Checks if the current user is an 'admin' for the current tenant.
CREATE OR REPLACE FUNCTION is_session_admin(tenant_id_in bigint)
RETURNS boolean
LANGUAGE sql
STABLE
SECURITY DEFINER
SET search_path = public
AS $$
  SELECT EXISTS (
    SELECT 1
    FROM account_tenant_memberships
    WHERE account_id = current_setting('myapp.user_id', true)::bigint
      AND tenant_id = tenant_id_in
      AND role = 'admin'
  );
$$;

-- Grant execution permission to the app user
GRANT EXECUTE ON FUNCTION is_session_admin(bigint) TO npadmin_app;

-- 2. Disable RLS on Global Tables
-- These tables contain global data (not tenant-scoped) or are handled by app logic.
ALTER TABLE IF EXISTS accounts DISABLE ROW LEVEL SECURITY;
ALTER TABLE IF EXISTS accounts NO FORCE ROW LEVEL SECURITY;
DROP POLICY IF EXISTS tenant_isolation_accounts ON accounts;

ALTER TABLE IF EXISTS account_password_hashes DISABLE ROW LEVEL SECURITY;
ALTER TABLE IF EXISTS account_password_hashes NO FORCE ROW LEVEL SECURITY;

-- 3. Apply Policies to Tenant-Scoped Tables
DO $$
DECLARE
  rec record;
  policy_sql TEXT;
  tenant_id_expr TEXT := E'current_setting(\'myapp.tenant_id\', true)::bigint';
  user_id_expr TEXT := E'current_setting(\'myapp.user_id\', true)::bigint';
BEGIN
  --
  -- GROUP A: Personal Data Tables
  -- Rule: Users see their OWN data; Admins see ALL data in the tenant.
  --
  FOR rec IN SELECT tbl, id_col FROM (VALUES
    ('profiles', 'id'),
    ('subscriptions', 'account_id'),
    ('subscriptions_history', 'account_id'),
    ('mbway_payments', 'account_id'),
    ('multibanco_payments', 'account_id'),
    ('purchased_tickets', 'account_id') 
  ) AS t(tbl, id_col)
  LOOP
    -- Enable RLS & Force it
    EXECUTE format('ALTER TABLE %I ENABLE ROW LEVEL SECURITY', rec.tbl);
    EXECUTE format('ALTER TABLE %I FORCE ROW LEVEL SECURITY', rec.tbl);
    EXECUTE format('DROP POLICY IF EXISTS tenant_isolation_%s ON %I', rec.tbl, rec.tbl);

    -- Create Policy
    -- USING: Tenant matches AND (It's my data OR I am an admin)
    -- CHECK: Tenant matches AND (It's my data - Admins usually don't create user data directly, or if they do, this allows it)
    policy_sql := format(
      'CREATE POLICY tenant_isolation_%I ON %I FOR ALL ' ||
      'USING ( tenant_id = %s AND ( %I = %s OR is_session_admin(%s) ) ) ' ||
      'WITH CHECK ( tenant_id = %s )', 
      rec.tbl, rec.tbl, tenant_id_expr, rec.id_col, user_id_expr, tenant_id_expr, tenant_id_expr
    );
    EXECUTE policy_sql;
  END LOOP;

  --
  -- GROUP B: Management Tables (Admin Only)
  -- Rule: Only Admins can see or touch these rows.
  --
  FOR rec IN SELECT tbl FROM (VALUES 
    ('orders')
  ) AS t(tbl)
  LOOP
    EXECUTE format('ALTER TABLE %I ENABLE ROW LEVEL SECURITY', rec.tbl);
    EXECUTE format('ALTER TABLE %I FORCE ROW LEVEL SECURITY', rec.tbl);
    EXECUTE format('DROP POLICY IF EXISTS tenant_isolation_%s ON %I', rec.tbl, rec.tbl);

    policy_sql := format(
      'CREATE POLICY tenant_isolation_%I ON %I FOR ALL ' ||
      'USING ( tenant_id = %s AND is_session_admin(%s) ) ' ||
      'WITH CHECK ( tenant_id = %s AND is_session_admin(%s) )',
      rec.tbl, rec.tbl, tenant_id_expr, tenant_id_expr, tenant_id_expr, tenant_id_expr
    );
    EXECUTE policy_sql;
  END LOOP;

  --
  -- GROUP C: Public Configuration/Event Tables
  -- Rule: Everyone in the tenant can READ; Only Admins can WRITE.
  --
  FOR rec IN SELECT tbl FROM (VALUES 
    ('prices'), 
    ('associate_category'),
    ('events'),                 
    ('event_ticket_definitions') 
  ) AS t(tbl)
  LOOP
    EXECUTE format('ALTER TABLE %I ENABLE ROW LEVEL SECURITY', rec.tbl);
    EXECUTE format('ALTER TABLE %I FORCE ROW LEVEL SECURITY', rec.tbl);
    
    EXECUTE format('DROP POLICY IF EXISTS tenant_read_%s ON %I', rec.tbl, rec.tbl);
    EXECUTE format('DROP POLICY IF EXISTS tenant_write_%s ON %I', rec.tbl, rec.tbl);

    -- READ Policy (Permissive Select)
    EXECUTE format(
      'CREATE POLICY tenant_read_%I ON %I FOR SELECT ' ||
      'USING ( tenant_id = %s )',
      rec.tbl, rec.tbl, tenant_id_expr
    );

    -- WRITE Policy (Admin Only)
    EXECUTE format(
      'CREATE POLICY tenant_write_%I ON %I FOR ALL ' ||
      'USING ( tenant_id = %s AND is_session_admin(%s) ) ' ||
      'WITH CHECK ( tenant_id = %s AND is_session_admin(%s) )',
      rec.tbl, rec.tbl, tenant_id_expr, tenant_id_expr, tenant_id_expr, tenant_id_expr
    );
  END LOOP;

  --
  -- GROUP D: Account Tenant Memberships (Special Case)
  -- Rule: Users can see their own memberships (to know they belong). Admins see all.
  -- Rule: Users can create their own membership (joining). Admins can manage all.
  --
  EXECUTE 'ALTER TABLE account_tenant_memberships ENABLE ROW LEVEL SECURITY';
  EXECUTE 'ALTER TABLE account_tenant_memberships FORCE ROW LEVEL SECURITY';
  EXECUTE 'DROP POLICY IF EXISTS atm_isolation ON account_tenant_memberships';

  -- Unified Policy for ATM
  -- SELECT: Tenant matches OR It is my user ID.
  -- WRITE: Tenant matches AND (It is my user ID OR I am admin).
  EXECUTE format(
    'CREATE POLICY atm_isolation ON account_tenant_memberships FOR ALL ' ||
    'USING ( tenant_id = %s OR account_id = %s ) ' ||
    'WITH CHECK ( tenant_id = %s AND (account_id = %s OR is_session_admin(%s)) )',
    tenant_id_expr, user_id_expr,
    tenant_id_expr, user_id_expr, tenant_id_expr
  );

END $$;