-- Grant permissions for the application user on ALL tables it needs to write to.
GRANT SELECT, INSERT, UPDATE, DELETE ON tenants TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON account_tenant_memberships TO npadmin_app;

-- Grant permissions for tables accessed during tenant/admin creation
GRANT SELECT, INSERT, UPDATE, DELETE ON accounts TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON account_password_hashes TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON profiles TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON associate_category TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON prices TO npadmin_app;

-- Grant permissions for tables accessed during login/auth
GRANT SELECT, INSERT, UPDATE, DELETE ON account_jwt_refresh_keys TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON account_login_hashes TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON account_verification_hashes TO npadmin_app;

-- Grant permissions for tables accessed by admin/associate dashboard
GRANT SELECT, INSERT, UPDATE, DELETE ON subscriptions TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON subscriptions_history TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON orders TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON multibanco_payments TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON mbway_payments TO npadmin_app;


-- Grant usage on ALL relevant sequences
GRANT USAGE ON SEQUENCE tenants_id_seq TO npadmin_app;
GRANT USAGE ON SEQUENCE account_tenant_memberships_id_seq TO npadmin_app;
GRANT USAGE ON SEQUENCE accounts_id_seq TO npadmin_app;
GRANT USAGE ON SEQUENCE associate_category_id_seq TO npadmin_app;
GRANT USAGE ON SEQUENCE subscriptions_id_seq TO npadmin_app;
GRANT USAGE ON SEQUENCE subscriptions_history_id_seq TO npadmin_app;
GRANT USAGE ON SEQUENCE orders_id_seq TO npadmin_app;
GRANT USAGE ON SEQUENCE multibanco_payments_id_seq TO npadmin_app;
GRANT USAGE ON SEQUENCE mbway_payments_id_seq TO npadmin_app;


-- Grant permissions on account_status (needed for SELECT)
GRANT SELECT ON account_status TO npadmin_app;