-- Revoke permissions for the application user
REVOKE SELECT, INSERT, UPDATE, DELETE ON tenants FROM npadmin_app;
REVOKE SELECT, INSERT, UPDATE, DELETE ON account_tenant_memberships FROM npadmin_app;
REVOKE SELECT, INSERT, UPDATE, DELETE ON accounts FROM npadmin_app;
REVOKE SELECT, INSERT, UPDATE, DELETE ON account_password_hashes FROM npadmin_app;
REVOKE SELECT, INSERT, UPDATE, DELETE ON profiles FROM npadmin_app;
REVOKE SELECT, INSERT, UPDATE, DELETE ON associate_category FROM npadmin_app;
REVOKE SELECT, INSERT, UPDATE, DELETE ON prices FROM npadmin_app;

-- Revoke permissions for tables accessed during login/auth
REVOKE SELECT, INSERT, UPDATE, DELETE ON account_jwt_refresh_keys FROM npadmin_app;
REVOKE SELECT, INSERT, UPDATE, DELETE ON account_login_hashes FROM npadmin_app;
REVOKE SELECT, INSERT, UPDATE, DELETE ON account_verification_hashes FROM npadmin_app;

-- Revoke permissions for tables accessed by admin/associate dashboard
REVOKE SELECT, INSERT, UPDATE, DELETE ON subscriptions FROM npadmin_app;
REVOKE SELECT, INSERT, UPDATE, DELETE ON subscriptions_history FROM npadmin_app;
REVOKE SELECT, INSERT, UPDATE, DELETE ON orders FROM npadmin_app;
REVOKE SELECT, INSERT, UPDATE, DELETE ON multibanco_payments FROM npadmin_app;
REVOKE SELECT, INSERT, UPDATE, DELETE ON mbway_payments FROM npadmin_app;


-- Revoke usage on the sequences
REVOKE USAGE ON SEQUENCE tenants_id_seq FROM npadmin_app;
REVOKE USAGE ON SEQUENCE account_tenant_memberships_id_seq FROM npadmin_app;
REVOKE USAGE ON SEQUENCE accounts_id_seq FROM npadmin_app;
REVOKE USAGE ON SEQUENCE associate_category_id_seq FROM npadmin_app;
REVOKE USAGE ON SEQUENCE subscriptions_id_seq FROM npadmin_app;
REVOKE USAGE ON SEQUENCE subscriptions_history_id_seq FROM npadmin_app;
REVOKE USAGE ON SEQUENCE orders_id_seq FROM npadmin_app;
REVOKE USAGE ON SEQUENCE multibanco_payments_id_seq FROM npadmin_app;
REVOKE USAGE ON SEQUENCE mbway_payments_id_seq FROM npadmin_app;


-- Revoke permissions on account_status
REVOKE SELECT ON account_status FROM npadmin_app;