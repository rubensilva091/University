-- Esta tabela, tal como a 'accounts', é global.
-- A RLS foi ativada por engano sem uma política,
-- bloqueando todos os SELECTs durante o login.
-- Vamos desativá-la permanentemente.

ALTER TABLE account_password_hashes DISABLE ROW LEVEL SECURITY;
ALTER TABLE account_password_hashes NO FORCE ROW LEVEL SECURITY;