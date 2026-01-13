-- Esta função permite criar categorias de forma segura, ignorando o bloqueio de RLS.
-- Útil para o registo automático de novas categorias (ex: "jovem").

CREATE OR REPLACE FUNCTION create_associate_category_safe(p_name text, p_tenant_id bigint)
RETURNS bigint
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
DECLARE
  v_id bigint;
BEGIN
  -- Tenta inserir. Se já existir (race condition), atualiza o timestamp e retorna o ID.
  INSERT INTO associate_category (name, description, tenant_id, created_at, updated_at)
  VALUES (p_name, p_name, p_tenant_id, NOW(), NOW())
  ON CONFLICT (name, tenant_id) DO UPDATE SET updated_at = NOW()
  RETURNING id INTO v_id;

  RETURN v_id;
END;
$$;