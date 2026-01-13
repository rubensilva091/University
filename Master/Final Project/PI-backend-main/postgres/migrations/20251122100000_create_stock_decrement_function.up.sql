-- Esta função permite decrementar o stock de forma segura sem dar permissão de escrita direta ao utilizador.
-- SECURITY DEFINER: Executa com as permissões do dono da função (Admin/Sistema), ignorando o RLS do utilizador atual.

CREATE OR REPLACE FUNCTION decrement_ticket_stock(p_definition_id bigint, p_tenant_id bigint)
RETURNS void
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
DECLARE
  v_rows_affected int;
BEGIN
  -- Tenta decrementar. O filtro tenant_id evita que se alterem bilhetes de outras organizações.
  UPDATE event_ticket_definitions
  SET available_quantity = available_quantity - 1
  WHERE id = p_definition_id 
    AND tenant_id = p_tenant_id 
    AND available_quantity > 0;
  
  GET DIAGNOSTICS v_rows_affected = ROW_COUNT;
  
  -- Se nenhuma linha foi afetada, ou o ID está errado, ou o Tenant está errado, ou o stock é 0.
  IF v_rows_affected = 0 THEN
     RAISE EXCEPTION 'Stock update failed: Ticket not found, wrong tenant, or sold out';
  END IF;
END;
$$;