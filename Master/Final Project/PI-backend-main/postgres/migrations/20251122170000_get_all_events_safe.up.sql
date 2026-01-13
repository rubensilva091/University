-- Função para listar TODOS os eventos de TODOS os tenants (ignorando RLS).
-- Útil para a Search Bar global ou Homepage pública.

CREATE OR REPLACE FUNCTION get_all_events_safe()
RETURNS SETOF events
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
BEGIN
  RETURN QUERY 
  SELECT * FROM events 
  WHERE event_date >= now() -- Apenas eventos futuros
  ORDER BY event_date ASC;
END;
$$;

-- Dar permissão ao utilizador da aplicação para usar esta função
GRANT EXECUTE ON FUNCTION get_all_events_safe() TO npadmin_app;