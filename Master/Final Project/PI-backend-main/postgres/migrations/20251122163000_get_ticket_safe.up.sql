-- Functions to search for tickets globally (bypassing RLS) to process callbacks.

-- 1. Get by Multibanco Reference
CREATE OR REPLACE FUNCTION get_ticket_by_reference_safe(p_reference text)
RETURNS SETOF purchased_tickets
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
BEGIN
  RETURN QUERY 
  SELECT * FROM purchased_tickets 
  WHERE payment_reference = p_reference 
  AND status = 'pending';
END;
$$;

-- 2. Get by MBWay Request ID
CREATE OR REPLACE FUNCTION get_ticket_by_request_id_safe(p_request_id text)
RETURNS SETOF purchased_tickets
LANGUAGE plpgsql
SECURITY DEFINER
AS $$
BEGIN
  RETURN QUERY 
  SELECT * FROM purchased_tickets 
  WHERE payment_request_id = p_request_id 
  AND status = 'pending';
END;
$$;