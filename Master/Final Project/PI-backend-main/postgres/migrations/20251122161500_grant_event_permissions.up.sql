-- Dar permissões de leitura e escrita nas novas tabelas de eventos
GRANT SELECT, INSERT, UPDATE, DELETE ON events TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON event_ticket_definitions TO npadmin_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON purchased_tickets TO npadmin_app;

-- Dar permissões de uso nas sequências (para criar IDs novos)
GRANT USAGE ON SEQUENCE events_id_seq TO npadmin_app;
GRANT USAGE ON SEQUENCE event_ticket_definitions_id_seq TO npadmin_app;
GRANT USAGE ON SEQUENCE purchased_tickets_id_seq TO npadmin_app;