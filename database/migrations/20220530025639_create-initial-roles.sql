CREATE ROLE readonly;

CREATE ROLE readwrite;

REVOKE CREATE ON SCHEMA public FROM PUBLIC;

DO $$
BEGIN
    EXECUTE format('REVOKE ALL ON DATABASE %I FROM PUBLIC;', current_database());
    EXECUTE format('GRANT CONNECT ON DATABASE %I TO readonly;', current_database());
    EXECUTE format('GRANT CONNECT ON DATABASE %I TO readwrite;', current_database());
END;
$$;

