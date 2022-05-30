DO $$
BEGIN
    IF NOT EXISTS(SELECT 1 FROM pg_roles WHERE rolname = 'readonly') THEN
        CREATE ROLE readonly;
    END IF;

    IF NOT EXISTS(SELECT 1 FROM pg_roles WHERE rolname = 'readwrite') THEN
        CREATE ROLE readwrite;
    END IF;

    REVOKE CREATE ON SCHEMA public FROM PUBLIC;
    EXECUTE format('REVOKE ALL ON DATABASE %I FROM PUBLIC;', current_database());
    EXECUTE format('GRANT CONNECT ON DATABASE %I TO readonly;', current_database());
    EXECUTE format('GRANT CONNECT ON DATABASE %I TO readwrite;', current_database());
END;
$$;

