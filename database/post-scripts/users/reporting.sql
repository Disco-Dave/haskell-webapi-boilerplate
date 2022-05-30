CREATE FUNCTION pg_temp.create_user_if_not_exists (_password text)
    RETURNS void
    AS $$
BEGIN
    IF EXISTS (
        SELECT
        FROM
            pg_catalog.pg_roles
        WHERE
            rolname = 'reporting') THEN
    RAISE NOTICE 'Role "reporting" already exists. Skipping.';
ELSE
    BEGIN
        -- nested block
        EXECUTE format('create role reporting login password %L;', _password);
    EXCEPTION
        WHEN duplicate_object THEN
            RAISE NOTICE 'Role "reporting" was just created by a concurrent transaction. Skipping.';
    END;
END IF;
END
$$
LANGUAGE plpgsql;

SELECT
    pg_temp.create_user_if_not_exists (:'password');

GRANT readonly TO reporting;
