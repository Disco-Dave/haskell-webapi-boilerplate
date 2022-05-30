create function pg_temp.create_user_if_not_exists (_password text)
    returns void
    as
    $$
begin
    if exists (
        select
        from
            pg_catalog.pg_roles
        where
            rolname = 'webserver') then
    raise notice 'Role "webserver" already exists. Skipping.';
else
    begin
        -- nested block
        execute format('create role webserver login password %L;' , _password);
    exception
        when duplicate_object then
            raise notice 'Role "webserver" was just created by a concurrent transaction. Skipping.';
    end;
end if;
end $$
language plpgsql;

select
    pg_temp.create_user_if_not_exists (:'password');
