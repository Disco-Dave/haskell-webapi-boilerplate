CREATE TABLE public.tasks (
    task_id bigserial PRIMARY KEY,
    description text NOT NULL,
    created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
);

GRANT SELECT ON TABLE public.tasks TO readonly;

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE public.tasks TO readwrite;

GRANT USAGE ON SEQUENCE public.tasks_task_id_seq TO readwrite;

