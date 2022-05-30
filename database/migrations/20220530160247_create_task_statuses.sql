CREATE TABLE public.task_statuses (
    task_id bigint PRIMARY KEY REFERENCES public.tasks (task_id),
    task_status_id char NOT NULL REFERENCES public.task_status_lookup (task_status_id),
    created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
);

GRANT SELECT ON TABLE public.task_statuses TO readonly;

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE public.task_statuses TO readwrite;

