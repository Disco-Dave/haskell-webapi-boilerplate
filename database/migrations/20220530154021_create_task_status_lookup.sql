CREATE TABLE public.task_status_lookup (
    task_status_id char PRIMARY KEY,
    name text NOT NULL,
    created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
);

GRANT SELECT ON TABLE public.task_status_lookup TO readonly, readwrite;


INSERT INTO public.task_status_lookup
    (
        task_status_id,
        name,
        created_at,
        updated_at
    )
VALUES
    (
        's', -- task_status_id
        'Started', -- name
        CURRENT_TIMESTAMP, -- created_at
        CURRENT_TIMESTAMP -- updated_at
    ),
    (
        'r', -- task_status_id
        'Removed', -- name
        CURRENT_TIMESTAMP, -- created_at
        CURRENT_TIMESTAMP -- updated_at
    ),
    (
        'f', -- task_status_id
        'Finished', -- name
        CURRENT_TIMESTAMP, -- created_at
        CURRENT_TIMESTAMP -- updated_at
    );
