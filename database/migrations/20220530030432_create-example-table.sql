CREATE TABLE public.examples (
    example_id bigserial PRIMARY KEY,
    note text NOT NULL,
    created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
);

GRANT SELECT ON TABLE public.examples TO readonly;

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE public.examples TO readwrite;

GRANT USAGE ON SEQUENCE public.examples_example_id_seq TO readwrite;

