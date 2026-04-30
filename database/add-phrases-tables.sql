-- add-phrases-tables.sql
-- Migration: add the phrases and phrase_votes tables.
-- Run once against an existing Harlie database:
--
--   psql -U <bot_user> -d <bot_db> -f database/add-phrases-tables.sql

BEGIN;

CREATE TABLE IF NOT EXISTS public.phrases (
    phrase_id  serial       PRIMARY KEY,
    context_id integer      NOT NULL REFERENCES public.contexts(context_id),
    channel    text         NOT NULL,
    trigger_text text,
    phrase_text  text       NOT NULL,
    created_at timestamptz  DEFAULT now()
);

CREATE INDEX IF NOT EXISTS phrases_context_idx
    ON public.phrases (context_id);

CREATE INDEX IF NOT EXISTS phrases_channel_idx
    ON public.phrases (channel);

CREATE TABLE IF NOT EXISTS public.phrase_votes (
    phrase_id  integer      NOT NULL REFERENCES public.phrases(phrase_id) ON DELETE CASCADE,
    voter      text         NOT NULL,
    voted_at   timestamptz  DEFAULT now(),
    PRIMARY KEY (phrase_id, voter)
);

CREATE INDEX IF NOT EXISTS phrase_votes_phrase_idx
    ON public.phrase_votes (phrase_id);

-- Transfer ownership to the database owner so the bot user can
-- read/write these tables even when the migration is run as postgres.
DO $$
DECLARE
    db_owner text;
BEGIN
    SELECT pg_catalog.pg_get_userbyid(d.datdba) INTO db_owner
      FROM pg_catalog.pg_database d
     WHERE d.datname = current_database();

    EXECUTE format('ALTER TABLE public.phrases OWNER TO %I', db_owner);
    EXECUTE format('ALTER TABLE public.phrase_votes OWNER TO %I', db_owner);
    EXECUTE format('ALTER SEQUENCE public.phrases_phrase_id_seq OWNER TO %I', db_owner);
END
$$;

COMMIT;
