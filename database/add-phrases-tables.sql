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

-- Transfer ownership to whichever role already owns the rest of the
-- harlie schema, so the bot user can read/write these tables even when
-- the migration is run as postgres.  We pick the owner of public.contexts
-- (the FK target above, guaranteed to exist by this point) rather than
-- the database owner, because on installs where the database itself is
-- owned by postgres but the harlie tables are owned by the bot user
-- the datdba lookup picks postgres and the migration cannot be applied
-- as the bot user.
DO $$
DECLARE
    schema_owner text;
BEGIN
    SELECT tableowner INTO schema_owner
      FROM pg_catalog.pg_tables
     WHERE schemaname = 'public'
       AND tablename  = 'contexts';

    IF schema_owner IS NULL THEN
        RAISE EXCEPTION 'cannot determine harlie schema owner: public.contexts not found';
    END IF;

    EXECUTE format('ALTER TABLE public.phrases OWNER TO %I', schema_owner);
    EXECUTE format('ALTER TABLE public.phrase_votes OWNER TO %I', schema_owner);
    EXECUTE format('ALTER SEQUENCE public.phrases_phrase_id_seq OWNER TO %I', schema_owner);
END
$$;

COMMIT;
