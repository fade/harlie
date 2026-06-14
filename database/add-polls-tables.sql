-- add-polls-tables.sql
-- Migration: add the polls, poll_options and poll_votes tables.
--
-- Backs the !poll command: timed, multiple-choice channel polls that
-- are defined in a private query window, published to a channel, voted
-- on by channel members (one vote per nick), and auto-closed with a
-- results announcement when their timer expires.
--
-- Polls survive bot restarts because their expiry is stored as a
-- timestamptz; a periodic sweeper closes and announces any poll whose
-- expires_at has passed.
--
-- Safe to run on an already-migrated database: every step is idempotent.
--
-- Run once with:
--   psql -U <bot_user> -d <bot_db> -f database/add-polls-tables.sql
--
-- Author: Glenn Thompson

BEGIN;

CREATE TABLE IF NOT EXISTS public.polls (
    poll_id    serial       PRIMARY KEY,
    channel    text         NOT NULL,
    question   text         NOT NULL,
    creator    text         NOT NULL,
    created_at timestamptz  DEFAULT now(),
    expires_at timestamptz  NOT NULL,
    published  boolean      NOT NULL DEFAULT false,
    closed     boolean      NOT NULL DEFAULT false,
    announced  boolean      NOT NULL DEFAULT false
);

CREATE INDEX IF NOT EXISTS polls_channel_idx
    ON public.polls (channel);

CREATE INDEX IF NOT EXISTS polls_sweep_idx
    ON public.polls (announced, closed, expires_at);

CREATE TABLE IF NOT EXISTS public.poll_options (
    poll_id     integer NOT NULL REFERENCES public.polls(poll_id) ON DELETE CASCADE,
    option_num  integer NOT NULL,
    option_text text    NOT NULL,
    PRIMARY KEY (poll_id, option_num)
);

CREATE TABLE IF NOT EXISTS public.poll_votes (
    poll_id    integer     NOT NULL REFERENCES public.polls(poll_id) ON DELETE CASCADE,
    voter      text        NOT NULL,
    option_num integer     NOT NULL,
    voted_at   timestamptz DEFAULT now(),
    PRIMARY KEY (poll_id, voter)
);

CREATE INDEX IF NOT EXISTS poll_votes_poll_idx
    ON public.poll_votes (poll_id);

-- Transfer ownership to whichever role already owns the rest of the
-- harlie schema, so the bot user can read/write these tables even when
-- the migration is run as postgres.  We pick the owner of public.contexts
-- (guaranteed to exist by this point) rather than the database owner,
-- matching the approach used by add-phrases-tables.sql.
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

    EXECUTE format('ALTER TABLE public.polls OWNER TO %I', schema_owner);
    EXECUTE format('ALTER TABLE public.poll_options OWNER TO %I', schema_owner);
    EXECUTE format('ALTER TABLE public.poll_votes OWNER TO %I', schema_owner);
    EXECUTE format('ALTER SEQUENCE public.polls_poll_id_seq OWNER TO %I', schema_owner);
END
$$;

COMMIT;

DO $$
BEGIN
    RAISE NOTICE '';
    RAISE NOTICE 'Polls migration complete.';
    RAISE NOTICE 'The !poll command is now available.';
END $$;
