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

COMMIT;
