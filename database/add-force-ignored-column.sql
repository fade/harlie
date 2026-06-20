-- add-force-ignored-column.sql
-- Migration: add the force_ignored column to public.harlie_user.
--
-- Backs operator force-ignore of misbehaving bots: bots that never emit
-- the join sentinel can be muted directly from the REPL
-- (force-ignore-bot / unforce-ignore-bot).  The flag is per-user and
-- channel-independent; harlie applies the existing per-channel ignore
-- machinery wherever it sees a flagged nick, so the mute survives restarts.
--
-- Safe to run on an already-migrated database: the step is idempotent.
--
-- Run once with:
--   psql -U <bot_user> -d <bot_db> -f database/add-force-ignored-column.sql
--
-- Author: Brian O'Reilly

BEGIN;

ALTER TABLE public.harlie_user
    ADD COLUMN IF NOT EXISTS force_ignored boolean NOT NULL DEFAULT false;

COMMIT;

DO $$
BEGIN
    RAISE NOTICE '';
    RAISE NOTICE 'Force-ignore migration complete.';
    RAISE NOTICE 'Operators can now use force-ignore-bot / unforce-ignore-bot.';
END $$;
