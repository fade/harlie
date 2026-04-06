-- add-pending-memos-table.sql
--
-- Adds the pending_memos table for persistent !tell / !memos.
-- Memos survive bot restarts and are deleted once delivered.
-- Safe to run on an already-migrated database — all steps are idempotent.
--
-- Run with:
--   psql -U <user> <dbname> -f database/add-pending-memos-table.sql
--
-- Author: Glenn Thompson

BEGIN;

-- ---------------------------------------------------------------------------
-- Step 1: Create pending_memos table
-- ---------------------------------------------------------------------------

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = 'pending_memos'
    ) THEN
        CREATE TABLE public.pending_memos (
            memo_id    SERIAL PRIMARY KEY,
            sender     TEXT NOT NULL,
            recipient  TEXT NOT NULL,
            channel    TEXT NOT NULL,
            message    TEXT NOT NULL,
            created_at BIGINT NOT NULL
        );
        RAISE NOTICE 'Step 1: Created pending_memos table.';
    ELSE
        RAISE NOTICE 'Step 1: pending_memos table already exists — skipping.';
    END IF;
END $$;

-- ---------------------------------------------------------------------------
-- Step 2: Create index on recipient for fast lookups (case-insensitive)
-- ---------------------------------------------------------------------------

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_indexes
        WHERE schemaname = 'public'
          AND tablename  = 'pending_memos'
          AND indexname   = 'idx_pending_memos_recipient'
    ) THEN
        CREATE INDEX idx_pending_memos_recipient
            ON public.pending_memos (lower(recipient));
        RAISE NOTICE 'Step 2: Created idx_pending_memos_recipient index.';
    ELSE
        RAISE NOTICE 'Step 2: idx_pending_memos_recipient index already exists — skipping.';
    END IF;
END $$;

-- ---------------------------------------------------------------------------
-- Step 3: Grant permissions to the bot user
--
-- The bot connects as its own DB role, not as postgres.
-- Adjust 'consort' below to match your bot's database user.
-- ---------------------------------------------------------------------------

DO $$
BEGIN
    EXECUTE 'GRANT ALL ON pending_memos TO consort';
    EXECUTE 'GRANT USAGE, SELECT ON SEQUENCE pending_memos_memo_id_seq TO consort';
    RAISE NOTICE 'Step 3: Granted permissions on pending_memos to consort.';
EXCEPTION WHEN undefined_object THEN
    RAISE NOTICE 'Step 3: Role "consort" not found — adjust the GRANT to your bot user.';
END $$;

COMMIT;

-- ---------------------------------------------------------------------------

DO $$
BEGIN
    RAISE NOTICE '';
    RAISE NOTICE 'Pending memos migration complete.';
    RAISE NOTICE 'The !tell system is now persistent across bot restarts.';
END $$;
