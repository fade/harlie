-- add-quotes-table.sql
--
-- Adds the quotes table for the !quote / !addquote feature.
-- Safe to run on an already-migrated database — all steps are idempotent.
--
-- Run with:
--   psql -U <user> <dbname> -f database/add-quotes-table.sql
--
-- Author: Glenn Thompson

BEGIN;

-- ---------------------------------------------------------------------------
-- Step 1: Create quotes table
-- ---------------------------------------------------------------------------

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.tables
        WHERE table_schema = 'public' AND table_name = 'quotes'
    ) THEN
        CREATE TABLE public.quotes (
            quote_id SERIAL PRIMARY KEY,
            channel  TEXT NOT NULL,
            added_by TEXT NOT NULL,
            quote_text TEXT NOT NULL,
            added_at TIMESTAMP WITH TIME ZONE DEFAULT now()
        );
        RAISE NOTICE 'Step 1: Created quotes table.';
    ELSE
        RAISE NOTICE 'Step 1: quotes table already exists — skipping.';
    END IF;
END $$;

-- ---------------------------------------------------------------------------
-- Step 2: Create index on channel for fast per-channel lookups
-- ---------------------------------------------------------------------------

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_indexes
        WHERE schemaname = 'public'
          AND tablename  = 'quotes'
          AND indexname   = 'idx_quotes_channel'
    ) THEN
        CREATE INDEX idx_quotes_channel ON public.quotes (channel);
        RAISE NOTICE 'Step 2: Created idx_quotes_channel index.';
    ELSE
        RAISE NOTICE 'Step 2: idx_quotes_channel index already exists — skipping.';
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
    EXECUTE 'GRANT ALL ON quotes TO consort';
    EXECUTE 'GRANT USAGE, SELECT ON SEQUENCE quotes_quote_id_seq TO consort';
    RAISE NOTICE 'Step 3: Granted permissions on quotes to consort.';
EXCEPTION WHEN undefined_object THEN
    RAISE NOTICE 'Step 3: Role "consort" not found — adjust the GRANT to your bot user.';
END $$;

COMMIT;

-- ---------------------------------------------------------------------------

DO $$
BEGIN
    RAISE NOTICE '';
    RAISE NOTICE 'Quotes migration complete.';
    RAISE NOTICE 'The bot can now use !addquote, !quote, and !editquote commands.';
END $$;
