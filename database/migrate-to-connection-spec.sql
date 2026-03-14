-- migrate-to-connection-spec.sql
--
-- Prepares a legacy Harlie database for the new connection-spec config system.
-- Safe to run on an already-migrated database — all steps are idempotent.
--
-- Run with:
--   psql -U <user> <dbname> -f database/migrate-to-connection-spec.sql
--
-- Author: Brian O'Reilly

BEGIN;

-- ---------------------------------------------------------------------------
-- Step 1: Fix Bug A — irc_server stored as a tuple string
--
-- The old load-contexts used (car server-spec) instead of (caar server-spec),
-- producing values like "(irc.libera.chat :ssl)" in the irc_server column.
-- This extracts the hostname from any such values.
-- ---------------------------------------------------------------------------

DO $$
DECLARE
    bad_count INTEGER;
BEGIN
    SELECT count(*) INTO bad_count
    FROM contexts
    WHERE irc_server ~ E'^\\(';

    IF bad_count > 0 THEN
        RAISE NOTICE 'Step 1: Found % row(s) with tuple-formatted irc_server — fixing.', bad_count;

        UPDATE contexts
        SET irc_server = regexp_replace(irc_server, E'^\\(([^ ]+).*\\)$', '\1')
        WHERE irc_server ~ E'^\\(';

        RAISE NOTICE 'Step 1: Fixed. Affected rows updated.';
    ELSE
        RAISE NOTICE 'Step 1: No tuple-formatted irc_server values found — skipping.';
    END IF;
END $$;


-- ---------------------------------------------------------------------------
-- Step 2: Deduplicate contexts rows
--
-- For each group sharing (context_name, irc_server, irc_channel), keep the
-- row with the lowest context_id (oldest, most likely to have FK children).
-- Reassign urls.context_id and words.context_id before deleting duplicates.
-- ---------------------------------------------------------------------------

DO $$
DECLARE
    dup         RECORD;
    survivor_id BIGINT;
    dup_ids     BIGINT[];
    total_dupes INTEGER := 0;
BEGIN
    FOR dup IN
        SELECT context_name, irc_server, irc_channel, count(*) AS cnt
        FROM contexts
        GROUP BY context_name, irc_server, irc_channel
        HAVING count(*) > 1
    LOOP
        SELECT min(context_id) INTO survivor_id
        FROM contexts
        WHERE context_name = dup.context_name
          AND irc_server   = dup.irc_server
          AND irc_channel  = dup.irc_channel;

        SELECT array_agg(context_id) INTO dup_ids
        FROM contexts
        WHERE context_name = dup.context_name
          AND irc_server   = dup.irc_server
          AND irc_channel  = dup.irc_channel
          AND context_id  <> survivor_id;

        -- Reassign urls referencing any duplicate to the survivor
        UPDATE urls
        SET context_id = survivor_id
        WHERE context_id = ANY(dup_ids);

        -- Reassign words referencing any duplicate to the survivor
        UPDATE words
        SET context_id = survivor_id
        WHERE context_id = ANY(dup_ids);

        DELETE FROM contexts
        WHERE context_id = ANY(dup_ids);

        total_dupes := total_dupes + array_length(dup_ids, 1);

        RAISE NOTICE 'Step 2: Deduplicated (%, %, %) — kept context_id %, removed %.',
            dup.context_name, dup.irc_server, dup.irc_channel,
            survivor_id, dup_ids;
    END LOOP;

    IF total_dupes = 0 THEN
        RAISE NOTICE 'Step 2: No duplicate contexts rows found — skipping.';
    ELSE
        RAISE NOTICE 'Step 2: Removed % duplicate row(s) total.', total_dupes;
    END IF;
END $$;


-- ---------------------------------------------------------------------------
-- Step 3: Add unique constraint for idempotent upserts
--
-- Required by the ON CONFLICT clause in load-contexts (init-first-run.lisp).
-- No-op if the constraint already exists.
-- ---------------------------------------------------------------------------

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'contexts_identity_unique'
          AND conrelid = 'public.contexts'::regclass
    ) THEN
        ALTER TABLE public.contexts
            ADD CONSTRAINT contexts_identity_unique
            UNIQUE (context_name, irc_server, irc_channel);
        RAISE NOTICE 'Step 3: Added contexts_identity_unique constraint.';
    ELSE
        RAISE NOTICE 'Step 3: contexts_identity_unique already present — skipping.';
    END IF;
END $$;


COMMIT;

-- ---------------------------------------------------------------------------

DO $$
BEGIN
    RAISE NOTICE '';
    RAISE NOTICE 'Migration complete.';
    RAISE NOTICE 'Next: run the Lisp migration (run-lisp-migration) to';
    RAISE NOTICE 'produce a new config.lisp, then restart the bot.';
END $$;
