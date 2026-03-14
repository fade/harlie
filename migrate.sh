#!/usr/bin/env bash
# migrate.sh — Harlie database migration to connection-spec config system
#
# Backs up the Harlie PostgreSQL database, then runs
# database/migrate-to-connection-spec.sql to:
#   - Fix irc_server tuple-string values (Bug A)
#   - Deduplicate contexts rows
#   - Add the unique constraint required by the new load-contexts upsert
#
# Usage:
#   bash migrate.sh [--db DBNAME] [--user PGUSER] [--dry-run]
#
# Defaults:
#   --db    botdb
#   --user  (current Unix user)
#
# Author: Brian O'Reilly

set -euo pipefail

# ---------------------------------------------------------------------------
# Defaults
# ---------------------------------------------------------------------------
DB="botdb"
PGUSER="$(id -un)"
DRY_RUN=false
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SQL_FILE="${SCRIPT_DIR}/database/migrate-to-connection-spec.sql"

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case "$1" in
        --db)    DB="$2";     shift 2 ;;
        --user)  PGUSER="$2"; shift 2 ;;
        --dry-run) DRY_RUN=true; shift ;;
        -h|--help)
            sed -n '2,20p' "$0" | grep '^#' | sed 's/^# \?//'
            exit 0
            ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
info()  { echo "[migrate.sh] $*"; }
warn()  { echo "[migrate.sh] WARNING: $*" >&2; }
die()   { echo "[migrate.sh] ERROR: $*" >&2; exit 1; }

# ---------------------------------------------------------------------------
# Step 1: Check for a running Harlie process
# ---------------------------------------------------------------------------
info "Checking for running Harlie processes..."

HARLIE_PIDS="$(pgrep -f 'sbcl.*harlie' 2>/dev/null || true)"

if [[ -n "$HARLIE_PIDS" ]]; then
    warn "A Harlie process appears to be running (PIDs: ${HARLIE_PIDS})."
    warn "Migrating a live database is safe (all SQL is transactional),"
    warn "but the bot will need to be restarted to pick up the new config."
    echo ""
    read -r -p "[migrate.sh] Continue anyway? [y/N] " response
    case "$response" in
        [yY][eE][sS]|[yY]) : ;;
        *) info "Aborted."; exit 0 ;;
    esac
fi

# ---------------------------------------------------------------------------
# Step 2: Verify the SQL file exists
# ---------------------------------------------------------------------------
[[ -f "$SQL_FILE" ]] || die "SQL migration file not found: ${SQL_FILE}"

# ---------------------------------------------------------------------------
# Step 3: Verify psql and pg_dump are available
# ---------------------------------------------------------------------------
command -v psql    >/dev/null 2>&1 || die "psql not found in PATH"
command -v pg_dump >/dev/null 2>&1 || die "pg_dump not found in PATH"

# ---------------------------------------------------------------------------
# Step 4: Verify we can connect to the database
# ---------------------------------------------------------------------------
info "Testing database connection (db=${DB} user=${PGUSER})..."
psql -U "${PGUSER}" "${DB}" -c "SELECT 1;" >/dev/null 2>&1 \
    || die "Cannot connect to database '${DB}' as user '${PGUSER}'."

# ---------------------------------------------------------------------------
# Step 5: Backup
# ---------------------------------------------------------------------------
BACKUP_FILE="${SCRIPT_DIR}/botdb-pre-migration-$(date +%Y%m%d%H%M%S).sql"

if [[ "$DRY_RUN" == "true" ]]; then
    info "[DRY RUN] Would write backup to: ${BACKUP_FILE}"
else
    info "Backing up database to ${BACKUP_FILE}..."
    pg_dump -U "${PGUSER}" "${DB}" > "${BACKUP_FILE}" \
        || die "pg_dump failed — aborting before any schema changes."
    info "Backup written: ${BACKUP_FILE}"
fi

# ---------------------------------------------------------------------------
# Step 6: Run the SQL migration
# ---------------------------------------------------------------------------
if [[ "$DRY_RUN" == "true" ]]; then
    info "[DRY RUN] Would run: psql -U ${PGUSER} ${DB} -f ${SQL_FILE}"
else
    info "Running SQL migration..."
    psql -U "${PGUSER}" "${DB}" \
         --set ON_ERROR_STOP=1 \
         -f "${SQL_FILE}" \
        || die "SQL migration failed. Database is unchanged (transaction was rolled back). Backup is at: ${BACKUP_FILE}"
    info "SQL migration completed successfully."
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
echo ""
info "Database migration done."
echo ""
echo "  Backup:  ${BACKUP_FILE}"
echo "  Applied: ${SQL_FILE}"
echo ""
echo "Next steps:"
echo "  1. If you have not yet produced a new config.lisp, run the Lisp migration:"
echo "       sbcl --load migrate-from-legacy.lisp \\"
echo "            --load config.lisp \\"
echo "            --eval '(harlie::run-lisp-migration)' \\"
echo "            --quit"
echo "     Review new-config.lisp and copy it into place as config.lisp."
echo ""
echo "  2. Update to the new code (branch: unfuck_configuration_and_state)."
echo ""
echo "  3. Restart the bot.  load-contexts will sync the contexts table"
echo "     from config on every startup — no further manual SQL needed."
