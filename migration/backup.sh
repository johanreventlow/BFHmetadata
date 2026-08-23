#!/usr/bin/env bash
# migration/backup.sh — backup af Supabase-skema public før migration.
# Kodeordet læses fra miljøet (SUPABASE_DB_PASSWORD), aldrig fra kommandolinjen.
# Dumpen lægges UDEN FOR repoet.
set -euo pipefail

: "${SUPABASE_DB_PASSWORD:?SUPABASE_DB_PASSWORD er ikke sat}"

HOST=aws-0-eu-west-1.pooler.supabase.com
PORT=5432
USER=postgres.ijgwlqpbjcfffdmxeahh
DB=postgres
OUT_DIR="${HOME}/backups"
STAMP="$(date +%Y-%m-%d-%H%M)"
DUMP="${OUT_DIR}/BFHmetadata-${STAMP}.dump"
COUNTS="${OUT_DIR}/BFHmetadata-${STAMP}.counts.txt"

mkdir -p "$OUT_DIR"

echo "== Rækketal FØR dump (referencen — ikke et hardcodet snapshot) =="
PGPASSWORD="$SUPABASE_DB_PASSWORD" psql -h "$HOST" -p "$PORT" -U "$USER" -d "$DB" \
  -v ON_ERROR_STOP=1 -At -F',' -o "$COUNTS" <<'SQL'
SELECT 'tblIndikatorer', count(*) FROM "tblIndikatorer"
UNION ALL SELECT 'tblDiagrammer', count(*) FROM "tblDiagrammer"
UNION ALL SELECT 'forbind_faggrupper', count(*) FROM "tblForbindIndikatorerFaggrupper"
UNION ALL SELECT 'forbind_dataprodukter', count(*) FROM "tblForbindIndikatorerDataprodukter"
UNION ALL SELECT 'forbind_organisation', count(*) FROM "tblForbindIndikatorerOrganisation"
ORDER BY 1;
SQL
cat "$COUNTS"

echo "== Dump =="
PGPASSWORD="$SUPABASE_DB_PASSWORD" pg_dump -h "$HOST" -p "$PORT" -U "$USER" -d "$DB" \
  --schema=public --no-owner --no-privileges -Fc -f "$DUMP"

echo "== Katalogtjek (svag verifikation) =="
pg_restore --list "$DUMP" > /dev/null && echo "OK: arkivet kan læses"

ls -lh "$DUMP"
echo
echo "GATE: kør nu prøve-restoren manuelt (se Step 3 i planen), FØR migrationen."
