#!/usr/bin/env bash
# ==============================================================================
# 99_pak_zip.sh — Pak migration-arbejdsmappen til transport til Mac
# ==============================================================================
# Kør fra ~/bfh_db_migration/ (eller en parent-mappe).
# Output: bfh_db_migration_YYYY-MM-DD.zip i parent-mappen.
#
# Udelukker:
#  - .Renviron (secrets — Mac sætter sin egen)
#  - renv/library/ (R-pakke-cache — bygges på Mac)
#  - .Rhistory, *.tmp, .DS_Store
#
# Kør: bash 99_pak_zip.sh
# Eller via git-bash på Windows.
# ==============================================================================

set -euo pipefail

ARBEJDSMAPPE_NAVN="bfh_db_migration"
DATO=$(date +%Y-%m-%d)
ZIP_NAVN="${ARBEJDSMAPPE_NAVN}_${DATO}.zip"

# Find arbejdsmappen (vi kan være i den, eller i parent)
if [ -d "$ARBEJDSMAPPE_NAVN" ]; then
  PARENT_DIR="$(pwd)"
  cd "$ARBEJDSMAPPE_NAVN"
elif [ "$(basename "$(pwd)")" = "$ARBEJDSMAPPE_NAVN" ]; then
  PARENT_DIR="$(dirname "$(pwd)")"
else
  echo "FEJL: Kør fra ~/$ARBEJDSMAPPE_NAVN/ eller dens parent" >&2
  exit 1
fi

ARBEJDSMAPPE_FULDT="$(pwd)"
ZIP_FULDT="${PARENT_DIR}/${ZIP_NAVN}"

echo "Arbejdsmappe:  $ARBEJDSMAPPE_FULDT"
echo "Output ZIP:    $ZIP_FULDT"

# Tjek at obligatoriske artefakter findes
PAAKRAEVEDE=(
  "access_schema.yaml"
  "access_data_dump"
)
MANGLER=()
for f in "${PAAKRAEVEDE[@]}"; do
  if [ ! -e "$f" ]; then
    MANGLER+=("$f")
  fi
done

if [ ${#MANGLER[@]} -gt 0 ]; then
  echo ""
  echo "ADVARSEL: følgende forventede output mangler:"
  printf '  - %s\n' "${MANGLER[@]}"
  echo ""
  echo "Har du kørt 00_introspect_access.R? (Ctrl-C for at afbryde, Enter for at fortsætte alligevel)"
  read -r
fi

# Slet evt. eksisterende ZIP
if [ -f "$ZIP_FULDT" ]; then
  rm "$ZIP_FULDT"
fi

# Pak hele arbejdsmappen, ekskluder secrets + caches
cd "$PARENT_DIR"
zip -r "$ZIP_NAVN" "$ARBEJDSMAPPE_NAVN" \
  -x "${ARBEJDSMAPPE_NAVN}/.Renviron" \
  -x "${ARBEJDSMAPPE_NAVN}/renv/library/*" \
  -x "${ARBEJDSMAPPE_NAVN}/.Rhistory" \
  -x "${ARBEJDSMAPPE_NAVN}/.DS_Store" \
  -x "${ARBEJDSMAPPE_NAVN}/*.tmp"

echo ""
echo "===== Færdig ====="
ls -lh "$ZIP_FULDT"
echo ""
echo "Næste skridt:"
echo "  1. Upload $ZIP_FULDT til OneDrive (eller USB-drev)"
echo "  2. Download på Mac → unpack til ~/$ARBEJDSMAPPE_NAVN/"
echo "  3. På Mac: cp .Renviron.example .Renviron + udfyld Supabase credentials"
echo "  4. På Mac: kør Trin 1 (DDL-generering) og videre"
