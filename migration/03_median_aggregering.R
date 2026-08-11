# Migration: aggregering-kolonne på tblDiagrammerMedian + backfill.
#
# HVORFOR: et median-knæk gemmes kun som en DATO. resolve_median_breaks()
# oversætter datoen til en RÆKKEPOSITION i den serie den anvendes på — så
# samme dato betyder forskellige ting ved forskellig aggregering. Målt:
#
#   Knæk-dato 2025-03-17 (mandag):
#     dag    -> fase 2 starter 2025-03-17
#     uge    -> fase 2 starter 2025-03-17
#     maaned -> fase 2 starter 2025-04-01   <- drifter 2 uger, tavst
#
# Uden at vide hvilken aggregering knækket blev SAT under, kan appen hverken
# validere eller advare. Kolonnen gør knækket selvbeskrivende.
#
# BACKFILL er faktuel, ikke et gæt: alle 105 eksisterende knæk ligger allerede
# på deres periodes bucket-grænse (104/104 måneds-knæk på månedsstart,
# 1/1 uge-knæk på mandag) — de er registreret mod korrekt aggregerede serier.
#
# Idempotent: kan køres flere gange. ADD COLUMN IF NOT EXISTS + backfill kun
# hvor aggregering IS NULL.
#
# KØR MOD KOPI/BRANCH FØRST — dette rører den delte produktions-DB som BFHddl
# også læser. Kolonnen er nullable og additiv, så BFHddl påvirkes ikke.
#
# Brug:
#   Rscript migration/03_median_aggregering.R           # dry-run (viser plan)
#   Rscript migration/03_median_aggregering.R --apply   # udfør

args <- commandArgs(trailingOnly = TRUE)
apply_changes <- "--apply" %in% args

readRenviron(".Renviron")
cfg <- yaml::read_yaml("config.yml")$default$supabase
pw <- Sys.getenv("SUPABASE_DB_PASSWORD")
if (!nzchar(pw)) stop("SUPABASE_DB_PASSWORD mangler i .Renviron", call. = FALSE)

con <- DBI::dbConnect(RPostgres::Postgres(), host = cfg$host, port = cfg$port,
  dbname = cfg$dbname, user = cfg$user, password = pw, sslmode = cfg$sslmode)
on.exit(DBI::dbDisconnect(con), add = TRUE)

cat("Target:", cfg$host, "/", cfg$dbname, "\n")
cat("Mode:  ", if (apply_changes) "APPLY" else "DRY-RUN", "\n\n")

# --- 1. Kolonne ------------------------------------------------------------
has_col <- DBI::dbGetQuery(con, "
  SELECT count(*) AS n FROM information_schema.columns
  WHERE table_name = 'tblDiagrammerMedian' AND column_name = 'aggregering'")$n[1]

if (has_col == 0) {
  cat("[1] Kolonnen 'aggregering' mangler -> tilføjes (text NULL)\n")
  if (apply_changes) {
    DBI::dbExecute(con,
      'ALTER TABLE "tblDiagrammerMedian" ADD COLUMN IF NOT EXISTS "aggregering" text')
    cat("    OK\n")
  }
} else {
  cat("[1] Kolonnen 'aggregering' findes allerede - springes over\n")
}

# --- 2. Backfill -----------------------------------------------------------
# Sæt = diagrammets nuværende periode_aggregering, kun hvor værdien mangler.
if (has_col == 0 && !apply_changes) {
  cat("[2] Backfill kan først opgøres når kolonnen findes (dry-run)\n")
} else {
  todo <- DBI::dbGetQuery(con, '
    SELECT COALESCE(NULLIF(TRIM(d."periode_aggregering"), \'\'), \'(tom)\') AS periode,
           count(*) AS n
    FROM "tblDiagrammerMedian" m
    JOIN "tblDiagrammer" d ON d."id" = m."diagram"
    WHERE m."aggregering" IS NULL
    GROUP BY 1 ORDER BY 2 DESC')
  if (nrow(todo) == 0) {
    cat("[2] Ingen knæk mangler aggregering - backfill ikke nødvendig\n")
  } else {
    # as.numeric: RPostgres returnerer count(*) som int64, der ellers printes
    # som vrøvl (5.19e-322) når man summerer det direkte.
    cat("[2] Backfill af", sum(as.numeric(todo$n)), "knæk:\n")
    print(todo, row.names = FALSE)
    if (apply_changes) {
      n <- DBI::dbExecute(con, '
        UPDATE "tblDiagrammerMedian" m
        SET "aggregering" = NULLIF(TRIM(d."periode_aggregering"), \'\')
        FROM "tblDiagrammer" d
        WHERE d."id" = m."diagram" AND m."aggregering" IS NULL')
      cat("    OK -", n, "rækker opdateret\n")
    }
  }
}

# --- 3. Verifikation -------------------------------------------------------
if (apply_changes) {
  cat("\n[3] Slutstatus:\n")
  res <- DBI::dbGetQuery(con, '
    SELECT COALESCE("aggregering", \'(NULL)\') AS aggregering, count(*) AS n
    FROM "tblDiagrammerMedian" GROUP BY 1 ORDER BY 2 DESC')
  print(res, row.names = FALSE)

  # Sanity: stemmer knækkenes aggregering med diagrammets periode?
  mism <- DBI::dbGetQuery(con, '
    SELECT count(*) AS n FROM "tblDiagrammerMedian" m
    JOIN "tblDiagrammer" d ON d."id" = m."diagram"
    WHERE m."aggregering" IS NOT NULL
      AND m."aggregering" IS DISTINCT FROM NULLIF(TRIM(d."periode_aggregering"), \'\')')$n[1]
  cat("\nMismatch mod diagrammets nuværende periode:", mism,
      if (mism == 0) "(forventet lige efter backfill)\n" else "(!)\n")
} else {
  cat("\nDRY-RUN - intet ændret. Kør med --apply for at udføre.\n")
}
