# =============================================================================
# 02_migrate_data.R — Trin 2: Importér Parquet-dumps → Supabase/Postgres
# =============================================================================
# Faset migration der adresserer Codex adversarial review:
#   Fase 0  Læs Parquet-dumps + byg type-map fra access_schema.yaml
#   Fase 1  Pre-flight orphan-tjek (PARQUET-baseret — kræver ingen DB)
#   Fase 2  Opret tabeller (01a_create_tables.sql) — FK-fri
#   Fase 3  Load data pr. tabel (rækkefølge-fri, da ingen FK endnu)
#   Fase 4  Påfør FK (01b) — fejler loud hvis orphans; ikke-gennemtvungen
#           kun hvis pre-flight var ren
#   Fase 5  Reset IDENTITY-sequences (ellers PK-kollision på første app-insert)
#   Fase 6  Verifikation: row counts (parquet vs DB) + æ/ø/å-stikprøve
#
# Miljø:
#   MIGRATION_TARGET   supabase | supabase_local   (default: supabase)
#   MIGRATION_DRYRUN   1 → kun Fase 0+1 (ingen DB-forbindelse)
#   SUPABASE_DB_PASSWORD   (fra .Renviron — ALDRIG i kode/config)
#
# Kør:  Rscript 02_migrate_data.R
#       MIGRATION_DRYRUN=1 Rscript 02_migrate_data.R   # pre-flight uden DB
#
# VIGTIGT: Scriptet er IKKE idempotent. CREATE TABLE har ingen IF NOT EXISTS,
# og Fase 4-6 kører efter at Fase 2+3-transaktionen er committet. Første DB-kørsel
# SKAL ramme et tomt target. Fejler en senere fase: DROP de oprettede tabeller
# før re-run (en tom-target-guard nedenfor stopper loud hvis target ej er tomt).
# =============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(arrow)
  library(DBI)
})

source("migration_metadata.R", encoding = "UTF-8")

log_msg <- function(...) message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), paste0(...)))
die     <- function(...) stop(paste0(...), call. = FALSE)

cfg          <- yaml::read_yaml("config.yml")$default
schema_path  <- cfg$paths$schema_yaml
dump_dir     <- cfg$paths$data_dump_dir
tables_sql   <- "01a_create_tables.sql"
fks_sql      <- "01b_foreign_keys.sql"

TARGET  <- Sys.getenv("MIGRATION_TARGET", "supabase")
DRYRUN  <- nzchar(Sys.getenv("MIGRATION_DRYRUN"))

# =============================================================================
# Fase 0 — Læs dumps + byg kolonne-type-map
# =============================================================================
read_dumps <- function() {
  files <- list.files(dump_dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(files) == 0) die("Ingen Parquet-filer i ", dump_dir)
  dumps <- lapply(files, function(f) arrow::read_parquet(f))
  names(dumps) <- sub("\\.parquet$", "", basename(files))
  log_msg("Fase 0: ", length(dumps), " Parquet-dumps læst (",
          sum(vapply(dumps, nrow, integer(1))), " rækker i alt)")
  dumps
}

# Byg map: tabel → (kolonne → Postgres-type) fra access_schema.yaml
build_type_map <- function() {
  schema <- yaml::read_yaml(schema_path)$tabeller
  lapply(schema, function(tbl) {
    setNames(
      vapply(tbl$kolonner, function(k) map_odbc_type(k$type), character(1)),
      vapply(tbl$kolonner, function(k) k$navn, character(1))
    )
  })
}

# Coerce df-kolonner til de typer DDL'en forventer (undgår dbWriteTable-mismatch)
coerce_df <- function(df, coltypes) {
  for (cn in names(df)) {
    pgt <- coltypes[[cn]]
    if (is.null(pgt)) next
    df[[cn]] <- switch(pgt,
      "BOOLEAN"   = as.logical(df[[cn]]),
      "INTEGER"   = suppressWarnings(as.integer(df[[cn]])),
      "NUMERIC"   = suppressWarnings(as.numeric(df[[cn]])),
      "TIMESTAMP" = as.POSIXct(df[[cn]], tz = "UTC"),
      "TEXT"      = as.character(df[[cn]]),
      df[[cn]]
    )
  }
  df
}

# =============================================================================
# Fase 1 — Orphan-tjek på PARQUET (ingen DB nødvendig)
# =============================================================================
# For hver FK: child-kolonnens non-null værdier SKAL findes i parent-PK-sættet.
check_orphans <- function(dumps) {
  log_msg("Fase 1: Pre-flight orphan-tjek (parquet)")
  results <- list()
  for (fk in FK_MAP) {
    cdf <- dumps[[fk$child]]; pdf <- dumps[[fk$parent]]
    if (is.null(cdf) || is.null(pdf)) {
      log_msg("  ! mangler dump for ", fk$child, " eller ", fk$parent); next
    }
    child_vals  <- cdf[[fk$col]]
    parent_vals <- pdf[[fk$pcol]]
    nn <- child_vals[!is.na(child_vals)]
    orphans <- nn[!(nn %in% parent_vals)]
    n_orph  <- length(unique(orphans))
    tag <- if (fk$enforced) "[enforced]" else "[IKKE-enforced]"
    if (n_orph > 0) {
      ex <- paste(utils::head(unique(orphans), 5), collapse = ", ")
      log_msg(sprintf("  %s ORPHANS %s.%s → %s.%s: %d unikke (fx: %s)",
                      tag, fk$child, fk$col, fk$parent, fk$pcol, n_orph, ex))
    } else {
      log_msg(sprintf("  %s OK %s.%s → %s.%s", tag, fk$child, fk$col, fk$parent, fk$pcol))
    }
    results[[paste(fk$child, fk$col, sep=".")]] <- list(fk = fk, n_orphans = n_orph)
  }
  enforced_orphans <- Filter(function(r) isTRUE(r$fk$enforced) && r$n_orphans > 0, results)
  if (length(enforced_orphans) > 0) {
    die(length(enforced_orphans), " gennemtvungne FK'er har orphans — ",
        "data inkonsistent, ret kilde før migration (se log ovenfor)")
  }
  results
}

# =============================================================================
# DB-helpers
# =============================================================================
connect_target <- function() {
  tc <- cfg[[TARGET]]
  if (is.null(tc)) die("Ukendt MIGRATION_TARGET: ", TARGET)
  pw <- if (identical(TARGET, "supabase_local")) {
    Sys.getenv("SUPABASE_DB_PASSWORD", "postgres")
  } else {
    pw <- Sys.getenv("SUPABASE_DB_PASSWORD")
    if (!nzchar(pw)) die("SUPABASE_DB_PASSWORD mangler i .Renviron")
    pw
  }
  log_msg("Forbinder til ", TARGET, " (", tc$host, ":", tc$port, "/", tc$dbname, ")")
  DBI::dbConnect(RPostgres::Postgres(),
    host = tc$host, port = tc$port, dbname = tc$dbname,
    user = tc$user, password = pw, sslmode = tc$sslmode)
}

# Kør .sql-fil: strip kommentarer/blanke, split på statement-terminator ";"
run_sql_file <- function(con, path, label) {
  raw  <- readLines(path, encoding = "UTF-8", warn = FALSE)
  code <- raw[!grepl("^\\s*(--|$)", raw)]                 # drop kommentarer + blanke
  stmts <- strsplit(paste(code, collapse = "\n"), ";", fixed = TRUE)[[1]]
  stmts <- trimws(stmts); stmts <- stmts[nzchar(stmts)]
  log_msg("  ", label, ": ", length(stmts), " statements")
  for (s in stmts) DBI::dbExecute(con, s)
  length(stmts)
}

# =============================================================================
# Fase 3 — Load data
# =============================================================================
load_tables <- function(con, dumps, type_map) {
  log_msg("Fase 3: Load data (FK-fri rækkefølge)")
  for (tname in names(dumps)) {
    df <- dumps[[tname]]
    ct <- type_map[[tname]]
    if (!is.null(ct)) df <- coerce_df(as.data.frame(df), ct)
    DBI::dbWriteTable(con, DBI::Id(table = tname), df, append = TRUE)
    log_msg(sprintf("  %-38s %d rækker", tname, nrow(df)))
  }
}

# =============================================================================
# Fase 5 — Reset IDENTITY-sequences
# =============================================================================
# Eksplicit Id-import advancer IKKE identity-sequencen → første app-insert uden
# id genbruger 1 og kolliderer. setval til max(pk) lukker hullet.
reset_sequences <- function(con) {
  log_msg("Fase 5: Reset IDENTITY-sequences")
  for (tname in names(PK_MAP)) {
    pk <- PK_MAP[[tname]]
    seq <- DBI::dbGetQuery(con, sprintf(
      "SELECT pg_get_serial_sequence('%s', %s) AS s",
      DBI::dbQuoteIdentifier(con, tname),
      DBI::dbQuoteString(con, pk)))$s[1]
    if (is.na(seq)) { log_msg("  ! ingen sequence for ", tname, ".", pk); next }
    mx <- DBI::dbGetQuery(con, sprintf("SELECT COALESCE(MAX(%s),0) AS m FROM %s",
            DBI::dbQuoteIdentifier(con, pk), DBI::dbQuoteIdentifier(con, tname)))$m[1]
    # setval returnerer den satte værdi → ét kald, ingen rå sekvens-identifier i FROM
    nv <- DBI::dbGetQuery(con, sprintf("SELECT setval('%s', %d, true) AS v", seq, max(mx, 1)))$v[1]
    if (nv < mx) die("Sequence ", seq, " (", nv, ") < max(", pk, ")=", mx, " for ", tname)
    log_msg(sprintf("  %-38s seq=%d (max %s=%d)", tname, nv, pk, mx))
  }
}

# =============================================================================
# Fase 6 — Verifikation
# =============================================================================
verify <- function(con, dumps) {
  log_msg("Fase 6: Verifikation")
  mismatch <- 0
  for (tname in names(dumps)) {
    n_db <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s",
              DBI::dbQuoteIdentifier(con, tname)))$n[1]
    n_pq <- nrow(dumps[[tname]])
    ok <- n_db == n_pq
    if (!ok) mismatch <- mismatch + 1
    log_msg(sprintf("  %-38s parquet=%-5d db=%-5d %s", tname, n_pq, n_db,
                    if (ok) "OK" else "MISMATCH"))
  }
  # æ/ø/å-stikprøve
  smp <- DBI::dbGetQuery(con,
    'SELECT efternavn FROM "tblPersoner" WHERE efternavn ~ \'[æøåÆØÅ]\' LIMIT 3')
  if (nrow(smp) > 0) log_msg("  æ/ø/å-stikprøve OK: ", paste(smp$efternavn, collapse=", "))
  if (mismatch > 0) die(mismatch, " tabeller har row-count-mismatch")
}

# =============================================================================
# Orkestrering
# =============================================================================
main <- function() {
  dumps      <- read_dumps()
  type_map   <- build_type_map()
  orphan_res <- check_orphans(dumps)  # Fase 1 — fejler loud ved enforced-orphans

  if (DRYRUN) {
    log_msg("DRYRUN: stopper efter pre-flight (ingen DB-ændringer)")
    return(invisible(NULL))
  }

  if (!requireNamespace("RPostgres", quietly = TRUE)) die("Pakke RPostgres mangler")
  con <- connect_target()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Tom-target-guard: scriptet er ikke idempotent (se header)
  existing <- intersect(names(dumps),
    DBI::dbGetQuery(con, "SELECT tablename FROM pg_tables WHERE schemaname='public'")$tablename)
  if (length(existing) > 0) {
    die("Target ej tomt — ", length(existing), " af vores tabeller findes allerede (fx ",
        paste(utils::head(existing, 3), collapse=", "), "). DROP dem før re-run.")
  }

  DBI::dbWithTransaction(con, {           # Fase 2+3 atomisk
    log_msg("Fase 2: Opret tabeller")
    run_sql_file(con, tables_sql, "01a_create_tables")
    load_tables(con, dumps, type_map)
  })

  log_msg("Fase 4: Påfør foreign keys")
  run_sql_file(con, fks_sql, "01b_foreign_keys")  # kun ukommenterede (enforced)
  # Auto-aktivér ikke-enforced FK'er hvis pre-flight var orphan-fri
  for (r in orphan_res) {
    if (!isTRUE(r$fk$enforced) && r$n_orphans == 0) {
      DBI::dbExecute(con, sub(";$", "", mk_fk_stmt(r$fk)))
      log_msg("  + ikke-enforced FK aktiveret (orphan-fri): ",
              r$fk$child, ".", r$fk$col)
    }
  }

  reset_sequences(con)
  verify(con, dumps)
  log_msg("===== Migration færdig =====")
}

# Auto-kør kun under Rscript (non-interactive). source() i konsol kører IKKE.
if (!interactive()) main()
