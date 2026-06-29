# ==============================================================================
# 00_introspect_access.R — Skema-introspection (KUN PÅ WINDOWS)
# ==============================================================================
# Trækker komplet skema-info + data-dumps fra Access-databasen via ODBC.
# Output er portabelt (YAML + Parquet) og bruges senere på Mac.
#
# Kør: source("00_introspect_access.R", encoding = "UTF-8")
#
# Forudsætninger:
# - Windows med MS Access ODBC-driver
# - ODBC DSN "dataportal_2024" konfigureret
# - R-pakker: DBI, odbc, yaml, arrow, dplyr, tibble
#
# Output:
#   access_schema.yaml       — Komplet skema-metadata
#   access_data_dump/<tbl>.parquet  — Data pr. tabel
#   introspection_log.txt    — Log med advarsler/fejl
#
# ARKITEKTUR: Vi bruger HARDCODED tabel-liste + FRESH-CONNECTION-PR-TABEL
# fordi Access ODBC-driveren har en kendt bug hvor connection-handle'en
# bliver "external pointer is not valid" efter første fejl. Hver tabel
# behandles med sin egen connection så ét fejl ikke vælter resten.
# ==============================================================================

suppressMessages({
  library(DBI)
  library(odbc)
  library(yaml)
  library(arrow)
  library(tibble)
  library(dplyr)
})

DSN_NAVN <- "dataportal_2024"
ARBEJDSMAPPE <- normalizePath(".", winslash = "/")
DATA_DUMP_DIR <- file.path(ARBEJDSMAPPE, "access_data_dump")
SCHEMA_FIL <- file.path(ARBEJDSMAPPE, "access_schema.yaml")
LOG_FIL <- file.path(ARBEJDSMAPPE, "introspection_log.txt")

# Kendte bruger-tabeller (fra tidligere kode-udforskning af bfh_dataportal
# + BFHddl). Hvis Access har flere tabeller end disse, tilføj dem her.
# Hvis nogle af disse ikke eksisterer i din DB, springer scriptet dem over
# uden at fejle.
KENDTE_TABELLER <- c(
  # Indikator-kerne + hierarki
  "tblIndikatorer",
  "tblIndikatorHierarki",
  "tblIndikatorNiveauer",
  # Faggrupper + datakilder
  "tblFaggrupper",
  "tblDatakilder",
  # Personer
  "tblPersoner",
  # Organisation
  "tblOrganisationStruktur",
  "tblOrganisationOversaettelse",
  "tblOrganisationNiveauer",
  # Forbindelses-tabeller (many-to-many)
  "tblForbindIndikatorerFaggrupper",
  "tblForbindIndikatorerOrganisation",
  "tblForbindIndikatorerDataprodukter",
  # Diagram-config
  "tblDiagrammer",
  "tblDiagramTyper",
  "tblDiagramIndstillinger",
  "tblDiagrammerMaal",
  "tblDiagrammerMedian",
  "tblDiagrammerKommentar",
  # Dataprodukter
  "tblDataprodukter"
)

# ------------------------------------------------------------------------------
# Logger
# ------------------------------------------------------------------------------
log_skriv <- function(msg, level = "INFO") {
  linje <- paste0("[", format(Sys.time(), "%H:%M:%S"), " ", level, "] ", msg)
  cat(linje, "\n")
  cat(linje, "\n", file = LOG_FIL, append = TRUE)
}

# ------------------------------------------------------------------------------
# Fresh-connection-helper — åbner en NY connection hver gang.
# Workaround for Access ODBC "external pointer"-bug.
# ------------------------------------------------------------------------------
fresh_con <- function() {
  dbConnect(odbc::odbc(), dsn = DSN_NAVN, encoding = "UTF8")
}

# Kør en operation med fresh connection, luk altid bagefter,
# returnér NULL ved fejl (i stedet for at vælte hele scriptet).
med_fresh_con <- function(operation, fejl_besked = NULL) {
  con <- tryCatch(fresh_con(), error = function(e) {
    log_skriv(paste0("  Connection fejlede: ", e$message), "ERROR")
    NULL
  })
  if (is.null(con)) return(NULL)

  resultat <- tryCatch(
    operation(con),
    error = function(e) {
      if (!is.null(fejl_besked)) {
        log_skriv(paste0("  ", fejl_besked, ": ", e$message), "WARN")
      }
      NULL
    }
  )
  try(dbDisconnect(con), silent = TRUE)
  resultat
}

# ------------------------------------------------------------------------------
# Start
# ------------------------------------------------------------------------------
if (file.exists(LOG_FIL)) file.remove(LOG_FIL)
dir.create(DATA_DUMP_DIR, showWarnings = FALSE, recursive = TRUE)

log_skriv("===== Access-introspection start =====")
log_skriv(paste0("Arbejdsmappe: ", ARBEJDSMAPPE))
log_skriv(paste0("DSN: ", DSN_NAVN))

# ------------------------------------------------------------------------------
# Step 1: Verificér forbindelse + tjek hvilke kendte tabeller der findes
# ------------------------------------------------------------------------------
log_skriv("Step 1: Verificér forbindelse + filtrér eksisterende tabeller ...")

# Tjek hver kendt tabel via en fresh connection + simple COUNT(*)-query.
# Hvis tabellen findes og kan læses → behold den. Ellers spring over.
faktiske_tabeller <- character(0)
row_counts <- list()

for (tbl in KENDTE_TABELLER) {
  rc <- med_fresh_con(
    operation = function(con) {
      dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM [", tbl, "]"))$n
    },
    fejl_besked = paste0("Tabel '", tbl, "' utilgængelig")
  )
  if (!is.null(rc)) {
    faktiske_tabeller <- c(faktiske_tabeller, tbl)
    row_counts[[tbl]] <- as.integer(rc)
    log_skriv(paste0("  ", tbl, ": ", rc, " rækker"))
  }
}

log_skriv(paste0("Fundet ", length(faktiske_tabeller), " af ",
                  length(KENDTE_TABELLER), " kendte tabeller"))

if (length(faktiske_tabeller) == 0) {
  stop("Ingen tabeller kunne læses. Tjek DSN '", DSN_NAVN,
       "' og at databasen er tilgængelig.")
}

# ------------------------------------------------------------------------------
# Step 2: For hver tabel — kolonner via dbColumnInfo() + sample (TOP 5)
# ------------------------------------------------------------------------------
log_skriv("Step 2: Henter kolonne-metadata + samples ...")

# Konverter kolonne-metadata til en simpel liste til YAML
kolonner_til_liste <- function(cols_df) {
  if (is.null(cols_df) || nrow(cols_df) == 0) return(list())
  lapply(seq_len(nrow(cols_df)), function(i) {
    row <- cols_df[i, ]
    list(
      navn = as.character(row$name),
      type = as.character(row$type),
      r_class = as.character(row[[".oclass"]] %||% NA)
    )
  })
}

tabel_info <- list()

for (tbl in faktiske_tabeller) {
  log_skriv(paste0("  Behandler: ", tbl))

  # Kolonne-info via dbColumnInfo på en limit 0-query
  cols <- med_fresh_con(
    operation = function(con) {
      res <- dbSendQuery(con, paste0("SELECT TOP 1 * FROM [", tbl, "]"))
      info <- dbColumnInfo(res)
      dbClearResult(res)
      info
    },
    fejl_besked = paste0(tbl, ": kolonne-info fejlede")
  )

  # Sample (5 rækker)
  sample <- med_fresh_con(
    operation = function(con) {
      dbGetQuery(con, paste0("SELECT TOP 5 * FROM [", tbl, "]"))
    },
    fejl_besked = paste0(tbl, ": sample fejlede")
  )

  tabel_info[[tbl]] <- list(
    navn = tbl,
    row_count = row_counts[[tbl]],
    kolonne_antal = if (is.null(cols)) NA_integer_ else nrow(cols),
    kolonner = kolonner_til_liste(cols),
    sample_raekker = if (is.null(sample)) NULL else
      lapply(seq_len(nrow(sample)), function(i) as.list(sample[i, , drop = FALSE]))
  )

  log_skriv(paste0("    ", if (is.null(cols)) "?" else nrow(cols),
                    " kolonner, ", if (is.null(sample)) 0 else nrow(sample),
                    " sample-rækker"))
}

# ------------------------------------------------------------------------------
# Step 3: Dump skema til YAML
# ------------------------------------------------------------------------------
log_skriv("Step 3: Skriver access_schema.yaml ...")

schema <- list(
  source_database = "dataportal_2024.accdb",
  source_dsn = DSN_NAVN,
  generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  generated_on = paste0(Sys.info()["nodename"], " (", R.version$platform, ")"),
  tabeller = tabel_info,
  noter = c(
    paste0("FK-relationer ER IKKE i denne fil — eksportér via Access GUI:"),
    paste0("  Database Tools > Relationships > screenshot som access_relationships.png"),
    paste0("  Database Tools > Database Documenter > PDF til access_database_documenter.pdf"),
    paste0("Indexes er heller ikke programmatisk hentet — Database Documenter ovenfor dækker dem"),
    paste0("Hvis flere tabeller findes i Access end de ", length(KENDTE_TABELLER),
           " kendte: tilføj dem til KENDTE_TABELLER i 00_introspect_access.R og genkør")
  )
)

yaml::write_yaml(schema, file = SCHEMA_FIL)
log_skriv(paste0("  Skrevet: ", SCHEMA_FIL))

# ------------------------------------------------------------------------------
# Step 4: Dump alle data til Parquet
# ------------------------------------------------------------------------------
log_skriv("Step 4: Dumper alle tabeller til Parquet ...")

dump_resultat <- list()

for (tbl in faktiske_tabeller) {
  parquet_path <- file.path(DATA_DUMP_DIR, paste0(tbl, ".parquet"))

  data <- med_fresh_con(
    operation = function(con) dbReadTable(con, tbl),
    fejl_besked = paste0(tbl, ": dbReadTable fejlede")
  )

  if (is.null(data)) {
    dump_resultat[[tbl]] <- list(status = "FEJL")
    log_skriv(paste0("  ", tbl, ": FEJL — sprunget over"), "ERROR")
    next
  }

  res <- tryCatch({
    arrow::write_parquet(data, parquet_path)
    list(status = "OK", rows = nrow(data),
         size_kb = round(file.size(parquet_path) / 1024, 1))
  }, error = function(e) {
    list(status = "FEJL", besked = e$message)
  })

  dump_resultat[[tbl]] <- res
  if (res$status == "OK") {
    log_skriv(paste0("  ", tbl, ": ", res$rows, " rækker, ",
                      res$size_kb, " KB"))
  } else {
    log_skriv(paste0("  ", tbl, " Parquet-write FEJL: ", res$besked), "ERROR")
  }
}

# ------------------------------------------------------------------------------
# Opsummering
# ------------------------------------------------------------------------------
n_ok <- sum(sapply(dump_resultat, \(r) r$status == "OK"))
log_skriv("===== Opsummering =====")
log_skriv(paste0("Tabeller fundet:        ", length(faktiske_tabeller)))
log_skriv(paste0("Parquet-dumps succes:   ", n_ok))
log_skriv(paste0("Schema-fil:             ", SCHEMA_FIL))
log_skriv(paste0("Data-dump-mappe:        ", DATA_DUMP_DIR))
log_skriv("===== Færdig =====")

cat("\n")
cat("==== NÆSTE SKRIDT (MANUELT I ACCESS GUI) ====\n")
cat("1. Database Tools > Relationships > screenshot\n")
cat("   Gem som: ", file.path(ARBEJDSMAPPE, "access_relationships.png"), "\n")
cat("2. Database Tools > Database Documenter > PDF\n")
cat("   Gem som: ", file.path(ARBEJDSMAPPE, "access_database_documenter.pdf"), "\n")
cat("3. Pak til ZIP: bash 99_pak_zip.sh\n")
