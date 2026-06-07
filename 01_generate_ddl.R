# =============================================================================
# 01_generate_ddl.R — Trin 1: Generér Postgres-DDL fra Access-skema
# =============================================================================
# Læser access_schema.yaml (Trin 0b output) + statisk verificeret FK/PK-map
# (kilde: access_database_documenter.pdf) og skriver 01_schema.sql.
#
# Konservativ 1:1-migration: bevarer tabel-/kolonne-navne (inkl. æ/ø/å) og
# eksakt casing via double-quote-citater. Kun PK-kolonner får NOT NULL.
#
# Kør:  Rscript 01_generate_ddl.R
# =============================================================================

suppressPackageStartupMessages({
  library(yaml)
})

# --- Lille log-helper (ej rå cat) -------------------------------------------
log_msg <- function(...) message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), paste0(...)))

# --- Stier -------------------------------------------------------------------
cfg <- yaml::read_yaml("config.yml")$default
schema_path <- cfg$paths$schema_yaml      # access_schema.yaml
out_path    <- cfg$paths$ddl_sql          # 01_schema.sql

# =============================================================================
# Type-mapping: ODBC-typekode → Postgres. Fail-loud på ukendt kode.
# Verificeret sæt i access_schema.yaml: 4, 12, -1, -7, 93, 2
# =============================================================================
map_odbc_type <- function(code) {
  m <- c(
    "4"   = "INTEGER",    # LONG / INTEGER (autonumber for PK håndteres separat)
    "12"  = "TEXT",       # TEXT/varchar — YAML mangler størrelser → konservativ TEXT
    "-1"  = "TEXT",       # MEMO
    "-7"  = "BOOLEAN",    # BIT
    "93"  = "TIMESTAMP",  # DATETIME
    "2"   = "NUMERIC"     # NUMERIC (kun maal_vaerdi)
  )
  key <- as.character(code)
  if (!key %in% names(m)) {
    stop(sprintf("Ukendt ODBC-typekode '%s' — tilføj mapping i map_odbc_type()", key),
         call. = FALSE)
  }
  unname(m[key])
}

# =============================================================================
# PK-map: tabeller med enkelt surrogat-PK (id/Id, case varierer).
# Junction-tabeller (tblForbind*) udelades bevidst — ingen PK i v1
# (undgå import-fejl ved evt. dubletter; composite-PK overvejes efter dublet-tjek).
# Værdien er den EKSAKTE kolonne-casing fra Access.
# =============================================================================
pk_map <- list(
  tblIndikatorer                     = "id",
  tblIndikatorHierarki               = "Id",
  tblIndikatorNiveauer               = "Id",
  tblFaggrupper                      = "Id",
  tblDatakilder                      = "Id",
  tblPersoner                        = "Id",
  tblOrganisationStruktur            = "Id",
  tblOrganisationOversaettelse       = "Id",
  tblOrganisationNiveauer            = "Id",
  tblDiagrammer                      = "id",
  tblDiagramTyper                    = "Id",
  tblDiagramIndstillinger            = "Id",
  tblDiagrammerMaal                  = "id",
  tblDiagrammerMedian                = "id",
  tblDiagrammerKommentar             = "id",
  tblDataprodukter                   = "Id"
  # tblForbindIndikatorerFaggrupper    — ingen PK (junction)
  # tblForbindIndikatorerOrganisation  — ingen PK (junction)
  # tblForbindIndikatorerDataprodukter — ingen PK (junction)
)

# =============================================================================
# FK-map: 17 relationer, alle int → parent-PK. Verificeret mod
# access_database_documenter.pdf (Relationer-sektioner) 2026-06-07.
# enforced = FALSE → kommenteres ud (orphan-tjek i Trin 2 før aktivering).
# Eksakt casing på parent-kolonne bevaret.
# =============================================================================
fk_map <- list(
  list(child="tblForbindIndikatorerDataprodukter", col="dataprodukt_id",            parent="tblDataprodukter",        pcol="Id", enforced=TRUE),
  list(child="tblForbindIndikatorerDataprodukter", col="indikator_id",              parent="tblIndikatorer",          pcol="id", enforced=TRUE),
  list(child="tblForbindIndikatorerFaggrupper",    col="faggruppe_id",              parent="tblFaggrupper",           pcol="Id", enforced=TRUE),
  list(child="tblForbindIndikatorerFaggrupper",    col="indikator_id",              parent="tblIndikatorer",          pcol="id", enforced=TRUE),
  list(child="tblForbindIndikatorerOrganisation",  col="indikator_id",              parent="tblIndikatorer",          pcol="id", enforced=TRUE),
  list(child="tblForbindIndikatorerOrganisation",  col="organisations_id",          parent="tblOrganisationStruktur", pcol="Id", enforced=TRUE),
  list(child="tblDiagrammer",                      col="diagram_type",              parent="tblDiagramTyper",         pcol="Id", enforced=TRUE),
  list(child="tblDiagrammer",                      col="indikator",                 parent="tblIndikatorer",          pcol="id", enforced=TRUE),
  list(child="tblDiagrammer",                      col="organisatorisk_navn_teknisk", parent="tblOrganisationStruktur", pcol="Id", enforced=TRUE),
  list(child="tblIndikatorer",                     col="indikator_hierarki",        parent="tblIndikatorHierarki",    pcol="Id", enforced=TRUE),
  list(child="tblIndikatorer",                     col="kontaktperson",             parent="tblPersoner",             pcol="Id", enforced=TRUE),
  list(child="tblIndikatorHierarki",               col="indikator_niveau",          parent="tblIndikatorNiveauer",    pcol="Id", enforced=TRUE),
  list(child="tblOrganisationStruktur",            col="organisatorisk_niveau",     parent="tblOrganisationNiveauer", pcol="Id", enforced=TRUE),
  list(child="tblOrganisationStruktur",            col="parent_Id",                 parent="tblOrganisationStruktur", pcol="Id", enforced=TRUE),  # self-join
  list(child="tblOrganisationOversaettelse",       col="organisatorisk_navn_teknisk", parent="tblOrganisationStruktur", pcol="Id", enforced=TRUE),
  list(child="tblPersoner",                        col="organisatorisk_enhed",      parent="tblOrganisationStruktur", pcol="Id", enforced=TRUE),
  # Ikke-gennemtvunget i Access — kommenteres ud, aktiveres i Trin 2 efter orphan-tjek
  list(child="tblDiagrammerMedian",                col="diagram",                   parent="tblDiagrammer",           pcol="id", enforced=FALSE)
)

# --- Identifier-quoting (bevar casing + æ/ø/å) -------------------------------
q <- function(x) sprintf('"%s"', x)

# =============================================================================
# Generér DDL
# =============================================================================
log_msg("Læser skema: ", schema_path)
schema <- yaml::read_yaml(schema_path)
tabeller <- schema$tabeller
log_msg(length(tabeller), " tabeller indlæst")

lines <- c(
  "-- =============================================================================",
  "-- 01_schema.sql — Postgres-DDL for BFH-metadata (Access → Supabase)",
  sprintf("-- Genereret: %s af 01_generate_ddl.R", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("-- Kilde: %s + access_database_documenter.pdf", schema_path),
  "-- Konservativ 1:1-migration. Quoted identifiers bevarer casing + æ/ø/å.",
  "-- =============================================================================",
  ""
)

# --- CREATE TABLE pr. tabel --------------------------------------------------
for (tname in names(tabeller)) {
  tbl <- tabeller[[tname]]
  pk  <- pk_map[[tname]]   # NULL hvis junction
  coldefs <- character(0)

  for (kol in tbl$kolonner) {
    cname <- kol$navn
    is_pk <- !is.null(pk) && identical(cname, pk)
    if (is_pk) {
      # Autonumber-PK → IDENTITY BY DEFAULT (tillader eksplicit Id-import i Trin 2)
      coldef <- sprintf("  %s INTEGER GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY", q(cname))
    } else {
      coldef <- sprintf("  %s %s", q(cname), map_odbc_type(kol$type))
    }
    coldefs <- c(coldefs, coldef)
  }

  lines <- c(lines,
    sprintf("CREATE TABLE %s (", q(tname)),
    paste(coldefs, collapse = ",\n"),
    ");",
    "")
}

# --- ALTER TABLE ADD FOREIGN KEY ---------------------------------------------
lines <- c(lines,
  "-- =============================================================================",
  "-- Foreign keys (separat blok — load-rækkefølge ligegyldig)",
  "-- =============================================================================",
  "")

mk_fk <- function(fk) {
  cname_constraint <- sprintf("fk_%s_%s", fk$child, fk$col)
  sprintf('ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s (%s);',
          q(fk$child), q(cname_constraint), q(fk$col), q(fk$parent), q(fk$pcol))
}

for (fk in fk_map) {
  stmt <- mk_fk(fk)
  if (isTRUE(fk$enforced)) {
    lines <- c(lines, stmt)
  } else {
    lines <- c(lines,
      "-- IKKE-gennemtvunget i Access (mulige orphan-rækker).",
      "-- Aktivér i Trin 2 EFTER orphan-tjek:",
      paste0("-- ", stmt))
  }
}

# --- Skriv UTF-8 / LF --------------------------------------------------------
con <- file(out_path, open = "wb", encoding = "UTF-8")
writeLines(lines, con, sep = "\n")
close(con)

n_fk_active <- sum(vapply(fk_map, function(f) isTRUE(f$enforced), logical(1)))
log_msg("Skrevet: ", out_path)
log_msg(sprintf("  %d CREATE TABLE, %d aktive FK, %d kommenteret",
                length(tabeller), n_fk_active, length(fk_map) - n_fk_active))
log_msg("Færdig. Review 01_schema.sql før Trin 2 (data-import).")
