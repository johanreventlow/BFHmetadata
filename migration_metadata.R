# =============================================================================
# migration_metadata.R — Delt sandhedskilde for skema-metadata
# =============================================================================
# Sources af BÅDE 01_generate_ddl.R og 02_migrate_data.R. FK/PK-map + type-
# mapping holdes ÉT sted så de ikke drifter mellem scripts.
#
# Kilde: access_database_documenter.pdf (Relationer + Tabelindeks),
# verificeret mod access_schema.yaml 2026-06-07.
# =============================================================================

# --- Type-mapping: ODBC-typekode → Postgres. Fail-loud på ukendt kode. -------
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

# --- PK-map: tabeller med enkelt surrogat-PK (eksakt casing) -----------------
# Junction-tabeller (tblForbind*) udeladt bevidst — ingen PK i v1.
PK_MAP <- list(
  tblIndikatorer               = "id",
  tblIndikatorHierarki         = "Id",
  tblIndikatorNiveauer         = "Id",
  tblFaggrupper                = "Id",
  tblDatakilder                = "Id",
  tblPersoner                  = "Id",
  tblOrganisationStruktur      = "Id",
  tblOrganisationOversaettelse = "Id",
  tblOrganisationNiveauer      = "Id",
  tblDiagrammer                = "id",
  tblDiagramTyper              = "Id",
  tblDiagramIndstillinger      = "Id",
  tblDiagrammerMaal            = "id",
  tblDiagrammerMedian          = "id",
  tblDiagrammerKommentar       = "id",
  tblDataprodukter             = "Id"
)

# --- FK-map: 17 relationer, alle int → parent-PK -----------------------------
# enforced=FALSE → kommenteres i DDL + valideres ekstra i orphan-tjek.
FK_MAP <- list(
  list(child="tblForbindIndikatorerDataprodukter", col="dataprodukt_id",              parent="tblDataprodukter",        pcol="Id", enforced=TRUE),
  list(child="tblForbindIndikatorerDataprodukter", col="indikator_id",                parent="tblIndikatorer",          pcol="id", enforced=TRUE),
  list(child="tblForbindIndikatorerFaggrupper",    col="faggruppe_id",                parent="tblFaggrupper",           pcol="Id", enforced=TRUE),
  list(child="tblForbindIndikatorerFaggrupper",    col="indikator_id",                parent="tblIndikatorer",          pcol="id", enforced=TRUE),
  list(child="tblForbindIndikatorerOrganisation",  col="indikator_id",                parent="tblIndikatorer",          pcol="id", enforced=TRUE),
  list(child="tblForbindIndikatorerOrganisation",  col="organisations_id",            parent="tblOrganisationStruktur", pcol="Id", enforced=TRUE),
  list(child="tblDiagrammer",                      col="diagram_type",                parent="tblDiagramTyper",         pcol="Id", enforced=TRUE),
  list(child="tblDiagrammer",                      col="indikator",                   parent="tblIndikatorer",          pcol="id", enforced=TRUE),
  list(child="tblDiagrammer",                      col="organisatorisk_navn_teknisk", parent="tblOrganisationStruktur", pcol="Id", enforced=TRUE),
  list(child="tblIndikatorer",                     col="indikator_hierarki",          parent="tblIndikatorHierarki",    pcol="Id", enforced=TRUE),
  list(child="tblIndikatorer",                     col="kontaktperson",               parent="tblPersoner",             pcol="Id", enforced=TRUE),
  list(child="tblIndikatorHierarki",               col="indikator_niveau",            parent="tblIndikatorNiveauer",    pcol="Id", enforced=TRUE),
  list(child="tblOrganisationStruktur",            col="organisatorisk_niveau",       parent="tblOrganisationNiveauer", pcol="Id", enforced=TRUE),
  list(child="tblOrganisationStruktur",            col="parent_Id",                   parent="tblOrganisationStruktur", pcol="Id", enforced=TRUE),  # self-join
  list(child="tblOrganisationOversaettelse",       col="organisatorisk_navn_teknisk", parent="tblOrganisationStruktur", pcol="Id", enforced=TRUE),
  list(child="tblPersoner",                        col="organisatorisk_enhed",        parent="tblOrganisationStruktur", pcol="Id", enforced=TRUE),
  # Ikke-gennemtvunget i Access (mulige orphan-rækker)
  list(child="tblDiagrammerMedian",                col="diagram",                     parent="tblDiagrammer",           pcol="id", enforced=FALSE)
)

# --- ALTER TABLE ADD FK-statement (double-quoted idents) ---------------------
# Delt af 01 (DDL-gen) og 02 (auto-aktivering af rene ikke-enforced FK'er).
mk_fk_stmt <- function(fk) {
  qi <- function(x) sprintf('"%s"', x)
  sprintf('ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s (%s);',
          qi(fk$child), qi(sprintf("fk_%s_%s", fk$child, fk$col)),
          qi(fk$col), qi(fk$parent), qi(fk$pcol))
}
