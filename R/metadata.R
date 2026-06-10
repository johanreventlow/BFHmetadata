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

# --- tblIndikatorer felt-metadata til CRUD-form ------------------------------
# kind: pk | fk | text | textarea | bool | int | date
INDIKATOR_FIELDS <- list(
  list(col="id",                       kind="pk"),
  list(col="indikator_hierarki",       kind="fk", parent="tblIndikatorHierarki", label="hierarki_navn"),
  list(col="indikator_navn",           kind="text"),
  list(col="indikator_navn_teknisk",   kind="text"),
  list(col="kontaktperson",            kind="fk", parent="tblPersoner",          label="fornavn||' '||efternavn"),
  list(col="sp_rapport_id",            kind="text"),
  list(col="tillad_auto_opdatering",   kind="bool"),
  list(col="aktiv_indikator",          kind="bool"),
  list(col="nøgleindikator",           kind="bool"),
  list(col="definition_kort",          kind="textarea"),
  list(col="definition_dataportal",    kind="textarea"),
  list(col="tæller_beskrivelse",       kind="textarea"),
  list(col="nævner_beskrivelse",       kind="textarea"),
  list(col="indikator_ukompatibel_med",kind="textarea"),
  list(col="mål",                      kind="text"),
  list(col="datakilde",                kind="fk", parent="tblDatakilder",        label="datakilde_navn"),
  list(col="direkte_link",             kind="text"),
  list(col="ønsket_tendens",           kind="text"),
  list(col="antal_observationer",      kind="int"),
  list(col="periode_fra",              kind="date"),
  list(col="output_enhed",             kind="text")
)

# Felter sikre til inline DT-redigering (simple tekst/tal — ej FK/bool/date)
INLINE_EDITABLE <- c("indikator_navn", "mål", "output_enhed",
                     "direkte_link", "ønsket_tendens")

# --- m2m junction-metadata for tblIndikatorer --------------------------------
# Junctions har ingen PK (kun (indikator_id, parent_id)) → skrives via replace.
# label: SQL-udtryk for parent-tekstværdi (double-quoted idents / COALESCE).
INDIKATOR_JUNCTIONS <- list(
  faggrupper    = list(table = "tblForbindIndikatorerFaggrupper",
                       fk = "faggruppe_id",   parent = "tblFaggrupper",
                       parent_pk = "Id", label = '"faggruppe"'),
  dataprodukter = list(table = "tblForbindIndikatorerDataprodukter",
                       fk = "dataprodukt_id", parent = "tblDataprodukter",
                       parent_pk = "Id", label = '"dataprodukt_navn"'),
  organisation  = list(table = "tblForbindIndikatorerOrganisation",
                       fk = "organisations_id", parent = "tblOrganisationStruktur",
                       parent_pk = "Id",
                       label = 'COALESCE("organisatorisk_navn_langt","organisatorisk_navn_teknisk")')
)

# --- Simple opslagstabeller (Class A) til generisk inline-redigering ----------
# Hver: id (modul-namespace/nav-value), table, pk ("Id" for alle), label (vist),
# cols (ordnet: col/type/label; type "int" coerces + valideres). ref_check kun
# hvor DB ikke enforcer FK (tblDatakilder) → app-niveau "i brug"-tjek før slet.
# Verificeret mod access_schema.yaml 2026-06-10.
LOOKUP_TABLES <- list(
  list(id = "faggrupper", table = "tblFaggrupper", pk = "Id", label = "Faggrupper",
       cols = list(
         list(col = "faggruppe", type = "text", label = "Faggruppe"))),
  list(id = "datakilder", table = "tblDatakilder", pk = "Id", label = "Datakilder",
       ref_check = list(child = "tblIndikatorer", col = "datakilde"),
       cols = list(
         list(col = "datakilde_navn",        type = "text", label = "Navn"),
         list(col = "datakilde_beskrivelse", type = "text", label = "Beskrivelse"))),
  list(id = "dataprodukter", table = "tblDataprodukter", pk = "Id", label = "Dataprodukter",
       cols = list(
         list(col = "dataprodukt_navn",        type = "text", label = "Navn"),
         list(col = "dataprodukt_kort_navn",   type = "text", label = "Kort navn"),
         list(col = "dataprodukt_beskrivelse", type = "text", label = "Beskrivelse"))),
  list(id = "diagramtyper", table = "tblDiagramTyper", pk = "Id", label = "Diagramtyper",
       cols = list(
         list(col = "diagram_type",           type = "text", label = "Type"),
         list(col = "diagram_type_kommentar", type = "text", label = "Kommentar"))),
  list(id = "org_niveauer", table = "tblOrganisationNiveauer", pk = "Id",
       label = "Organisations-niveauer",
       cols = list(
         list(col = "organisatorisk_niveau",      type = "int",  label = "Niveau (tal)"),
         list(col = "organisatorisk_niveau_navn", type = "text", label = "Niveau-navn"))),
  list(id = "indikator_niveauer", table = "tblIndikatorNiveauer", pk = "Id",
       label = "Indikator-niveauer",
       cols = list(
         list(col = "indikator_niveau",      type = "int",  label = "Niveau (tal)"),
         list(col = "indikator_niveau_navn", type = "text", label = "Niveau-navn")))
)
