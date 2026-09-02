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
    stop(sprintf("Ukendt ODBC-typekode '%s' \u2014 tilf\u00F8j mapping i map_odbc_type()", key),
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

# Kanoniske output_enhed-værdier. SKAL matche BFHddl::map_output_enhed
# (target_parsing.R) — værdier udenfor sættet ignoreres af diagram-
# pipelinen (y-akse falder tilbage til default). Derfor dropdown, ej fritekst.
OUTPUT_ENHED_CHOICES <- c(
  "Procent", "Andel", "Antal", "Kr", "Tid (dage)", "Tid (timer)",
  "Tid (minutter)", "Tid (tt:mm)", "Rate"
)

# Kanoniske maal_retning-værdier (tblDiagrammerMaal) — komparator for
# diagrammets måltal. Dropdown, ej fritekst, for at undgå frie varianter.
MAAL_RETNING_CHOICES <- c(">=", "<=", "=", "<", ">")

# --- tblIndikatorer felt-metadata til CRUD-form ------------------------------
# kind: pk | fk | text | textarea | bool | int | date | choice (fast værdisæt)
INDIKATOR_FIELDS <- list(
  list(col="id",                       kind="pk"),
  # aktiv_col: fk_options medtager aktiv-flag → dropdowns kan filtrere
  # inaktive noder ved nyvalg (eksisterende vaerdier bevares med suffix)
  # parent_col: dropdown vises som indrykket trae (hierarchy_indent_options)
  list(col="indikator_hierarki",       kind="fk", parent="tblIndikatorHierarki",
       label="hierarki_navn", aktiv_col="aktiv", parent_col="parent_id"),
  list(col="indikator_navn",           kind="text"),
  list(col="indikator_navn_teknisk",   kind="text"),
  list(col="kontaktperson",            kind="fk", parent="tblPersoner",          label="fornavn||' '||efternavn"),
  list(col="sp_rapport_id",            kind="text"),
  list(col="tillad_auto_opdatering",   kind="bool"),
  list(col="aktiv_indikator",          kind="bool"),
  list(col="n\u00F8gleindikator",           kind="bool"),
  # Opt-in: nulfyld tomme perioder i hændelsestællinger (BFHddl
  # fill_empty_periods, DATA_CONVENTIONS §3b). Kræver migration
  # 04_add_nulfyld_tomme_perioder.sql i Supabase.
  list(col="nulfyld_tomme_perioder",   kind="bool"),
  list(col="definition_kort",          kind="textarea"),
  list(col="definition_dataportal",    kind="textarea"),
  list(col="t\u00E6ller_beskrivelse",       kind="textarea"),
  list(col="n\u00E6vner_beskrivelse",       kind="textarea"),
  list(col="indikator_ukompatibel_med",kind="textarea"),
  list(col="m\u00E5l",                      kind="text"),
  list(col="datakilde",                kind="fk", parent="tblDatakilder",        label="datakilde_navn"),
  list(col="direkte_link",             kind="text"),
  list(col="\u00F8nsket_tendens",           kind="text"),
  list(col="antal_observationer",      kind="int"),
  list(col="periode_fra",              kind="date"),
  list(col="output_enhed",             kind="choice", choices=OUTPUT_ENHED_CHOICES)
)

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

# Adskiller mellem sammensatte dele af en dropdown-label (fx teknisk navn og
# langt navn). Holdes som konstant saa label_expr og tests deler definition.
LABEL_SEPARATOR <- " \u2014 "

# --- Simple opslagstabeller (Class A) til generisk inline-redigering ----------
# Hver: id (modul-namespace/nav-value), table, pk ("Id" for alle), label (vist),
# cols (ordnet: col/type/label; type "int" coerces + valideres). ref_check kun
# hvor DB ikke enforcer FK (tblDatakilder) → app-niveau "i brug"-tjek før slet.
# Verificeret mod access_schema.yaml 2026-06-10.
LOOKUP_TABLES <- list(
  list(id = "faggrupper", table = "tblFaggrupper", pk = "Id", label = "Faggrupper",
       excel_adapter = TRUE,
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
         list(col = "indikator_niveau_navn", type = "text", label = "Niveau-navn"))),
  # FK-tabel: organisatorisk_enhed redigeres via dropdown (label fra org-struktur).
  # Slet-guard via DB-FK (tblIndikatorer.kontaktperson → Personer).
  list(id = "personer", table = "tblPersoner", pk = "Id", label = "Personer",
       cols = list(
         list(col = "fornavn",   type = "text", label = "Fornavn"),
         list(col = "efternavn", type = "text", label = "Efternavn"),
         list(col = "titel",     type = "text", label = "Titel"),
         list(col = "email",     type = "text", label = "E-mail"),
         # parent_col: dropdown vises som indrykket org-trae
         list(col = "organisatorisk_enhed", type = "fk", label = "Organisatorisk enhed",
              parent = "tblOrganisationStruktur", parent_pk = "Id",
              parent_col = "parent_Id",
              label_expr = 'COALESCE("organisatorisk_navn_langt","organisatorisk_navn_teknisk")'))),
  # Oversættelse: navn-fra-data → organisatorisk enhed. Bruges af signal-scan
  # (enhed-varianter); sletning ufarlig (intet refererer til rækkerne).
  list(id = "org_oversaettelse", table = "tblOrganisationOversaettelse",
       pk = "Id", label = "Organisations-overs\u00E6ttelse",
       cols = list(
         list(col = "organisatorisk_navn_fra_data", type = "text",
              label = "Navn fra data"),
         # parent_col: dropdown vises som indrykket trae, saa den hierarkiske
         # placering fremgaar. Label = teknisk navn (det brugeren genkender)
         # + separator + langt navn; det lange navn udelades hvis det mangler
         # eller er identisk med det tekniske (CONCAT_WS springer NULL over).
         list(col = "organisatorisk_navn_teknisk", type = "fk",
              label = "Organisatorisk enhed",
              parent = "tblOrganisationStruktur", parent_pk = "Id",
              parent_col = "parent_Id",
              label_expr = sprintf(
                paste0("CONCAT_WS('%s', ",
                       "NULLIF(\"organisatorisk_navn_teknisk\", ''), ",
                       "NULLIF(\"organisatorisk_navn_langt\", ",
                       "\"organisatorisk_navn_teknisk\"))"),
                LABEL_SEPARATOR))))
)

# --- Hierarki-tabeller (traeer med parent-FK) til generisk mod_hierarchy ------
# level: FK-kolonne paa noden + parent-tabel med numerisk niveau (num_col, til
# bloed niveau-konsistens-advarsel) og visningsnavn (name_col/label_expr).
# aktiv_col: NULL hvis tabellen ingen aktiv-kolonne har.
HIERARCHY_TABLES <- list(
  org_struktur = list(
    id = "org_struktur", table = "tblOrganisationStruktur", pk = "Id",
    parent_col = "parent_Id", display_col = "organisatorisk_navn_langt",
    label = "Organisations-struktur", aktiv_col = NULL,
    fields = list(
      list(col = "organisatorisk_navn_teknisk", type = "text",
           label = "Teknisk navn"),
      list(col = "organisatorisk_navn_langt", type = "text",
           label = "Langt navn"),
      list(col = "organisatorisk_navn_kort", type = "text",
           label = "Kort navn")),
    level = list(col = "organisatorisk_niveau",
                 parent = "tblOrganisationNiveauer", parent_pk = "Id",
                 num_col = "organisatorisk_niveau",
                 name_col = "organisatorisk_niveau_navn",
                 label_expr = '"organisatorisk_niveau_navn"')),
  # Datasæt/datapakke-træet bag tblIndikatorer.indikator_hierarki.
  # parent_col er "parent_id" med LILLE i — modsat org-tabellens "parent_Id".
  indikator_hierarki = list(
    id = "indikator_hierarki", table = "tblIndikatorHierarki", pk = "Id",
    parent_col = "parent_id", display_col = "hierarki_navn",
    label = "Indikator-hierarki", aktiv_col = "aktiv",
    # Kaskade-filtre (som Indikator-sidens Datapakke/Datasæt): hvert filter
    # tilbyder noder paa et navngivet niveau; valg beskaerer grid'et til
    # grenen under noden. Opt-in — org-instansen har ingen filtre.
    filters = list(
      list(id = "datapakke", label = "Datapakke", niveau_navn = "Datapakke"),
      list(id = "datasaet", label = "Datas\u00E6t", niveau_navn = "Datas\u00E6t")),
    fields = list(
      list(col = "hierarki_navn", type = "text", label = "Navn"),
      list(col = "hierarki_navn_kort", type = "text", label = "Kort navn"),
      # Opt-in: BFHddl viser hierarki_navn_kort i chart-titlens
      # datasaet-linje naar flaget er sat (fx LUP). Det lange navn
      # forbliver autoritativt til dataportal-generering.
      list(col = "brug_kort_navn_i_titel", type = "checkbox",
           label = "Kort navn i titel"),
      list(col = "beskrivelse_kort", type = "textarea",
           label = "Kort beskrivelse"),
      list(col = "beskrivelse_lang", type = "textarea",
           label = "Lang beskrivelse"),
      list(col = "kilde_id", type = "text", label = "Kilde-id (import)")),
    level = list(col = "indikator_niveau", parent = "tblIndikatorNiveauer",
                 parent_pk = "Id", num_col = "indikator_niveau",
                 name_col = "indikator_niveau_navn",
                 label_expr = '"indikator_niveau_navn"'))
)

# --- Bulk-redigering: serverside-allowlist (Leverance 2 af ------------------
# docs/plans/2026-08-30-bulk-redigering-design.md) ---------------------------
# Kun felter herfra kan rammes af bulk_update() — browserens felt-valg slås
# op her FØR nogen DB-forbindelse åbnes (krav 5, hardening-designet:
# browser-events må ikke direkte afgøre SQL-kolonner). "kind" genbruger de
# samme kategorier som INDIKATOR_FIELDS/mod_indikator_crud's
# .IND_FK_FIELDS/.IND_BOOL_FIELDS: bool | fk | choice | text.
# indikator_navn og indikator_navn_teknisk er UDELADT med vilje: et fælles navn
# på N rækker giver ingen mening, og navn_teknisk er parquet-nøglen — den kan
# KUN ændres én ad gangen via indikator-modalen, bag en bekræftelsesdialog
# (mod_indikator_crud: .byg_teknisk_confirm). Grid'et holder den readOnly.
BULK_INDIKATOR_FIELDS <- list(
  list(col = "aktiv_indikator",          kind = "bool"),
  list(col = "nøgleindikator",           kind = "bool"),
  list(col = "nulfyld_tomme_perioder",   kind = "bool"),
  list(col = "tillad_auto_opdatering",   kind = "bool"),
  list(col = "indikator_hierarki",       kind = "fk"),
  list(col = "kontaktperson",            kind = "fk"),
  list(col = "datakilde",                kind = "fk"),
  list(col = "output_enhed",   kind = "choice", choices = OUTPUT_ENHED_CHOICES),
  list(col = "ønsket_tendens",           kind = "text"),
  list(col = "mål",                      kind = "text")
)

# indikator og organisatorisk_navn_teknisk er UDELADT med vilje: at flytte N
# diagrammer til samme indikator/enhed kolliderer med duplikat-guarden og er
# sjældent intentionen (jf. design-dokumentet).
BULK_DIAGRAM_FIELDS <- list(
  list(col = "diagram_aktivt",           kind = "bool"),
  list(col = "direktionens_tavle",       kind = "bool"),
  list(col = "indgaar_i_aggregering",    kind = "bool"),
  list(col = "aggreger_egne_og_boern",   kind = "bool"),
  list(col = "periode_aggregering",      kind = "text"),
  list(col = "maalgruppe",               kind = "fk"),
  list(col = "diagram_type",             kind = "fk")
)

# tabel_key → {table, pk, fields}. Fælles indgang for bulk_update/bulk_undo
# (R/fct_db.R) og SQL-byggerne (R/fct_sql.R) — kun disse to (tabel,kolonne)-
# par kan nogensinde nå en bulk-SQL-streng.
BULK_TABLES <- list(
  indikator = list(table = "tblIndikatorer", pk = "id", fields = BULK_INDIKATOR_FIELDS),
  diagram   = list(table = "tblDiagrammer",  pk = "id", fields = BULK_DIAGRAM_FIELDS)
)

#' Slå (tabel_key, felt) op i bulk-allowlisten. NULL hvis ukendt tabel eller
#' feltet ikke er tilladt i bulk-redigering for den tabel. `tables` kan
#' overrides i integrationstests, der peger på en engangstabel i stedet for
#' den rigtige BULK_TABLES (Leverance 3's audit-migration findes endnu ikke).
#' @noRd
bulk_field_config <- function(tabel_key, felt, tables = BULK_TABLES) {
  tbl <- tables[[tabel_key]]
  if (is.null(tbl)) return(NULL)
  Find(function(f) identical(f$col, felt), tbl$fields)
}

#' Konvertér en rå værdi til feltets deklarerede R-type (fld = element fra
#' en BULK_*_FIELDS-liste, dvs. har $kind og evt. $choices).
#' allow_blank = FALSE (default, bruges til MÅLVÆRDIEN i en batch): bool og
#' fk har intet tomt valg — matcher grid'ets checkbokse/dropdowns, som aldrig
#' tilbyder "ryd feltet". allow_blank = TRUE (bruges til FØRVÆRDIER, som
#' beskriver eksisterende data og godt kan være NULL uanset kind) tillader
#' blank for alle kinds.
#' Fejler (stop()) ved ugyldig værdi FØR nogen transaktion åbnes.
#' @noRd
bulk_coerce_value <- function(fld, raw, allow_blank = FALSE) {
  kind <- fld$kind
  is_blank <- length(raw) == 0L || (length(raw) == 1L && is.na(raw))
  if (is_blank) {
    if (allow_blank) {
      return(switch(kind, bool = NA, fk = NA_integer_, NA_character_))
    }
    if (identical(kind, "text")) return(NA_character_)
    stop("Vælg en værdi — feltet kan ikke sættes tomt", call. = FALSE)
  }
  switch(kind,
    bool = {
      v <- if (is.logical(raw)) raw[1] else identical(as.character(raw)[1], "TRUE")
      if (is.na(v)) stop("Ugyldig bool-værdi", call. = FALSE)
      isTRUE(v)
    },
    fk = {
      iv <- suppressWarnings(as.integer(raw[1]))
      if (is.na(iv)) stop("Vælg en værdi fra listen", call. = FALSE)
      iv
    },
    choice = {
      chr <- as.character(raw)[1]
      if (!chr %in% fld$choices) {
        stop(sprintf("'%s' er ikke en gyldig værdi", chr), call. = FALSE)
      }
      chr
    },
    text = as.character(raw)[1],
    stop(sprintf("Ukendt felt-kind: '%s'", kind), call. = FALSE)
  )
}

#' JSON-repræsentation til audit."tblAendringslog" (vaerdi_foer/vaerdi_efter er
#' jsonb NOT NULL). En manglende værdi bliver til JSON-`null` — ikke SQL NULL,
#' som kolonnen ville afvise. jsonlite står for escaping, så en tekstværdi med
#' anførselstegn eller backslash ikke kan bryde JSON'en.
#' Rundtursstabil sammen med feltets kind, se bulk_json_to_value().
#' @noRd
bulk_value_to_json <- function(kind, value) {
  if (length(value) == 0L || is.na(value)) return("null")
  # Tving værdien på feltets egen type FØR serialisering: ellers kunne en bool
  # der er nået hertil som tekst ende som JSON-strengen "TRUE" i stedet for
  # true, og fortryd ville skrive noget andet tilbage end der stod.
  v <- switch(kind,
    bool = isTRUE(value),
    fk = as.integer(value),
    as.character(value)
  )
  # auto_unbox: skalarer skal være 42 / true / "abc", ikke [42] / [true].
  as.character(jsonlite::toJSON(v, auto_unbox = TRUE, na = "null"))
}

#' Omvendt af bulk_value_to_json() — re-typer en gemt audit-værdi til feltets
#' R-type ved fortryd. JSON-`null` (og manglende input) → NA i feltets type.
#' @noRd
bulk_json_to_value <- function(kind, json) {
  tom <- switch(kind, bool = NA, fk = NA_integer_, NA_character_)
  if (length(json) == 0L || is.na(json)) return(tom)
  v <- jsonlite::fromJSON(json)
  if (is.null(v) || length(v) == 0L || is.na(v)) return(tom)
  switch(kind,
    bool = isTRUE(as.logical(v)),
    fk = as.integer(v),
    as.character(v)
  )
}
