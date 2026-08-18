# Rene SQL-byggere for tblIndikatorer. Identifiers double-quotes (bevar casing+æøå).
# Bruger $n-placeholders (RPostgres parametrisering) → ingen SQL-injection.

#' @noRd
.fk_fields <- function() Filter(function(f) f$kind == "fk", INDIKATOR_FIELDS)

#' CTE-snippet (indsættes efter WITH RECURSIVE): pr. hierarki-node navnet på
#' forfaderen (inklusive noden selv) på niveau 'Datapakke' hhv. 'Datasæt'
#' (h_lvl.label_datapakke/label_datasaet, join på h_lvl.start_id).
#' Niveau-bevidst fordi indikatorernes FK peger på BLANDEDE niveauer (typisk
#' Indikatorsamling under et datasæt) — nodens forælder er derfor IKKE altid
#' datapakken. Niveau-navnene matcher tblIndikatorNiveauer og filtrene i
#' HIERARCHY_TABLES$indikator_hierarki.
#' @noRd
.hierarki_niveau_cte <- function() {
  paste0(
    "h_anc AS (",
    ' SELECT h."Id" AS start_id, h."parent_id", h."hierarki_navn",',
    ' hn."indikator_niveau_navn" AS niveau_navn',
    ' FROM "tblIndikatorHierarki" h',
    ' LEFT JOIN "tblIndikatorNiveauer" hn ON hn."Id" = h."indikator_niveau"',
    " UNION ALL",
    ' SELECT a.start_id, p."parent_id", p."hierarki_navn",',
    ' pn."indikator_niveau_navn"',
    ' FROM h_anc a JOIN "tblIndikatorHierarki" p ON p."Id" = a."parent_id"',
    ' LEFT JOIN "tblIndikatorNiveauer" pn ON pn."Id" = p."indikator_niveau"',
    "), h_lvl AS (",
    " SELECT start_id,",
    ' max("hierarki_navn") FILTER (WHERE niveau_navn = \'Datapakke\')',
    " AS label_datapakke,",
    ' max("hierarki_navn") FILTER (WHERE niveau_navn = \'Datasæt\')',
    " AS label_datasaet",
    " FROM h_anc GROUP BY start_id",
    ")"
  )
}

#' SELECT med FK-labels (LEFT JOIN så NULL-FK bevares)
#' @noRd
build_list_sql <- function() {
  base_cols <- vapply(INDIKATOR_FIELDS, function(f) sprintf('i."%s"', f$col), "")
  joins <- character(0)
  labels <- character(0)
  for (f in .fk_fields()) {
    al <- paste0("p_", f$col)
    labels <- c(labels, sprintf(
      '(%s) AS "label_%s"',
      gsub("([a-zæøå_]+)", sprintf("%s.\\1", al), f$label, perl = TRUE), f$col
    ))
    joins <- c(joins, sprintf(
      'LEFT JOIN "%s" %s ON %s."Id" = i."%s"',
      f$parent, al, al, f$col
    ))
  }
  # Datapakke/Datasæt = forfader-navne på de navngivne niveauer (h_lvl-CTE) —
  # label_indikator_hierarki fra loopet er fortsat nodens EGET navn (dropdown).
  joins <- c(
    joins,
    'LEFT JOIN h_lvl ON h_lvl.start_id = i."indikator_hierarki"'
  )
  labels <- c(
    labels,
    'h_lvl.label_datapakke AS "label_datapakke"',
    'h_lvl.label_datasaet AS "label_datasaet"'
  )
  sprintf(
    'WITH RECURSIVE %s SELECT %s, %s FROM "tblIndikatorer" i %s ORDER BY i."id"',
    .hierarki_niveau_cte(),
    paste(base_cols, collapse = ", "), paste(labels, collapse = ", "),
    paste(joins, collapse = " ")
  )
}

#' id + label for FK-dropdown. parent_col (valgfri): medtag parent-FK'en
#' AS parent_id, saa kalderen kan vise valgene som indrykket trae
#' (hierarchy_indent_options).
#' @noRd
build_fk_options_sql <- function(parent, label_expr, parent_col = NULL) {
  pc <- if (is.null(parent_col)) "" else
    sprintf(', "%s" AS parent_id', parent_col)
  sprintf('SELECT "Id" AS id, (%s) AS label%s FROM "%s" ORDER BY 2',
          label_expr, pc, parent)
}

#' id + label + aktiv-flag for FK-dropdown der skal kunne filtrere inaktive
#' noder klient-side (uden at miste eksisterende vaerdier der peger paa dem)
#' @noRd
build_fk_options_aktiv_sql <- function(parent, label_expr, aktiv_col,
                                       parent_col = NULL) {
  pc <- if (is.null(parent_col)) "" else
    sprintf(', "%s" AS parent_id', parent_col)
  sprintf(
    'SELECT "Id" AS id, (%s) AS label, "%s" AS aktiv%s FROM "%s" ORDER BY 2',
    label_expr, aktiv_col, pc, parent
  )
}

#' Parametriseret UPDATE; cols → $1..$n, id → $(n+1)
#' @noRd
build_update_sql <- function(cols) {
  sets <- vapply(seq_along(cols), function(i) sprintf('"%s" = $%d', cols[i], i), "")
  sprintf(
    'UPDATE "tblIndikatorer" SET %s WHERE "id" = $%d',
    paste(sets, collapse = ", "), length(cols) + 1
  )
}

#' Parametriseret INSERT med RETURNING id
#' @noRd
build_insert_sql <- function(cols) {
  ph <- paste(sprintf("$%d", seq_along(cols)), collapse = ", ")
  qcols <- paste(sprintf('"%s"', cols), collapse = ", ")
  sprintf('INSERT INTO "tblIndikatorer" (%s) VALUES (%s) RETURNING "id"', qcols, ph)
}

#' Soft-delete / gendan
#' @noRd
build_soft_delete_sql <- function() {
  'UPDATE "tblIndikatorer" SET "aktiv_indikator" = $1 WHERE "id" = $2'
}

#' SELECT parent-ids for én indikators junction-rækker
#' @noRd
build_junction_select_sql <- function(j) {
  sprintf('SELECT "%s" FROM "%s" WHERE "indikator_id" = $1', j$fk, j$table)
}

#' DELETE alle junction-rækker for én indikator
#' @noRd
build_junction_delete_sql <- function(j) {
  sprintf('DELETE FROM "%s" WHERE "indikator_id" = $1', j$table)
}

#' Multi-row INSERT: $1 = indikator_id (genbrugt), $2..$(n+1) = parent-ids
#' @noRd
build_junction_insert_sql <- function(j, n) {
  vals <- vapply(seq_len(n), function(i) sprintf("($1, $%d)", i + 1), "")
  sprintf(
    'INSERT INTO "%s" ("indikator_id", "%s") VALUES %s',
    j$table, j$fk, paste(vals, collapse = ", ")
  )
}

#' id + tekst-label for m2m-multiselect
#' OBS: j$label er et betroet SQL-fragment fra INDIKATOR_JUNCTIONS (metadata.R),
#' aldrig bruger-input. Interpoleres bevidst direkte (kan ej parametriseres).
#' @noRd
build_junction_options_sql <- function(j) {
  sprintf(
    'SELECT "%s" AS id, (%s) AS label FROM "%s" ORDER BY 2',
    j$parent_pk, j$label, j$parent
  )
}

# --- Generiske byggere for simple opslagstabeller (inline-redigering) --------
# pk gives eksplicit (alle opslagstabeller bruger "Id" med stort — ikke "id").

#' Hent alle rækker, ordnet på pk
#' @noRd
build_lookup_list_sql <- function(table, pk) {
  sprintf('SELECT * FROM "%s" ORDER BY "%s"', table, pk)
}

#' Opdatér én celle: værdi=$1, pk=$2
#' @noRd
build_lookup_update_sql <- function(table, pk, col) {
  sprintf('UPDATE "%s" SET "%s" = $1 WHERE "%s" = $2', table, col, pk)
}

#' Indsæt blank række (kun pk auto-genereres), returnér ny pk
#' @noRd
build_lookup_insert_sql <- function(table, pk) {
  sprintf('INSERT INTO "%s" DEFAULT VALUES RETURNING "%s"', table, pk)
}

#' Slet række på pk
#' @noRd
build_lookup_delete_sql <- function(table, pk) {
  sprintf('DELETE FROM "%s" WHERE "%s" = $1', table, pk)
}

#' Tæl referencer fra en child-tabel (app-niveau FK-guard hvor DB ej enforcer)
#' @noRd
build_lookup_refcount_sql <- function(child, col) {
  sprintf('SELECT count(*) AS n FROM "%s" WHERE "%s" = $1', child, col)
}

# --- Signal-gennemgang: diagram-indeks + median-knæk ------------------------

#' Ét row pr. aktivt Seriediagram med resolvede labels til filtrering/visning.
#' datapakke/datasaet = niveau-bevidst forfader-opslag (h_lvl-CTE, se
#' .hierarki_niveau_cte). Org-niveauer (overafdeling=5/
#' afdeling=6/afsnit=7) resolves via rekursiv ancestry (selv + forældre op ad
#' parent_Id-træet) → fremtidssikret når diagrammer opstår på dybere niveauer.
#' diagram_type=1 (Seriediagram) + diagram_aktivt.
#' @noRd
build_diagram_index_sql <- function() {
  paste0(
    "WITH RECURSIVE anc AS (",
    ' SELECT "Id" AS start_id, "parent_Id", "organisatorisk_niveau", "organisatorisk_navn_langt"',
    ' FROM "tblOrganisationStruktur"',
    " UNION ALL",
    ' SELECT a.start_id, p."parent_Id", p."organisatorisk_niveau", p."organisatorisk_navn_langt"',
    ' FROM anc a JOIN "tblOrganisationStruktur" p ON p."Id" = a."parent_Id"',
    "), lvl AS (",
    " SELECT start_id,",
    ' max("organisatorisk_navn_langt") FILTER (WHERE "organisatorisk_niveau" = 5) AS overafdeling,',
    ' max("organisatorisk_navn_langt") FILTER (WHERE "organisatorisk_niveau" = 6) AS afdeling,',
    ' max("organisatorisk_navn_langt") FILTER (WHERE "organisatorisk_niveau" = 7) AS afsnit',
    " FROM anc GROUP BY start_id",
    "), ",
    .hierarki_niveau_cte(),
    " ",
    'SELECT d."id" AS diagram_id, d."periode_aggregering", ',
    'i."id" AS indikator_id, i."indikator_navn", i."indikator_navn_teknisk", ',
    # Nulfyld-flag med i indekset: scan_diagram spejler BFHddl's
    # fill_empty_periods, så signalet beregnes på samme serie som tegnes
    'i."nulfyld_tomme_perioder", ',
    "h_lvl.label_datasaet AS datasaet, h_lvl.label_datapakke AS datapakke, ",
    'o."Id" AS org_id, o."organisatorisk_navn_teknisk" AS org_teknisk, ',
    'o."organisatorisk_navn_langt" AS org_navn, o."organisatorisk_niveau" AS org_niveau, ',
    "lvl.overafdeling, lvl.afdeling, lvl.afsnit ",
    'FROM "tblDiagrammer" d ',
    'JOIN "tblIndikatorer" i ON i."id" = d."indikator" ',
    'LEFT JOIN h_lvl ON h_lvl.start_id = i."indikator_hierarki" ',
    'LEFT JOIN "tblOrganisationStruktur" o ON o."Id" = d."organisatorisk_navn_teknisk" ',
    'LEFT JOIN lvl ON lvl.start_id = o."Id" ',
    'WHERE d."diagram_type" = 1 AND d."diagram_aktivt" ',
    'ORDER BY i."indikator_navn", o."organisatorisk_navn_langt"'
  )
}

#' @noRd
build_median_list_sql <- function() {
  'SELECT * FROM "tblDiagrammerMedian" WHERE "diagram" = $1 ORDER BY "laas_median"'
}

#' Alle median-knæk for MANGE diagrammer i ét kald. Et koldt scan af ~600
#' diagrammer ville ellers koste ~600 round-trips til Supabase (Irland,
#' ~40-80 ms hver). Parameteren er en array-LITERAL ("{1,2,3}", byg med
#' pg_int_array()) castet server-side: RPostgres kan ikke binde en R-vektor
#' som Postgres-array — en vektor som param betyder "kør statement N gange",
#' og hvert kald fik så ét nøgent tal til array-parameteren ("malformed
#' array literal: 1727", set i produktion; fallback reddede scannet).
#' @noRd
build_median_batch_sql <- function() {
  paste0(
    'SELECT * FROM "tblDiagrammerMedian" WHERE "diagram" = ANY($1::int[]) ',
    'ORDER BY "diagram", "laas_median"'
  )
}

#' Byg Postgres array-literal af heltal ("{1,2,3}"). Kun hele tal slipper
#' igennem (stopifnot) — literal-strengen interpoleres i en parameter, så
#' den må aldrig kunne indeholde andet end cifre og kommaer. NA droppes.
#' @noRd
pg_int_array <- function(ids) {
  ids <- ids[!is.na(ids)]
  stopifnot(is.numeric(ids), all(ids == as.integer(ids)))
  paste0("{", paste(as.integer(ids), collapse = ","), "}")
}

#' Gem median-knæk MED den aggregering det blev sat under. Uden aggregeringen
#' kan et knæk ikke valideres senere: samme dato giver forskellige faseskift
#' ved forskellig periode (og kan drifte flere uger) — se fct_period.R.
#' @noRd
build_median_insert_sql <- function() {
  paste0(
    'INSERT INTO "tblDiagrammerMedian" ("diagram", "laas_median", ',
    '"aggregering") VALUES ($1, $2, $3) RETURNING "id"'
  )
}

#' @noRd
build_median_delete_sql <- function() {
  'DELETE FROM "tblDiagrammerMedian" WHERE "id" = $1'
}

# --- Diagram-CRUD (admin) ----------------------------------------------------
# Kolonner der redigeres i diagram-formularen (rækkefølge = parameter-orden).
DIAGRAM_COLS <- c(
  "indikator", "organisatorisk_navn_teknisk", "diagram_type",
  "periode_aggregering", "indgaar_i_aggregering",
  "diagram_aktivt", "direktionens_tavle"
)

#' Indikator-options til diagram-siden: id + label + niveau-udledt datasaet
#' (h_lvl-CTE). datasaet driver per-raekke-filtrering af diagram-grid'ets
#' Indikator-dropdown: kun indikatorer under raekkens registrerede datasaet.
#' @noRd
build_diagram_indikator_options_sql <- function() {
  paste0(
    "WITH RECURSIVE ", .hierarki_niveau_cte(), " ",
    'SELECT i."id" AS id, i."indikator_navn" AS label, ',
    "h_lvl.label_datasaet AS datasaet ",
    'FROM "tblIndikatorer" i ',
    'LEFT JOIN h_lvl ON h_lvl.start_id = i."indikator_hierarki" ',
    "ORDER BY 2"
  )
}

#' Alle diagrammer med resolvede labels — INGEN aktiv/type-filtre (admin).
#' datasaet/datapakke = niveau-bevidst forfader-opslag (h_lvl-CTE, samme som
#' build_list_sql/build_diagram_index_sql).
#' @noRd
build_diagram_admin_sql <- function() {
  paste0(
    "WITH RECURSIVE ", .hierarki_niveau_cte(), " ",
    'SELECT d."id" AS diagram_id, d."indikator", ',
    'd."organisatorisk_navn_teknisk", d."diagram_type", ',
    'd."periode_aggregering", d."indgaar_i_aggregering", ',
    'd."diagram_aktivt", d."direktionens_tavle", ',
    'i."indikator_navn", ',
    'COALESCE(o."organisatorisk_navn_langt", o."organisatorisk_navn_teknisk") ',
    "AS org_navn, ",
    't."diagram_type" AS type_navn, ',
    "h_lvl.label_datasaet AS datasaet, h_lvl.label_datapakke AS datapakke ",
    'FROM "tblDiagrammer" d ',
    'LEFT JOIN "tblIndikatorer" i ON i."id" = d."indikator" ',
    'LEFT JOIN "tblOrganisationStruktur" o ',
    'ON o."Id" = d."organisatorisk_navn_teknisk" ',
    'LEFT JOIN "tblDiagramTyper" t ON t."Id" = d."diagram_type" ',
    'LEFT JOIN h_lvl ON h_lvl.start_id = i."indikator_hierarki" ',
    'ORDER BY i."indikator_navn", org_navn'
  )
}

#' @noRd
build_diagram_insert_sql <- function() {
  ph <- paste(sprintf("$%d", seq_along(DIAGRAM_COLS)), collapse = ", ")
  qcols <- paste(sprintf('"%s"', DIAGRAM_COLS), collapse = ", ")
  sprintf(
    'INSERT INTO "tblDiagrammer" (%s) VALUES (%s) RETURNING "id"',
    qcols, ph
  )
}

#' @noRd
build_diagram_update_sql <- function() {
  sets <- vapply(
    seq_along(DIAGRAM_COLS),
    function(i) sprintf('"%s" = $%d', DIAGRAM_COLS[i], i), ""
  )
  sprintf(
    'UPDATE "tblDiagrammer" SET %s WHERE "id" = $%d',
    paste(sets, collapse = ", "), length(DIAGRAM_COLS) + 1
  )
}

#' @noRd
build_diagram_delete_sql <- function() {
  'DELETE FROM "tblDiagrammer" WHERE "id" = $1'
}

#' Blød duplikat-guard: findes (indikator, org, type) allerede (undtagen id)?
#' @noRd
build_diagram_duplicate_sql <- function() {
  paste0(
    'SELECT count(*) AS n FROM "tblDiagrammer" WHERE "indikator" = $1 ',
    'AND "organisatorisk_navn_teknisk" = $2 AND "diagram_type" = $3 ',
    'AND "id" <> $4'
  )
}

#' Distinkte periode-værdier (choices til select — robust ved nye værdier)
#' @noRd
build_diagram_periode_sql <- function() {
  paste0(
    'SELECT DISTINCT "periode_aggregering" FROM "tblDiagrammer" ',
    'WHERE "periode_aggregering" IS NOT NULL ORDER BY 1'
  )
}

#' Antal median-knæk for ét diagram (pre-check før slet → venlig besked)
#' @noRd
build_median_count_sql <- function() {
  'SELECT count(*) AS n FROM "tblDiagrammerMedian" WHERE "diagram" = $1'
}

#' Hele org-træet (id, parent) til hierarki-oprulning. Hentes én gang pr. scan.
#' @noRd
build_org_struct_sql <- function() {
  'SELECT "Id" AS id, "parent_Id" AS parent_id FROM "tblOrganisationStruktur"'
}

#' Aggregerings-flag pr. diagram-række. BEVIDST uden aktiv-filter: BFHddl
#' læser flagene med active_only = FALSE — et inaktivt diagram kan stadig
#' bidrage opad.
#' @noRd
build_aggregation_flags_sql <- function() {
  paste0(
    'SELECT "organisatorisk_navn_teknisk" AS org_id, ',
    '"indikator" AS indikator_id, ',
    '"indgaar_i_aggregering" AS indgaar FROM "tblDiagrammer"'
  )
}

#' Én række pr. (org, enhed-fra-data-variant). LEFT JOIN bevarer organisationer
#' uden oversættelse. Bruges til at bygge parquet-enhed-filter pr. diagram.
#' @noRd
build_org_enhed_variants_sql <- function() {
  paste0(
    'SELECT o."Id" AS org_id, ',
    'o."organisatorisk_navn_teknisk" AS teknisk, ',
    'o."organisatorisk_navn_kort" AS kort, ',
    'o."organisatorisk_navn_langt" AS langt, ',
    'ov."organisatorisk_navn_fra_data" AS fra_data ',
    'FROM "tblOrganisationStruktur" o ',
    'LEFT JOIN "tblOrganisationOversaettelse" ov ',
    'ON ov."organisatorisk_navn_teknisk" = o."Id"'
  )
}

# --- Hierarki-CRUD (config-drevet, HIERARCHY_TABLES) --------------------------

#' Kolonner formularen redigerer (raekkefoelge = parameter-orden).
#' @noRd
hierarchy_edit_cols <- function(cfg) {
  c(
    vapply(cfg$fields, function(f) f$col, ""), cfg$parent_col, cfg$level$col,
    cfg$aktiv_col
  ) # aktiv_col er NULL for org → falder bort i c()
}

#' Alle noder med normaliserede aliaser (id/parent_id_raw) + niveau-join.
#' Aliaser goer mod_hierarchy uafhaengig af casing-forskelle (Id/parent_Id).
#' @noRd
build_hierarchy_list_sql <- function(cfg) {
  fcols <- vapply(cfg$fields, function(f) sprintf('h."%s"', f$col), "")
  aktiv <- if (is.null(cfg$aktiv_col)) {
    ""
  } else {
    sprintf('h."%s" AS aktiv, ', cfg$aktiv_col)
  }
  paste0(
    'SELECT h."', cfg$pk, '" AS id, h."', cfg$parent_col,
    '" AS parent_id_raw, ', paste(fcols, collapse = ", "), ", ", aktiv,
    'h."', cfg$level$col, '" AS niveau_id, ',
    'n."', cfg$level$num_col, '" AS niveau_num, ',
    'n."', cfg$level$name_col, '" AS niveau_navn ',
    'FROM "', cfg$table, '" h ',
    'LEFT JOIN "', cfg$level$parent, '" n ON n."', cfg$level$parent_pk,
    '" = h."', cfg$level$col, '"'
  )
}

#' @noRd
build_hierarchy_insert_sql <- function(cfg) {
  cols <- hierarchy_edit_cols(cfg)
  ph <- paste(sprintf("$%d", seq_along(cols)), collapse = ", ")
  qcols <- paste(sprintf('"%s"', cols), collapse = ", ")
  sprintf(
    'INSERT INTO "%s" (%s) VALUES (%s) RETURNING "%s"',
    cfg$table, qcols, ph, cfg$pk
  )
}

#' @noRd
build_hierarchy_update_sql <- function(cfg) {
  cols <- hierarchy_edit_cols(cfg)
  sets <- vapply(
    seq_along(cols),
    function(i) sprintf('"%s" = $%d', cols[i], i), ""
  )
  sprintf(
    'UPDATE "%s" SET %s WHERE "%s" = $%d',
    cfg$table, paste(sets, collapse = ", "), cfg$pk, length(cols) + 1
  )
}

#' @noRd
build_hierarchy_delete_sql <- function(cfg) {
  sprintf('DELETE FROM "%s" WHERE "%s" = $1', cfg$table, cfg$pk)
}

#' Antal boern (slet-guard: noder med boern kan ikke slettes)
#' @noRd
build_hierarchy_child_count_sql <- function(cfg) {
  sprintf(
    'SELECT count(*) AS n FROM "%s" WHERE "%s" = $1',
    cfg$table, cfg$parent_col
  )
}
