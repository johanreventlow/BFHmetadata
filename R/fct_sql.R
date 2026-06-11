# Rene SQL-byggere for tblIndikatorer. Identifiers double-quotes (bevar casing+æøå).
# Bruger $n-placeholders (RPostgres parametrisering) → ingen SQL-injection.

#' @noRd
.fk_fields <- function() Filter(function(f) f$kind == "fk", INDIKATOR_FIELDS)

#' SELECT med FK-labels (LEFT JOIN så NULL-FK bevares)
#' @noRd
build_list_sql <- function() {
  base_cols <- vapply(INDIKATOR_FIELDS, function(f) sprintf('i."%s"', f$col), "")
  joins <- character(0); labels <- character(0)
  for (f in .fk_fields()) {
    al <- paste0("p_", f$col)
    labels <- c(labels, sprintf('(%s) AS "label_%s"',
                  gsub("([a-zæøå_]+)", sprintf('%s.\\1', al), f$label, perl = TRUE), f$col))
    joins  <- c(joins, sprintf('LEFT JOIN "%s" %s ON %s."Id" = i."%s"',
                  f$parent, al, al, f$col))
  }
  # Datapakke = forælder-hierarki til datasæt-hierarkiet (selv-join på parent_id).
  # Afhænger af alias p_indikator_hierarki fra loopet (FK-felt i v1).
  joins  <- c(joins, paste0('LEFT JOIN "tblIndikatorHierarki" dp ',
                'ON dp."Id" = p_indikator_hierarki."parent_id"'))
  labels <- c(labels, 'dp."hierarki_navn" AS "label_datapakke"')
  sprintf('SELECT %s, %s FROM "tblIndikatorer" i %s ORDER BY i."id"',
          paste(base_cols, collapse = ", "), paste(labels, collapse = ", "),
          paste(joins, collapse = " "))
}

#' id + label for FK-dropdown
#' @noRd
build_fk_options_sql <- function(parent, label_expr) {
  sprintf('SELECT "Id" AS id, (%s) AS label FROM "%s" ORDER BY 2', label_expr, parent)
}

#' Parametriseret UPDATE; cols → $1..$n, id → $(n+1)
#' @noRd
build_update_sql <- function(cols) {
  sets <- vapply(seq_along(cols), function(i) sprintf('"%s" = $%d', cols[i], i), "")
  sprintf('UPDATE "tblIndikatorer" SET %s WHERE "id" = $%d',
          paste(sets, collapse = ", "), length(cols) + 1)
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
  sprintf('INSERT INTO "%s" ("indikator_id", "%s") VALUES %s',
          j$table, j$fk, paste(vals, collapse = ", "))
}

#' id + tekst-label for m2m-multiselect
#' OBS: j$label er et betroet SQL-fragment fra INDIKATOR_JUNCTIONS (metadata.R),
#' aldrig bruger-input. Interpoleres bevidst direkte (kan ej parametriseres).
#' @noRd
build_junction_options_sql <- function(j) {
  sprintf('SELECT "%s" AS id, (%s) AS label FROM "%s" ORDER BY 2',
          j$parent_pk, j$label, j$parent)
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
#' datapakke = forælder-hierarki (h.parent_id → dp). Org-niveauer (overafdeling=5/
#' afdeling=6/afsnit=7) resolves via rekursiv ancestry (selv + forældre op ad
#' parent_Id-træet) → fremtidssikret når diagrammer opstår på dybere niveauer.
#' diagram_type=1 (Seriediagram) + diagram_aktivt.
#' @noRd
build_diagram_index_sql <- function() {
  paste0(
    'WITH RECURSIVE anc AS (',
    ' SELECT "Id" AS start_id, "parent_Id", "organisatorisk_niveau", "organisatorisk_navn_langt"',
    ' FROM "tblOrganisationStruktur"',
    ' UNION ALL',
    ' SELECT a.start_id, p."parent_Id", p."organisatorisk_niveau", p."organisatorisk_navn_langt"',
    ' FROM anc a JOIN "tblOrganisationStruktur" p ON p."Id" = a."parent_Id"',
    '), lvl AS (',
    ' SELECT start_id,',
    ' max("organisatorisk_navn_langt") FILTER (WHERE "organisatorisk_niveau" = 5) AS overafdeling,',
    ' max("organisatorisk_navn_langt") FILTER (WHERE "organisatorisk_niveau" = 6) AS afdeling,',
    ' max("organisatorisk_navn_langt") FILTER (WHERE "organisatorisk_niveau" = 7) AS afsnit',
    ' FROM anc GROUP BY start_id',
    ') ',
    'SELECT d."id" AS diagram_id, ',
    'i."id" AS indikator_id, i."indikator_navn", i."indikator_navn_teknisk", ',
    'h."hierarki_navn" AS datasaet, dp."hierarki_navn" AS datapakke, ',
    'o."Id" AS org_id, o."organisatorisk_navn_teknisk" AS org_teknisk, ',
    'o."organisatorisk_navn_langt" AS org_navn, o."organisatorisk_niveau" AS org_niveau, ',
    'lvl.overafdeling, lvl.afdeling, lvl.afsnit ',
    'FROM "tblDiagrammer" d ',
    'JOIN "tblIndikatorer" i ON i."id" = d."indikator" ',
    'LEFT JOIN "tblIndikatorHierarki" h ON h."Id" = i."indikator_hierarki" ',
    'LEFT JOIN "tblIndikatorHierarki" dp ON dp."Id" = h."parent_id" ',
    'LEFT JOIN "tblOrganisationStruktur" o ON o."Id" = d."organisatorisk_navn_teknisk" ',
    'LEFT JOIN lvl ON lvl.start_id = o."Id" ',
    'WHERE d."diagram_type" = 1 AND d."diagram_aktivt" ',
    'ORDER BY i."indikator_navn", o."organisatorisk_navn_langt"')
}

#' @noRd
build_median_list_sql <- function() {
  'SELECT * FROM "tblDiagrammerMedian" WHERE "diagram" = $1 ORDER BY "laas_median"'
}

#' @noRd
build_median_insert_sql <- function() {
  'INSERT INTO "tblDiagrammerMedian" ("diagram", "laas_median") VALUES ($1, $2) RETURNING "id"'
}

#' @noRd
build_median_delete_sql <- function() {
  'DELETE FROM "tblDiagrammerMedian" WHERE "id" = $1'
}
