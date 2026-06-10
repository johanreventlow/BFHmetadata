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
