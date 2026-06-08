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
