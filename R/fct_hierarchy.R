# Rene trae-funktioner for hierarki-tabeller (org-struktur, indikator-
# hierarki). Ingen DB-afhaengighed — fuldt unit-testbare.

#' Depth-first trae-orden med dybde-kolonne. Multi-rod + orphan-tolerant +
#' cyklus-sikker. Orphans (parent findes ej i df) behandles som roedder;
#' noder i rene cykler appendes fladt sidst (depth 0) frem for at tabes.
#' @noRd
hierarchy_order <- function(df, pk, parent_col, sort_col = NULL) {
  df$depth <- integer(nrow(df))
  if (nrow(df) == 0) return(df)
  ids <- df[[pk]]
  parents <- df[[parent_col]]
  is_na_parent <- is.na(parents)
  is_orphan <- !is_na_parent & !(parents %in% ids)
  is_root <- is_na_parent | is_orphan
  kids_of <- split(which(!is_root), as.character(parents[!is_root]))
  order_idx <- integer(0); depths <- integer(0)
  visited <- rep(FALSE, nrow(df))
  visit <- function(i, depth) {
    if (visited[i]) return()               # cyklus-vaern
    visited[i] <<- TRUE
    order_idx <<- c(order_idx, i); depths <<- c(depths, depth)
    kids <- kids_of[[as.character(ids[i])]]
    if (!is.null(sort_col) && length(kids) > 1)
      kids <- kids[order(df[[sort_col]][kids])]
    for (k in kids) visit(k, depth + 1L)
  }
  true_roots <- which(is_na_parent)
  if (!is.null(sort_col) && length(true_roots) > 1)
    true_roots <- true_roots[order(df[[sort_col]][true_roots])]
  orphans <- which(is_orphan)
  if (!is.null(sort_col) && length(orphans) > 1)
    orphans <- orphans[order(df[[sort_col]][orphans])]
  roots <- c(true_roots, orphans)
  for (r in roots) visit(r, 0L)
  leftover <- which(!visited)              # rene cykler — tab dem ikke
  out <- df[c(order_idx, leftover), , drop = FALSE]
  out$depth <- c(depths, integer(length(leftover)))
  out
}

#' Alle ids i subtree under id — INKLUSIV id selv. Bruges til at ekskludere
#' egen subtree fra foraelder-dropdown (cyklus-lag 1) og som server-side
#' assert foer flyt (cyklus-lag 2).
#' @noRd
hierarchy_descendants <- function(df, pk, parent_col, id) {
  ids <- df[[pk]]; parents <- df[[parent_col]]
  res <- id; frontier <- id
  repeat {
    kids <- ids[!is.na(parents) & parents %in% frontier & !(ids %in% res)]
    if (length(kids) == 0) break
    res <- c(res, kids); frontier <- kids
  }
  res
}
