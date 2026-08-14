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

#' Hierarkisk ordnede org-choices til dropdown: named character vector
#' (indrykket label -> id som character), depth-first. tree = df med
#' id/parent_id (db$org_struct()); labels = df med id/label. NULL/tomt trae
#' degraderer til flad alfabetisk liste; enheder med label men uden
#' traerække appendes fladt sidst (tabes ikke).
#' @noRd
org_hierarchy_choices <- function(tree, labels) {
  if (is.null(labels) || nrow(labels) == 0) {
    return(character(0))
  }
  labels$label <- as.character(labels$label)
  flat <- function(d) {
    d <- d[order(d$label), , drop = FALSE]
    stats::setNames(as.character(d$id), d$label)
  }
  if (is.null(tree) || !is.data.frame(tree) || nrow(tree) == 0) {
    return(flat(labels))
  }
  df <- data.frame(id = tree$id, parent_id = tree$parent_id)
  df$label <- labels$label[match(df$id, labels$id)]
  # Traerækker uden label (burde ikke ske): vis id frem for at fejle
  df$label[is.na(df$label)] <- as.character(df$id[is.na(df$label)])
  ord <- hierarchy_order(df, "id", "parent_id", sort_col = "label")
  # Non-breaking spaces: alm. mellemrum kollapses af browseren i <option>
  indent <- strrep("  ", ord$depth)
  out <- stats::setNames(as.character(ord$id), paste0(indent, ord$label))
  missing <- labels[!(labels$id %in% df$id), , drop = FALSE]
  if (nrow(missing) > 0) out <- c(out, flat(missing))
  out
}

#' Valgt org-enheds id + alle underliggende enheders ids. Uden trae (NULL/
#' tomt, fx fejlet DB-hentning) degraderer filteret til kun enheden selv.
#' @noRd
org_subtree_ids <- function(tree, id) {
  if (is.null(tree) || !is.data.frame(tree) || nrow(tree) == 0) {
    return(id)
  }
  hierarchy_descendants(tree, "id", "parent_id", id)
}

#' Beskaer et depth-first-ordnet trae (hierarchy_order-output) til grenen
#' under id — inklusiv noden selv — og renormalisér depth saa grenens rod
#' vises uindrykket. Raekkeorden bevares (subtree-raekker er kontiguøse).
#' @noRd
hierarchy_subtree <- function(tree_df, pk, parent_col, id) {
  keep <- hierarchy_descendants(tree_df, pk, parent_col, id)
  out <- tree_df[tree_df[[pk]] %in% keep, , drop = FALSE]
  if ("depth" %in% names(out) && nrow(out) > 0) {
    base <- out$depth[out[[pk]] == id][1]
    if (!is.na(base)) out$depth <- pmax(out$depth - base, 0L)
  }
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
