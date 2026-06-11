# Vendored fra BFHddl (target_parsing.R): median-knæk-datoer → bfh_qic part-positioner.

#' Konverter tblDiagrammerMedian-datoer for ét diagram til række-positioner
#' (bfh_qic part=...). Knæk på første/sidste række eller uden for data droppes.
#' @noRd
resolve_median_breaks <- function(diagram_id, all_medians, x_dates) {
  if (is.null(all_medians) || !is.data.frame(all_medians) ||
      nrow(all_medians) == 0 || !"diagram" %in% names(all_medians)) return(NULL)
  rows <- all_medians[all_medians$diagram == diagram_id, , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)
  date_col <- intersect(names(rows),
    c("laas_median", "median_dato", "dato", "knaek_dato", "break_date"))
  if (length(date_col) == 0) return(NULL)
  bd <- sort(as.Date(rows[[date_col[1]]]))
  bd <- bd[!is.na(bd)]
  if (length(bd) == 0) return(NULL)
  x <- sort(unique(as.Date(x_dates)))
  pos <- integer(0)
  for (b in bd) {
    # part-position = FØRSTE række på/efter knæk-dato (BFHddl/qicharts2-konvention:
    # ny fase starter ved første observation >= laas_median). Drop knæk på
    # første række (rp>1) eller uden for data.
    p <- which(x >= b)
    if (length(p) > 0) {
      rp <- min(p)
      if (rp > 1 && rp <= length(x)) pos <- c(pos, rp)
    }
  }
  pos <- sort(unique(pos))
  if (length(pos) == 0) NULL else pos
}
