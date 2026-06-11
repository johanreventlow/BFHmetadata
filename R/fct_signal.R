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

#' Beregn run chart + Anhøj-signal for ét diagram-slice.
#' Alle faser beregnes (historik); signal-flag = seneste fase (max fase).
#' @param slice data.frame med dato/vaerdi (+ evt. taeller/naevner)
#' @param parts integer-vektor af part-positioner (fra resolve_median_breaks) el. NULL
#' @return list(signal, latest, summary_all, qic_result)
#' @noRd
compute_signal <- function(slice, parts = NULL) {
  has_n <- "naevner" %in% names(slice) && any(!is.na(slice$naevner))
  res <- if (has_n)
    BFHcharts::bfh_qic(slice, x = dato, y = taeller, n = naevner,
                       chart_type = "run", part = parts, multiply = 100)
  else
    BFHcharts::bfh_qic(slice, x = dato, y = vaerdi, chart_type = "run", part = parts)
  s <- res$summary
  latest <- s[s$fase == max(s$fase), , drop = FALSE]
  list(
    signal = isTRUE(latest$anhoej_signal[1]),
    latest = latest,
    summary_all = s,
    qic_result = res
  )
}
