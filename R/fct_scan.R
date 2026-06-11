# Headless scan-lag for signal-gennemgang. Sidder oven på Fase A-motoren
# (parquet/signal) + DB-accessors. Ingen Shiny-state → ren + testbar.

#' Byg parquet-enhed-filter (lowercase varianter) for ét org_id ud fra
#' org_enhed_variants()-df (org-navne + tblOrganisationOversaettelse-fra-data).
#' @noRd
enhed_variants_for <- function(variants_df, org_id) {
  if (is.null(variants_df) || nrow(variants_df) == 0) return(character(0))
  rows <- variants_df[variants_df$org_id == org_id, , drop = FALSE]
  if (nrow(rows) == 0) return(character(0))
  v <- c(rows$fra_data, rows$teknisk[1], rows$kort[1], rows$langt[1])
  v <- tolower(v[!is.na(v) & nzchar(v)])
  unique(v)
}
