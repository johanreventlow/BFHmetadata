# Headless scan-lag for signal-gennemgang. Sidder oven på Fase A-motoren
# (parquet/signal) + DB-accessors. Ingen Shiny-state → ren + testbar.

#' Byg parquet-enhed-filter (lowercase varianter) for ét org_id ud fra
#' org_enhed_variants()-df (org-navne + tblOrganisationOversaettelse-fra-data).
#' @noRd
enhed_variants_for <- function(variants_df, org_id) {
  if (is.null(variants_df) || nrow(variants_df) == 0) return(character(0))
  rows <- variants_df[variants_df$org_id == org_id, , drop = FALSE]
  if (nrow(rows) == 0) return(character(0))
  # teknisk/kort/langt kommer fra org-siden af LEFT JOIN → identiske for alle
  # rækker af samme org_id; derfor [1]. fra_data varierer pr. oversættelse.
  v <- c(rows$fra_data, rows$teknisk[1], rows$kort[1], rows$langt[1])
  v <- tolower(v[!is.na(v) & nzchar(v)])
  unique(v)
}

#' Scan ét diagram: byg enhed-filter → load parquet-slice → (vindue) → resolve
#' median-knæk → compute_signal. Fanger fejl pr. diagram (safe_operation).
#' @param row liste/df-række med indikator_navn_teknisk, org_id, diagram_id
#' @param base_path bruger-valgt parquet-rodmappe
#' @param medians_df alle median-rækker for diagrammet (kolonner diagram, laas_median) el. NULL
#' @param variants_df org_enhed_variants()-output
#' @param window_n behold seneste N observationer (NULL = alle)
#' @return list(diagram_id, status, signal, n_obs, slice, qic_result, summary)
#' @noRd
scan_diagram <- function(row, base_path, medians_df, variants_df, window_n = NULL) {
  empty <- function(status) list(diagram_id = row$diagram_id, status = status,
    signal = FALSE, n_obs = 0L, slice = NULL, qic_result = NULL, summary = NULL)
  # Værdi-givende if/else (ingen non-local return ud af safe_operation-blokken):
  # blokkens sidste udtryk er resultatet → fallback="fejl" rammes kun ved fejl.
  safe_operation(sprintf("scan diagram %s", row$diagram_id), {
    variants <- enhed_variants_for(variants_df, row$org_id)
    if (length(variants) == 0) {
      # Seriediagrammer er org-scopede: uden enhed-varianter kan slicet ikke
      # afgrænses til rette enhed → "ingen_data" frem for signal på blandede
      # enheder. (Rigtige org'er har altid navne → rammer ej normal-flow.)
      empty("ingen_data")
    } else {
      path <- parquet_indicator_path(base_path, row$indikator_navn_teknisk)
      slice <- parquet_load_slice(path, enhed = variants)
      if (is.null(slice) || nrow(slice) == 0) {
        empty("ingen_data")
      } else {
        if (!is.null(window_n)) slice <- parquet_limit_observations(slice, window_n)
        slice <- slice[order(slice$dato), , drop = FALSE]
        parts <- resolve_median_breaks(row$diagram_id, medians_df, slice$dato)
        sig <- compute_signal(slice, parts = parts)
        list(diagram_id = row$diagram_id, status = "ok", signal = isTRUE(sig$signal),
             n_obs = length(unique(as.Date(slice$dato))), slice = slice,
             qic_result = sig$qic_result, summary = sig$summary_all)
      }
    }
  }, fallback = empty("fejl"))
}

# De 5 filter-dimensioner (kolonnenavne i diagram-indekset).
.SIGNAL_FILTER_DIMS <- c("overafdeling", "afsnit", "datapakke",
                         "datasaet", "indikator_navn")

#' Sorterede unikke valg pr. filter-dimension (NA/tomme droppes).
#' @noRd
index_filter_choices <- function(index) {
  stats::setNames(lapply(.SIGNAL_FILTER_DIMS, function(col) {
    v <- index[[col]]
    v <- v[!is.na(v) & nzchar(v)]
    sort(unique(v))
  }), .SIGNAL_FILTER_DIMS)
}

#' Subset diagram-indeks på et named filter (AND). Tomme/NULL-værdier ignoreres.
#' @noRd
apply_index_filters <- function(index, filters) {
  keep <- rep(TRUE, nrow(index))
  for (col in names(filters)) {
    val <- filters[[col]]
    # length-0-guard FØR nzchar: tom character(0) (fx multi-select) → spring over
    if (is.null(val) || length(val) == 0L || !nzchar(val) || !col %in% names(index)) next
    keep <- keep & !is.na(index[[col]]) & index[[col]] == val
  }
  index[keep, , drop = FALSE]
}
