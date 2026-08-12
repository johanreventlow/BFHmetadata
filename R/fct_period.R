# Vendored fra BFHddl (data_loader.R + pipeline.R): periode-aggregering.
#
# Signal-gennemgangen SKAL beregne signaler på samme serie som BFHddl tegner.
# Uden dette beregnes Anhøj-signaler på rå dagsdata, mens diagrammet vises
# uge-/månedsaggregeret — målt på produktionsdata gav det forskelligt signal
# for 12 af 39 enheder (31 %), i begge retninger.
#
# Adfærden skal være identisk med BFHddl's, på samme måde som
# source_fingerprint() er vendoret tegn-for-tegn: afviger de to, ændres
# signalerne tavst. Ret ALDRIG kun den ene side.

#' Oversæt dansk periode-værdi (tblDiagrammer.periode_aggregering) til
#' lubridate-enhed. Tom/NA → "day" (ingen aggregering).
#'
#' Ukendte værdier passerer uændret igennem, så de fejler højlydt i
#' aggregate_to_period() i stedet for stiltiende at blive til "day" — en
#' tastefejl i Access må ikke give uaggregerede signaler uden fejlmelding.
#' @noRd
period_to_en <- function(x) {
  if (is.null(x) || length(x) == 0L) return("day")
  x <- x[[1]]
  if (is.na(x) || !nzchar(trimws(x))) return("day")
  # tolower på "år" er encoding-følsom på Windows → trim + normalisér defensivt
  key <- tolower(trimws(as.character(x)))
  switch(key,
    "dag" = "day",
    "uge" = "week",
    "maaned" = "month",
    "måned" = "month",
    "kvartal" = "quarter",
    "aar" = "year",
    "år" = "year",
    key)
}

#' Frasortér median-knæk sat under en ANDEN aggregering end den serien
#' beregnes på.
#'
#' Et knæk er gemt som en dato, men betyder en rækkeposition. Ved skift af
#' periode_aggregering ville et gammelt knæk lande et andet sted i serien —
#' fx flytter 2025-03-17 sig til 2025-04-01 under måneds-aggregering, fordi
#' ingen bucket starter midt i måneden. Frem for at beregne en fase på en
#' grænse der aldrig var tilsigtet, ignoreres knækket (og vises som ignoreret
#' i UI'et, så faserne ikke ændrer sig usynligt).
#'
#' NULL/tom aggregering regnes som match (bagudkompatibelt: knæk fra før
#' kolonnen fandtes — efter backfill'en findes de ikke i praksis).
#'
#' @param medians_df df med evt. kolonnen `aggregering` (dansk værdi)
#' @param period_da diagrammets aktuelle periode_aggregering (dansk værdi)
#' @return samme df uden de knæk hvis aggregering afviger
#' @noRd
filter_medians_by_period <- function(medians_df, period_da) {
  if (is.null(medians_df) || !is.data.frame(medians_df) ||
      nrow(medians_df) == 0 || !"aggregering" %in% names(medians_df)) {
    return(medians_df)
  }
  # Sammenlign normaliseret (lubridate-enhed), så "maaned"/"måned" er samme.
  want <- period_to_en(period_da)
  have <- vapply(medians_df$aggregering, period_to_en, "")
  unknown <- is.na(medians_df$aggregering) |
    !nzchar(trimws(as.character(medians_df$aggregering)))
  medians_df[unknown | have == want, , drop = FALSE]
}

#' Aggregér dags-data til uge/måned/kvartal/år ved at summere taeller+naevner
#' pr. periode-bucket.
#'
#' Grupperer på alle kolonner undtagen dato/taeller/naevner, så `enhed`,
#' `indikator` m.fl. bevares — enheder må ALDRIG blandes sammen.
#'
#' @param data df med mindst taeller + en dato-kolonne (naevner valgfri)
#' @param period "day" (no-op), "week", "month", "quarter" el. "year"
#' @param date_col navn på dato-kolonnen
#' @param drop_incomplete fjern den igangværende (ufuldstændige) bucket?
#'   Uden trim ville fx en års-bucket i juli kun rumme 7 måneders data, men
#'   vises som et helt år → ligner et kunstigt fald i SPC-diagrammet.
#' @param today reference-dato (findes af testbarhedshensyn — drop_incomplete
#'   afhænger af dagens dato, så tests skal kunne sætte den eksplicit)
#' @noRd
aggregate_to_period <- function(data, period = c("day", "week", "month", "quarter", "year"),
                                date_col = "dato", drop_incomplete = TRUE,
                                today = Sys.Date()) {
  period <- match.arg(period)

  if (period == "day") {
    return(data)
  }

  if (!date_col %in% names(data)) {
    stop(sprintf("Data mangler dato-kolonnen '%s'", date_col), call. = FALSE)
  }

  # Værn (BFHmetadata-specifikt, findes ikke i BFHddl): kun taeller/naevner
  # summeres, så en vaerdi-kolonne ville havne i group_cols og gøre
  # aggregeringen til en TAVS no-op — serien ville se uaggregeret ud uden fejl.
  if ("vaerdi" %in% names(data)) {
    stop("Slice har en 'vaerdi'-kolonne, som ikke kan periode-aggregeres ",
         "(kun taeller/naevner summeres). Indikatoren skal enten lagres med ",
         "taeller/naevner eller vises uaggregeret.", call. = FALSE)
  }

  group_cols <- setdiff(names(data), c(date_col, "taeller", "naevner"))
  has_naevner <- "naevner" %in% names(data)
  week_start <- getOption("lubridate.week.start", 1)

  aggregated <- data |>
    dplyr::mutate(
      .period_bucket = lubridate::floor_date(
        .data[[date_col]],
        unit = period,
        week_start = week_start
      )
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, ".period_bucket")))) |>
    dplyr::summarise(
      # na.rm = FALSE er bevidst: et manglende barn må ikke tælle som 0.
      taeller = sum(.data$taeller, na.rm = FALSE),
      naevner = if (has_naevner) sum(.data$naevner, na.rm = FALSE) else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::rename(!!date_col := ".period_bucket")

  if (!has_naevner) {
    aggregated$naevner <- NULL
  }

  if (drop_incomplete) {
    current_bucket_start <- lubridate::floor_date(today, unit = period,
                                                 week_start = week_start)
    aggregated <- aggregated |>
      dplyr::filter(.data[[date_col]] < current_bucket_start)
  }

  aggregated
}
