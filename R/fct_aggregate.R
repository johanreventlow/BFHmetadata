# Vendored fra BFHddl (R/db_organisations.R + R/data_loader.R, v0.6.0):
# hierarki-oprulning til signal-gennemgang. Semantik pinnet af
# tests/testthat/test-aggregate.R — aendres oprulningen i BFHddl, skal
# denne fil OG testene synkroniseres med kilden.

# Har denne gren mindst een efterkommer (eller noden selv) med
# indgaar == TRUE for indikatoren?
#
# Bruges til gennemfald: et uregistreret mellemniveau maa kun traverseres
# igennem, hvis nogen laengere nede er markeret bevidst. Dermed kan data
# aldrig havne i en total uden at vaere flagget et sted i kaeden.
#
# Eksplicit FALSE (eller NA) paa en node stopper grenen med det samme -
# det er maaden man bevidst ekskluderer et helt undertrae.
#
# @noRd
.has_flagged_descendant <- function(org_id, indikator_id, org_struct,
                                     agg_flags, max_depth,
                                     .visited = integer(0)) {
  if (max_depth <= 0L || org_id %in% .visited) {
    return(FALSE)
  }

  rows <- agg_flags[
    agg_flags$org_id %in% org_id & agg_flags$indikator_id %in% indikator_id, ,
    drop = FALSE
  ]

  if (nrow(rows) > 0) {
    if (any(rows$indgaar %in% TRUE)) {
      return(TRUE)
    }
    # Eksplicit registreret uden TRUE (dvs. FALSE/NA) -> gren ekskluderet
    return(FALSE)
  }

  # Uregistreret node - kig videre ned
  child_ids <- org_struct$id[
    !is.na(org_struct$parent_id) & org_struct$parent_id == org_id
  ]
  if (length(child_ids) == 0) {
    return(FALSE)
  }

  for (cid in child_ids) {
    if (.has_flagged_descendant(cid, indikator_id, org_struct, agg_flags,
      max_depth = max_depth - 1L, .visited = c(.visited, org_id)
    )) {
      return(TRUE)
    }
  }
  FALSE
}

# Find aggregat-boern for et center og en indikator
#
# Bestemmer hvilke underafdelinger der skal summes op til et centerniveau
# for en given indikator. Et strukturelt barn (parent_id == center_org_id)
# klassificeres saadan:
# - Flagget: har en raekke i agg_flags for indikatoren med
#   indgaar == TRUE -> bidrager med sine egne data (grenen terminerer her,
#   ingen dobbelttaelling af efterkommere).
# - Eksplicit ekskluderet: har en raekke uden TRUE (dvs. FALSE/NA) ->
#   hele grenen udelades.
# - Uregistreret: har ingen raekke for indikatoren -> grenen traverseres
#   igennem (gennemfald), men kun hvis der laengere nede findes mindst
#   een node med indgaar == TRUE (depth-begraenset af max_depth).
#
# @param center_org_id Numerisk org-id for centret.
# @param indikator_id Indikator-id.
# @param org_struct Data frame med mindst id + parent_id (hele org-traeet).
# @param agg_flags Data frame med mindst org_id, indikator_id, indgaar
#   (en raekke pr. diagram-raekke i tblDiagrammer).
# @param max_depth Maks. antal gennemfald-niveauer der traverseres ned i
#   soegningen efter en flagget efterkommer. Default 5L.
#
# @return Integer-vektor af bidragende org-id'er.
# @noRd
find_aggregation_children <- function(center_org_id, indikator_id,
                                       org_struct, agg_flags,
                                       max_depth = 5L) {
  # Strukturelle boern
  child_ids <- org_struct$id[
    !is.na(org_struct$parent_id) & org_struct$parent_id == center_org_id
  ]
  if (length(child_ids) == 0) {
    return(integer(0))
  }

  rows_for_ind <- agg_flags[
    agg_flags$indikator_id %in% indikator_id, ,
    drop = FALSE
  ]

  agg_ids <- Filter(function(cid) {
    child_rows <- rows_for_ind[rows_for_ind$org_id %in% cid, , drop = FALSE]

    if (nrow(child_rows) > 0) {
      # Registreret: kun med hvis flaget er TRUE
      return(any(child_rows$indgaar %in% TRUE))
    }

    # Uregistreret mellemniveau: gennemfald kun hvis nogen laengere nede
    # er bevidst markeret
    .has_flagged_descendant(cid, indikator_id, org_struct, agg_flags,
      max_depth = max_depth
    )
  }, child_ids)

  as.integer(unique(agg_ids))
}

# Aggreger boerne-data til centerniveau (ren kerne)
#
# Summerer taeller (og naevner, hvis kolonnen findes) per dato paa tvaers
# af allerede-loadede boerne-raekker. Bruges til at beregne et
# organisatorisk centerniveau der ikke har egne raekker i raadata, men
# hvis underafdelinger har.
#
# na.rm = FALSE bevarer NA ved summering, saa et barn med manglende
# vaerdi ikke stiltiende taelles som 0.
#
# naevner medtages KUN hvis kolonnen findes i input - den maa ikke
# opfindes her, da rene taelle-serier ikke har naevner.
#
# @param child_data Data frame med raekker fra alle bidragende boern,
#   mindst kolonnerne dato, taeller (og evt. naevner).
# @param center_enhed Centrets navn der saettes i enhed-kolonnen.
# @param date_col Navn paa dato-kolonnen. Default "dato".
#
# @return Data frame med een raekke pr. dato (enhed = center_enhed).
# @noRd
aggregate_child_data <- function(child_data, center_enhed, date_col = "dato") {
  har_naevner <- "naevner" %in% names(child_data)

  aggregated <- child_data |>
    dplyr::group_by(.data[[date_col]]) |>
    dplyr::summarise(
      taeller = sum(.data$taeller, na.rm = FALSE),
      naevner = if (har_naevner) sum(.data$naevner, na.rm = FALSE) else NA_real_,
      .groups = "drop"
    )

  aggregated$enhed <- center_enhed

  keep <- c(date_col, "enhed", "taeller", if (har_naevner) "naevner")
  dplyr::select(aggregated, dplyr::all_of(keep))
}
