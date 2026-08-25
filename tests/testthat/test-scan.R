test_that("enhed_variants_for: dedup + lowercase + drop tomme/NA", {
  vdf <- data.frame(
    org_id  = c(12L, 12L, 12L, 99L),
    teknisk = c("Y Hjerte", "Y Hjerte", "Y Hjerte", "Z"),
    kort    = c("YHJ", "YHJ", "YHJ", NA),
    langt   = c("Y Hjerteafdeling", "Y Hjerteafdeling", "Y Hjerteafdeling", ""),
    fra_data = c("HJERTE", "Y HJ", NA, NA),
    stringsAsFactors = FALSE)
  v <- enhed_variants_for(vdf, 12L)
  expect_true(all(v == tolower(v)))                    # alt lowercase
  expect_true(all(c("hjerte", "y hj", "y hjerte", "yhj",
                    "y hjerteafdeling") %in% v))
  expect_false(any(is.na(v) | v == ""))                # ingen tomme/NA
  expect_equal(length(v), length(unique(v)))           # dedup
})

test_that("enhed_variants_for: ukendt org_id → character(0)", {
  vdf <- data.frame(org_id = 1L, teknisk = "a", kort = "b", langt = "c",
                    fra_data = NA, stringsAsFactors = FALSE)
  expect_equal(enhed_variants_for(vdf, 777L), character(0))
})

test_that("enhed_variants_for: NULL df → character(0)", {
  expect_equal(enhed_variants_for(NULL, 1L), character(0))
})

test_that("scan_diagram: fixture-parquet med langt løb → signal=TRUE", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "test_ind"
  dir.create(file.path(base, ind))
  df <- data.frame(
    dato = as.Date("2020-01-01") + 0:23 * 30,
    vaerdi = c(rep(10, 12), rep(2, 12)),
    taeller = NA_real_, naevner = NA_real_,
    enhed = "afd x", stringsAsFactors = FALSE)
  arrow::write_parquet(df, file.path(base, ind, "part-0.parquet"))

  row <- list(diagram_id = 1L, indikator_navn_teknisk = ind, org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "Afd X", kort = NA,
                    langt = NA, fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, base, medians_df = NULL, variants_df = vdf)

  expect_equal(res$status, "ok")
  expect_true(res$signal)
  expect_equal(res$n_obs, 24L)
  expect_s3_class(res$qic_result, "bfh_qic_result")
})

test_that("scan_diagram: manglende mappe → status 'ingen_data', intet hårdt fald", {
  base <- withr::local_tempdir()
  row <- list(diagram_id = 2L, indikator_navn_teknisk = "findes_ikke", org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "x", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, base, medians_df = NULL, variants_df = vdf)
  expect_equal(res$status, "ingen_data")
  expect_false(res$signal)
})

test_that("scan_diagram: window_n begrænser til seneste N observationer", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "win_ind"; dir.create(file.path(base, ind))
  df <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                   vaerdi = c(rep(10, 12), rep(2, 12)),
                   taeller = NA_real_, naevner = NA_real_, enhed = "e")
  arrow::write_parquet(df, file.path(base, ind, "p.parquet"))
  row <- list(diagram_id = 3L, indikator_navn_teknisk = ind, org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "E", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, base, medians_df = NULL, variants_df = vdf, window_n = 6L)
  expect_equal(res$n_obs, 6L)
})

test_that("scan_diagram: median-knæk splitter i faser (parts-stien)", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "med_ind"; dir.create(file.path(base, ind))
  df <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                   vaerdi = c(rep(10, 12), rep(2, 12)),
                   taeller = NA_real_, naevner = NA_real_, enhed = "e")
  arrow::write_parquet(df, file.path(base, ind, "p.parquet"))
  row <- list(diagram_id = 42L, indikator_navn_teknisk = ind, org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "E", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  # Knæk på 13. observation → fase 1 (1:12) + fase 2 (13:24)
  meds <- data.frame(diagram = 42L, laas_median = df$dato[13])
  res <- scan_diagram(row, base, medians_df = meds, variants_df = vdf)
  expect_equal(res$status, "ok")
  expect_equal(length(unique(res$summary$fase)), 2L)
  expect_equal(max(res$summary$fase), 2)
})

test_that("scan_diagram: character-dato i parquet → status ok (ej fejl) + signal", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "txt_ind"; dir.create(file.path(base, ind))
  # Rigtig parquet: dato som tekst. Uden Date-coerce fejler bfh_qic på ALLE diagrammer.
  arrow::write_parquet(data.frame(
    dato = as.character(as.Date("2020-01-01") + 0:23 * 30),
    vaerdi = c(rep(10, 12), rep(2, 12)),
    taeller = NA_real_, naevner = NA_real_, enhed = "e", stringsAsFactors = FALSE),
    file.path(base, ind, "p.parquet"))
  row <- list(diagram_id = 1L, indikator_navn_teknisk = ind, org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "E", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, base, medians_df = NULL, variants_df = vdf)
  expect_equal(res$status, "ok")
  expect_true(res$signal)
})

test_that("preview_break_parts: POSIXct-base (prod) == Date-base → samme positioner", {
  x <- as.Date("2020-01-01") + 0:23 * 30
  # Eksisterende knæk kommer fra DB som POSIXct (UTC); ekstra fra et klik (streng)
  p_posix <- preview_break_parts(1L,
    data.frame(laas_median = as.POSIXct("2020-04-30", tz = "UTC")), "2020-07-29", x)
  p_date <- preview_break_parts(1L,
    data.frame(laas_median = as.Date("2020-04-30")), "2020-07-29", x)
  expect_identical(p_posix, p_date)        # preview konsistent uanset base-type
  expect_equal(p_posix, c(5L, 8L))         # begge knæk → to positioner
})

test_that("preview_break_parts: tom base + ekstra → kun ekstra-knækket", {
  x <- as.Date("2020-01-01") + 0:23 * 30
  parts <- preview_break_parts(1L,
    data.frame(laas_median = as.Date(character(0))), "2020-07-29", x)
  expect_equal(parts, 8L)
})

test_that("scan_diagram: ingen enhed-varianter → ingen_data (ingen blandet-enhed-load)", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "noorg_ind"; dir.create(file.path(base, ind))
  arrow::write_parquet(data.frame(dato = as.Date("2020-01-01") + 0:5 * 30,
    vaerdi = 1:6, taeller = NA_real_, naevner = NA_real_, enhed = "e"),
    file.path(base, ind, "p.parquet"))
  row <- list(diagram_id = 9L, indikator_navn_teknisk = ind, org_id = 5L)
  vdf <- data.frame(org_id = 99L, teknisk = "X", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)  # ingen match på org_id 5
  res <- scan_diagram(row, base, medians_df = NULL, variants_df = vdf)
  expect_equal(res$status, "ingen_data")
})

test_that("slice_filter_enhed: case-insensitive match; NULL ved no-match/tom/manglende kolonne", {
  d <- data.frame(dato = as.Date("2020-01-01") + 0:2, vaerdi = 1:3,
                  enhed = c("Afd X", "AFD X", "Afd Y"), stringsAsFactors = FALSE)
  expect_equal(nrow(slice_filter_enhed(d, "afd x")), 2)
  expect_equal(nrow(slice_filter_enhed(d, c("afd x", "afd y"))), 3)
  expect_null(slice_filter_enhed(d, "ukendt"))
  expect_null(slice_filter_enhed(NULL, "afd x"))
  expect_null(slice_filter_enhed(d[0, ], "afd x"))
  # Uden enhed-kolonne kan slicet ikke afgrænses → NULL (= ingen_data), aldrig
  # signal på blandede enheder
  expect_null(slice_filter_enhed(
    data.frame(dato = as.Date("2020-01-01"), vaerdi = 1), "afd x"))
})

test_that("scan_diagram med slice_loader: bruger preloadet slice, rører ikke disken", {
  # base_path findes ikke → uden loader ville resultatet være ingen_data.
  # Med loader skal parquet-laget slet ikke i spil (per-indikator-genbrug).
  df <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                   vaerdi = c(rep(10, 12), rep(2, 12)),
                   taeller = NA_real_, naevner = NA_real_,
                   enhed = "afd x", stringsAsFactors = FALSE)
  row <- list(diagram_id = 1L, indikator_navn_teknisk = "ligegyldig", org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "Afd X", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, file.path(tempdir(), "findes_ej"), NULL, vdf,
                      slice_loader = function() df)
  expect_equal(res$status, "ok")
  expect_true(res$signal)
  expect_equal(res$n_obs, 24L)
})

test_that("scan_diagram med slice_loader der giver NULL → ingen_data", {
  row <- list(diagram_id = 2L, indikator_navn_teknisk = "x", org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "E", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, tempdir(), NULL, vdf, slice_loader = function() NULL)
  expect_equal(res$status, "ingen_data")
  expect_false(res$signal)
})

test_that("scan_diagram: slice_loader-slice for ANDEN enhed → ingen_data (filter virker)", {
  df <- data.frame(dato = as.Date("2020-01-01") + 0:5 * 30, vaerdi = 1:6,
                   taeller = NA_real_, naevner = NA_real_,
                   enhed = "anden afd", stringsAsFactors = FALSE)
  row <- list(diagram_id = 3L, indikator_navn_teknisk = "x", org_id = 5L)
  vdf <- data.frame(org_id = 5L, teknisk = "Afd X", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  res <- scan_diagram(row, tempdir(), NULL, vdf, slice_loader = function() df)
  expect_equal(res$status, "ingen_data")
})

test_that("medians_by_diagram: split pr. diagram; manglende id → NULL-agtig tom df", {
  m <- data.frame(id = 1:3, diagram = c(7L, 7L, 9L),
                  laas_median = as.Date(c("2020-02-01", "2020-05-01", "2020-03-01")))
  idx <- medians_by_diagram(m, c(7L, 9L, 11L))
  expect_equal(nrow(idx[["7"]]), 2L)
  expect_equal(nrow(idx[["9"]]), 1L)
  # Diagram uden knæk skal give en TOM df (ikke NULL) med diagram-kolonnen
  # intakt, så resolve_median_breaks() rammer sin nrow==0-guard som normalt.
  expect_equal(nrow(idx[["11"]]), 0L)
  expect_true("diagram" %in% names(idx[["11"]]))
})

test_that("medians_by_diagram: tom/NULL input → tomme df'er for alle id'er", {
  idx <- medians_by_diagram(NULL, c(1L, 2L))
  expect_equal(nrow(idx[["1"]]), 0L)
  expect_true("laas_median" %in% names(idx[["1"]]))
  idx2 <- medians_by_diagram(
    data.frame(id = integer(0), diagram = integer(0),
               laas_median = as.Date(character(0))), 5L)
  expect_equal(nrow(idx2[["5"]]), 0L)
})

test_that("medians_by_diagram: resultat virker i resolve_median_breaks (kontrakt)", {
  x <- as.Date("2020-01-01") + 0:23 * 30
  m <- data.frame(id = 1L, diagram = 7L, laas_median = x[13])
  idx <- medians_by_diagram(m, c(7L, 8L))
  expect_equal(resolve_median_breaks(7L, idx[["7"]], x), 13L)
  expect_null(resolve_median_breaks(8L, idx[["8"]], x))   # tom df → ingen knæk
})

test_that("index_filter_choices: sorterede unikke valg pr. dimension (drop NA)", {
  idx <- data.frame(
    overafdeling = c("B", "A", "A", NA),
    afsnit = NA_character_,
    datapakke = c("P", "P", "Q", "P"),
    datasaet = c("d1", "d2", "d1", "d3"),
    indikator_navn = c("i2", "i1", "i1", "i3"),
    stringsAsFactors = FALSE)
  ch <- index_filter_choices(idx)
  expect_equal(ch$overafdeling, c("A", "B"))     # sorteret, NA væk
  expect_equal(ch$afsnit, character(0))           # helt NA → tom
  expect_equal(ch$indikator_navn, c("i1", "i2", "i3"))
})

test_that("apply_index_filters: AND på tværs af dimensioner; tom filter = alt", {
  idx <- data.frame(
    diagram_id = 1:4,
    overafdeling = c("A", "A", "B", "A"),
    afsnit = NA_character_,
    datapakke = c("P", "Q", "P", "P"),
    datasaet = c("d1", "d1", "d1", "d2"),
    indikator_navn = c("i1", "i1", "i1", "i1"),
    stringsAsFactors = FALSE)
  expect_equal(nrow(apply_index_filters(idx, list())), 4)
  r <- apply_index_filters(idx, list(overafdeling = "A", datapakke = "P"))
  expect_equal(r$diagram_id, c(1L, 4L))
  # tom streng/NULL pr. dimension ignoreres
  expect_equal(nrow(apply_index_filters(idx, list(overafdeling = ""))), 4)
})

test_that("apply_index_filters: flere værdier pr. dimension = OR (multi-select)", {
  idx <- data.frame(
    diagram_id = 1:4,
    overafdeling = c("A", "A", "B", NA),
    afsnit = NA_character_,
    datapakke = c("P", "Q", "P", "P"),
    datasaet = c("d1", "d1", "d1", "d2"),
    indikator_navn = c("i1", "i1", "i1", "i1"),
    stringsAsFactors = FALSE)
  # OR inden for dimension: A eller B → rækker 1-3 (NA matcher aldrig)
  r <- apply_index_filters(idx, list(overafdeling = c("A", "B")))
  expect_equal(r$diagram_id, 1:3)
  # AND på tværs af dimensioner bevares med multi-værdier
  r <- apply_index_filters(idx, list(overafdeling = c("A", "B"), datapakke = "P"))
  expect_equal(r$diagram_id, c(1L, 3L))
  # character(0) (multi-select ryddet) og tomme strenge i vektoren ignoreres
  expect_equal(nrow(apply_index_filters(idx, list(overafdeling = character(0)))), 4)
  r <- apply_index_filters(idx, list(overafdeling = c("", "B")))
  expect_equal(r$diagram_id, 3L)
})

# --- Periode-aggregering i scan_diagram -------------------------------------
# Signalet SKAL beregnes på samme serie som BFHddl tegner. Se fct_period.R.

# Daglig serie (2 år) med taeller/naevner — den form uge-indikatorerne har.
make_daily_fixture <- function(base, ind, env = parent.frame()) {
  dir.create(file.path(base, ind), recursive = TRUE, showWarnings = FALSE)
  dates <- seq(as.Date("2024-01-01"), as.Date("2025-12-28"), by = "day")
  df <- data.frame(dato = dates, indikator = ind, enhed = "e",
                   taeller = rep(c(2, 3), length.out = length(dates)),
                   naevner = 10, stringsAsFactors = FALSE)
  arrow::write_parquet(df, file.path(base, ind, "p.parquet"))
  df
}

.scan_row <- function(ind, id = 90L) list(diagram_id = id,
  indikator_navn_teknisk = ind, org_id = 5L)
.scan_vdf <- function() data.frame(org_id = 5L, teknisk = "E", kort = NA,
  langt = NA, fra_data = NA, stringsAsFactors = FALSE)

test_that("scan_diagram: uge-aggregering FØR vindue — spænd, ikke kun antal", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  make_daily_fixture(base, "uge_ind")
  res <- scan_diagram(.scan_row("uge_ind"), base, medians_df = NULL,
                      variants_df = .scan_vdf(), window_n = 24L, period = "week")
  expect_equal(res$status, "ok")
  expect_equal(res$n_obs, 24L)
  # Kernen i regressionen: 24 UGER spænder ~161 dage. Ved forkert rækkefølge
  # (limit før aggregering) ville man få 24 dage → ~4 buckets. At asserte
  # n_obs alene ville bestå i begge tilfælde — derfor testes spændet.
  span <- as.numeric(max(res$slice$dato) - min(res$slice$dato))
  expect_gt(span, 150)
  expect_lt(span, 175)
})

test_that("scan_diagram: uden period er adfærden uændret (rå daglig)", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  make_daily_fixture(base, "raa_ind")
  res <- scan_diagram(.scan_row("raa_ind"), base, medians_df = NULL,
                      variants_df = .scan_vdf(), window_n = 24L)
  expect_equal(res$n_obs, 24L)
  # 24 dage, ikke 24 uger
  expect_equal(as.numeric(max(res$slice$dato) - min(res$slice$dato)), 23)
})

test_that("scan_diagram: period='day' svarer til ingen aggregering", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  make_daily_fixture(base, "dag_ind")
  a <- scan_diagram(.scan_row("dag_ind"), base, NULL, .scan_vdf(),
                    window_n = 24L, period = "day")
  b <- scan_diagram(.scan_row("dag_ind"), base, NULL, .scan_vdf(),
                    window_n = 24L, period = NULL)
  expect_equal(a$n_obs, b$n_obs)
  expect_equal(a$slice$dato, b$slice$dato)
})

test_that("scan_diagram: maaned-aggregering på månedsdata er nær-no-op", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "mdr_ind"; dir.create(file.path(base, ind))
  # Allerede månedslagret + dublerede (enhed, dato) som i virkelige data
  d <- data.frame(
    dato = rep(seq(as.Date("2023-01-01"), by = "month", length.out = 24), each = 2),
    indikator = ind, enhed = "e", taeller = 1, naevner = 5)
  arrow::write_parquet(d, file.path(base, ind, "p.parquet"))
  res <- scan_diagram(.scan_row("mdr_ind"), base, NULL, .scan_vdf(),
                      period = "month")
  expect_equal(res$status, "ok")
  expect_equal(res$n_obs, 24L)      # dubletter kollapser til én pr. måned
  expect_true(all(res$slice$taeller == 2))
})

test_that("scan_diagram: aggregering blander ALDRIG enheder sammen", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "multi_ind"; dir.create(file.path(base, ind))
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-30"), by = "day")
  d <- rbind(
    data.frame(dato = dates, indikator = ind, enhed = "e", taeller = 1, naevner = 10),
    data.frame(dato = dates, indikator = ind, enhed = "anden", taeller = 99, naevner = 10))
  arrow::write_parquet(d, file.path(base, ind, "p.parquet"))
  res <- scan_diagram(.scan_row("multi_ind"), base, NULL, .scan_vdf(),
                      period = "week")
  expect_equal(res$status, "ok")
  # Kun "e" må være med (variant-filteret), og taeller må ikke rumme 99'erne
  expect_true(all(res$slice$enhed == "e"))
  expect_true(all(res$slice$taeller <= 7))
})

test_that("scan_diagram: tomt efter drop_incomplete → ingen_data (ej crash)", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "kun_nu"; dir.create(file.path(base, ind))
  # Eneste data ligger i indeværende måned → aggregeringen tømmer slicet
  d <- data.frame(dato = Sys.Date(), indikator = ind, enhed = "e",
                  taeller = 1, naevner = 5)
  arrow::write_parquet(d, file.path(base, ind, "p.parquet"))
  res <- scan_diagram(.scan_row("kun_nu"), base, NULL, .scan_vdf(),
                      period = "month")
  expect_equal(res$status, "ingen_data")
  expect_false(res$signal)
})

test_that("scan_diagram: vaerdi-kolonne + aggregering → fejl (ej tavs no-op)", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "vaerdi_ind"; dir.create(file.path(base, ind))
  d <- data.frame(dato = seq(as.Date("2024-01-01"), by = "day", length.out = 60),
                  indikator = ind, enhed = "e", vaerdi = 1:60, taeller = 1)
  arrow::write_parquet(d, file.path(base, ind, "p.parquet"))
  res <- scan_diagram(.scan_row("vaerdi_ind"), base, NULL, .scan_vdf(),
                      period = "week")
  expect_equal(res$status, "fejl")   # safe_operation fanger → synlig fejl
})

# --- Nulfyldning af tomme perioder (spejler BFHddl fill_empty_periods) ------

test_that("scan_fill_empty_periods: huller OG hale fyldes med 0 frem til seneste afsluttede periode", {
  s <- data.frame(dato = as.Date(c("2026-01-05", "2026-01-19")),
                  taeller = c(2, 3), stringsAsFactors = FALSE)
  # I dag = onsdag 2026-02-04 → seneste afsluttede uge starter 2026-01-26
  out <- scan_fill_empty_periods(s, "week", today = as.Date("2026-02-04"))
  expect_equal(out$dato, as.Date(c("2026-01-05", "2026-01-12",
                                   "2026-01-19", "2026-01-26")))
  expect_equal(out$taeller, c(2, 0, 3, 0))
})

test_that("scan_fill_empty_periods: naevner-/vaerdi-serier røres ALDRIG", {
  d <- as.Date(c("2026-01-05", "2026-01-19"))
  andel <- data.frame(dato = d, taeller = c(1, 2), naevner = c(10, 10))
  expect_identical(scan_fill_empty_periods(andel, "week",
    today = as.Date("2026-02-04")), andel)
  vaerdi <- data.frame(dato = d, vaerdi = c(4, 6), taeller = NA_real_)
  expect_identical(scan_fill_empty_periods(vaerdi, "week",
    today = as.Date("2026-02-04")), vaerdi)
  # All-NA naevner er reelt fraværende (scan-slices bærer alle kolonner) → fyldes
  count_na_naevner <- data.frame(dato = d, taeller = c(1, 2),
                                 naevner = NA_real_)
  filled <- scan_fill_empty_periods(count_na_naevner, "week",
    today = as.Date("2026-02-04"))
  expect_equal(nrow(filled), 4L)
  expect_true(all(is.na(filled$naevner)))
})

test_that("scan_fill_empty_periods: tom/dato-løs slice returneres uændret", {
  empty <- data.frame(dato = as.Date(character(0)), taeller = numeric(0))
  expect_identical(scan_fill_empty_periods(empty, "week"), empty)
  expect_null(scan_fill_empty_periods(NULL, "week"))
})

test_that("scan_diagram: nulfyld-flag udfylder tomme perioder før signal", {
  skip_if_not_installed("arrow")
  base <- withr::local_tempdir()
  ind <- "count_gap"
  dir.create(file.path(base, ind))
  # Uge-tælling med hul (og hale op til i dag, jf. levende dataleverance)
  arrow::write_parquet(data.frame(
    indikator = "x", enhed = "e",
    dato = as.Date(c("2026-01-05", "2026-01-19")),
    taeller = c(2, 3), stringsAsFactors = FALSE),
    file.path(base, ind, "p.parquet"))
  vdf <- data.frame(org_id = 5L, teknisk = "E", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  row_on <- list(diagram_id = 1L, indikator_navn_teknisk = ind, org_id = 5L,
                 nulfyld_tomme_perioder = TRUE)
  res <- scan_diagram(row_on, base, medians_df = NULL, variants_df = vdf,
                      period = "uge")
  expect_equal(res$status, "ok")
  # Hullet 2026-01-12 er fyldt med 0
  expect_true(as.Date("2026-01-12") %in% as.Date(res$slice$dato))
  expect_equal(res$slice$taeller[as.Date(res$slice$dato) == as.Date("2026-01-12")], 0)
  # Uden flag: hullet forbliver tomt
  row_off <- list(diagram_id = 2L, indikator_navn_teknisk = ind, org_id = 5L)
  res_off <- scan_diagram(row_off, base, medians_df = NULL, variants_df = vdf,
                          period = "uge")
  expect_false(as.Date("2026-01-12") %in% as.Date(res_off$slice$dato))
})

test_that("scan_view_filter: show_breaks medtager knæk-diagrammer uden signal", {
  sl <- data.frame(
    diagram_id = 1:4,
    signal = c(TRUE, FALSE, FALSE, NA),
    has_breaks = c(FALSE, TRUE, FALSE, TRUE))
  # kun signal (default)
  expect_equal(scan_view_filter(sl, FALSE)$diagram_id, 1L)
  # signal ELLER eksisterende knæk
  expect_equal(scan_view_filter(sl, FALSE, show_breaks = TRUE)$diagram_id,
               c(1L, 2L, 4L))
  # show_all vinder over show_breaks
  expect_equal(scan_view_filter(sl, TRUE, show_breaks = TRUE)$diagram_id, 1:4)
  # manglende has_breaks-kolonne (ældre kaldere) → degrader til kun-signal
  expect_equal(
    scan_view_filter(sl[, c("diagram_id", "signal")], FALSE, show_breaks = TRUE)$diagram_id,
    1L)
})

test_that("scan_view_sort: grupperer efter signal-type, stabil inden for grupper", {
  sv <- data.frame(
    diagram_id = 1:6,
    signal = c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
    has_breaks = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    signal_type = c(NA, "kryds", "serie", NA, "begge", "kryds"))
  # begge > serie > kryds (stabil: 2 før 6) > knæk-uden-signal > øvrige
  expect_equal(scan_view_sort(sv, by_type = TRUE)$diagram_id,
               c(5L, 3L, 2L, 6L, 1L, 4L))
  # by_type = FALSE → uændret rækkefølge; NULL ind → NULL ud
  expect_equal(scan_view_sort(sv, by_type = FALSE)$diagram_id, 1:6)
  expect_null(scan_view_sort(NULL, by_type = TRUE))
})

test_that("scan_view_filter: NULL forbliver NULL; show_all styrer signal-filter", {
  expect_null(scan_view_filter(NULL, FALSE))
  expect_null(scan_view_filter(NULL, TRUE))
  sl <- data.frame(diagram_id = 1:3, signal = c(TRUE, FALSE, NA))
  expect_equal(scan_view_filter(sl, FALSE)$diagram_id, 1L)   # NA taeller ikke som signal
  expect_equal(scan_view_filter(sl, TRUE)$diagram_id, 1:3)
  # 0-raekke ind -> 0-raekke ud (ej NULL): nav-laget skelner "ej scannet" fra "tom"
  expect_equal(nrow(scan_view_filter(sl[0, , drop = FALSE], FALSE)), 0L)
})

test_that("phase_stats_df formaterer observeret/forventet pr. fase som PDF'erne", {
  s <- data.frame(fase = 1:2, antal_observationer = c(12L, 12L),
    anvendelige_observationer = c(12L, 11L),
    laengste_loeb = c(5L, 8L), laengste_loeb_max = c(7L, 7L),
    antal_kryds = c(6L, 2L), antal_kryds_min = c(4L, 4L),
    anhoej_signal = c(FALSE, TRUE))
  out <- phase_stats_df(s)
  expect_equal(nrow(out), 2L)
  expect_equal(out$Fase, 1:2)
  expect_equal(out[["Serielængde"]][1], "5 / maks. 7")
  expect_equal(out[["Antal kryds"]][2], "2 / min. 4")
  expect_equal(out[["Obs."]][2], "12 (11 anv.)")
  expect_equal(out$Signal, c("–", "⚠ signal"))
})

test_that("phase_stats_df taaler NULL, tom df og NA-vaerdier", {
  expect_null(phase_stats_df(NULL))
  expect_null(phase_stats_df(data.frame()))
  s <- data.frame(fase = 1L, antal_observationer = NA_integer_,
    anvendelige_observationer = NA_integer_,
    laengste_loeb = NA_integer_, laengste_loeb_max = NA_integer_,
    antal_kryds = NA_integer_, antal_kryds_min = NA_integer_,
    anhoej_signal = NA)
  out <- phase_stats_df(s)
  expect_equal(out[["Serielængde"]], "– / maks. –")
  expect_equal(out[["Antal kryds"]], "– / min. –")
  expect_equal(out$Signal, "–")
})

test_that("signal_cascade_choices begraenser valg til dimensioner ovenfor", {
  idx <- data.frame(
    datapakke = c("P1", "P1", "P2"),
    datasaet = c("D1", "D2", "D3"),
    indikator_navn = c("I1", "I2", "I3"),
    stringsAsFactors = FALSE)
  ch <- signal_cascade_choices(idx, list(datapakke = "P1"))
  expect_identical(ch$datasaet, c("D1", "D2"))
  expect_identical(ch$indikator_navn, c("I1", "I2"))
  # Egen dimension begraenses ALDRIG af sit eget valg
  ch2 <- signal_cascade_choices(idx, list(datapakke = "P1", datasaet = "D2"))
  expect_identical(ch2$datasaet, c("D1", "D2"))
  expect_identical(ch2$indikator_navn, "I2")
  # Ingen valg = alle; multi-select OR inden for dimensionen
  ch3 <- signal_cascade_choices(idx, list())
  expect_identical(ch3$datasaet, c("D1", "D2", "D3"))
  ch4 <- signal_cascade_choices(idx, list(datasaet = c("D1", "D3")))
  expect_identical(ch4$indikator_navn, c("I1", "I3"))
})

# --- Median-flade serier (halvdelen el. flere obs. på medianen) --------------

test_that("scan_majority_on_median: udløser ved >= halvdelen på medianen", {
  qd <- function(y, ...) data.frame(y = y, ...)
  # 16 af 24 på medianen (5) → TRUE
  expect_true(scan_majority_on_median(qd(c(rep(5, 16), 6:9, 6:9))))
  # præcis halvdelen (3 af 6 på median 2) → TRUE ("eller flere")
  expect_true(scan_majority_on_median(qd(c(1, 2, 2, 2, 3, 4))))
  # konstant serie (alle ens) → TRUE (no-variation ⊂ reglen)
  expect_true(scan_majority_on_median(qd(rep(7, 10))))
  # varieret serie uden obs. på medianen → FALSE
  expect_false(scan_majority_on_median(qd(rep(c(4, 6), 12))))
  # under halvdelen på medianen → FALSE
  expect_false(scan_majority_on_median(qd(c(1, 2, 2, 3, 4))))
})

test_that("scan_majority_on_median: pr. fase, include-maske og kant-input", {
  # Fase 1 varieret, fase 2 median-flad → TRUE (én udløsende fase er nok)
  qd2 <- data.frame(y = c(1, 2, 3, 4, 5, 5, 5, 6),
                    part = c(1, 1, 1, 1, 2, 2, 2, 2))
  expect_true(scan_majority_on_median(qd2))
  # Samme serie i ÉN fase: median 4.5, ingen obs. på den → FALSE
  expect_false(scan_majority_on_median(data.frame(y = c(1, 2, 3, 4, 5, 5, 5, 6))))
  # include = FALSE-rækker tæller ikke med (hverken tæller eller nævner):
  # uden masken er 4 af 8 på medianen (5) → TRUE; med masken 1 af 5 → FALSE
  y4 <- c(5, 5, 5, 5, 1, 2, 3, 9)
  expect_true(scan_majority_on_median(data.frame(y = y4)))
  qd3 <- data.frame(y = y4,
                    include = c(TRUE, FALSE, FALSE, FALSE,
                                TRUE, TRUE, TRUE, TRUE))
  expect_false(scan_majority_on_median(qd3))
  # NULL / tom / manglende y-kolonne → FALSE (aldrig fejl)
  expect_false(scan_majority_on_median(NULL))
  expect_false(scan_majority_on_median(data.frame(y = numeric(0))))
  expect_false(scan_majority_on_median(data.frame(x = 1:3)))
  # kun NA/ikke-endelige y → FALSE
  expect_false(scan_majority_on_median(data.frame(y = c(NA_real_, Inf))))
})

test_that("scan_diagram sætter majority_on_median for median-flad serie", {
  vdf <- data.frame(org_id = 5L, teknisk = "Afd X", kort = NA, langt = NA,
                    fra_data = NA, stringsAsFactors = FALSE)
  mk <- function(vals) data.frame(
    dato = as.Date("2020-01-01") + seq_along(vals) * 30, vaerdi = vals,
    taeller = NA_real_, naevner = NA_real_,
    enhed = "afd x", stringsAsFactors = FALSE)
  row <- list(diagram_id = 1L, indikator_navn_teknisk = "x", org_id = 5L)
  flad <- scan_diagram(row, tempdir(), NULL, vdf,
                       slice_loader = function() mk(c(rep(5, 16), 6:9, 6:9)))
  expect_equal(flad$status, "ok")
  expect_true(flad$majority_on_median)
  vari <- scan_diagram(row, tempdir(), NULL, vdf,
                       slice_loader = function() mk(rep(c(4, 6), 12)))
  expect_equal(vari$status, "ok")
  expect_false(vari$majority_on_median)
})

test_that("scan_view_filter: hide_median_tied skjuler i alle visningstilstande", {
  sl <- data.frame(diagram_id = 1:4,
                   signal = c(TRUE, TRUE, FALSE, FALSE),
                   has_breaks = c(FALSE, FALSE, TRUE, FALSE),
                   majority_on_median = c(FALSE, TRUE, TRUE, FALSE))
  # default-visning (kun signal): flagget signal-diagram skjules
  expect_equal(scan_view_filter(sl, FALSE, hide_median_tied = TRUE)$diagram_id, 1L)
  expect_equal(scan_view_filter(sl, FALSE)$diagram_id, 1:2)   # uden filter
  # + knæk-visning: flagget knæk-diagram skjules også
  expect_equal(
    scan_view_filter(sl, FALSE, show_breaks = TRUE, hide_median_tied = TRUE)$diagram_id,
    1L)
  # show_all: filteret gælder stadig
  expect_equal(scan_view_filter(sl, TRUE, hide_median_tied = TRUE)$diagram_id,
               c(1L, 4L))
  expect_equal(scan_view_filter(sl, TRUE)$diagram_id, 1:4)
  # ældre kaldere uden kolonnen: intet skjules
  expect_equal(
    scan_view_filter(sl[, c("diagram_id", "signal")], FALSE,
                     hide_median_tied = TRUE)$diagram_id,
    1:2)
})
