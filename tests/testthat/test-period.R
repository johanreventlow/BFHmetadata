# Periode-aggregering — vendoret fra BFHddl. Testene spejler BFHddl's egne
# (test-data_loader.R) plus BFHmetadata-specifikke værn.
#
# ALLE tests sætter `today` eksplicit: drop_incomplete afhænger af dagens dato,
# så en test der læner sig på Sys.Date() begynder at fejle ved periodegrænser.

# --- period_to_en -----------------------------------------------------------

test_that("period_to_en oversætter de danske værdier", {
  expect_equal(period_to_en("dag"), "day")
  expect_equal(period_to_en("uge"), "week")
  expect_equal(period_to_en("maaned"), "month")
  expect_equal(period_to_en("kvartal"), "quarter")
  # Begge stavemåder af år skal virke (BFHddl har begge grene)
  expect_equal(period_to_en("aar"), "year")
  expect_equal(period_to_en("år"), "year")
})

test_that("period_to_en: tom/NA/whitespace → dag (ingen aggregering)", {
  expect_equal(period_to_en(NA_character_), "day")
  expect_equal(period_to_en(NULL), "day")
  expect_equal(period_to_en(""), "day")
  expect_equal(period_to_en("   "), "day")
  expect_equal(period_to_en(character(0)), "day")
})

test_that("period_to_en er case-insensitiv", {
  expect_equal(period_to_en("UGE"), "week")
  expect_equal(period_to_en("Maaned"), "month")
})

test_that("period_to_en lader ukendt værdi passere (fejler højlydt nedstrøms)", {
  # Bevidst: en ukendt periode må ALDRIG stiltiende blive til 'day' — så ville
  # en tastefejl i Access give uaggregerede signaler uden nogen fejlmelding.
  expect_equal(period_to_en("fjortendagligt"), "fjortendagligt")
})

# --- aggregate_to_period: grundlæggende ------------------------------------

test_that("period = day er no-op", {
  d <- data.frame(dato = as.Date("2024-01-01") + 0:13,
                  enhed = "A", taeller = 1, naevner = 2)
  expect_identical(aggregate_to_period(d, "day", today = as.Date("2024-06-01")), d)
})

test_that("uge-aggregering summerer taeller+naevner pr. mandags-bucket", {
  # 14 dage fra mandag 2024-01-01 → præcis 2 hele uger
  d <- data.frame(dato = as.Date("2024-01-01") + 0:13,
                  enhed = "A", taeller = 1, naevner = 2)
  res <- aggregate_to_period(d, "week", today = as.Date("2024-06-01"))
  expect_equal(nrow(res), 2)
  expect_equal(sort(res$dato), as.Date(c("2024-01-01", "2024-01-08")))
  expect_equal(res$taeller, c(7, 7))
  expect_equal(res$naevner, c(14, 14))
})

test_that("maaned/kvartal/aar bucketer korrekt", {
  d <- data.frame(dato = as.Date("2024-01-15") + 0:0,
                  enhed = "A", taeller = 1, naevner = 1)
  d <- rbind(d, data.frame(dato = as.Date("2024-02-20"),
                           enhed = "A", taeller = 2, naevner = 2))
  today <- as.Date("2025-06-01")
  m <- aggregate_to_period(d, "month", today = today)
  expect_equal(sort(m$dato), as.Date(c("2024-01-01", "2024-02-01")))
  q <- aggregate_to_period(d, "quarter", today = today)
  expect_equal(q$dato, as.Date("2024-01-01"))
  expect_equal(q$taeller, 3)
  y <- aggregate_to_period(d, "year", today = today)
  expect_equal(y$dato, as.Date("2024-01-01"))
  expect_equal(y$taeller, 3)
})

test_that("grupperingskolonner bevares — enheder blandes ALDRIG sammen", {
  # Kernen i additivitets-kontrakten: to enheder i samme uge må give to rækker.
  d <- data.frame(
    dato = rep(as.Date("2024-01-01") + 0:6, 2),
    enhed = rep(c("A", "B"), each = 7),
    indikator = "ind",
    taeller = 1, naevner = 2)
  res <- aggregate_to_period(d, "week", today = as.Date("2024-06-01"))
  expect_equal(nrow(res), 2)
  expect_setequal(res$enhed, c("A", "B"))
  expect_true(all(res$taeller == 7))
  expect_true("indikator" %in% names(res))
})

test_that("naevner droppes helt når den ikke fandtes i input (tælle-serie)", {
  d <- data.frame(dato = as.Date("2024-01-01") + 0:6, enhed = "A", taeller = 3)
  res <- aggregate_to_period(d, "week", today = as.Date("2024-06-01"))
  expect_false("naevner" %in% names(res))
  expect_equal(res$taeller, 21)
})

test_that("na.rm = FALSE: én NA forgifter hele bucket'en (bevidst)", {
  # Fravær af data må ikke tælle som 0 — matcher BFHddl.
  d <- data.frame(dato = as.Date("2024-01-01") + 0:6, enhed = "A",
                  taeller = c(1, 1, NA, 1, 1, 1, 1))
  res <- aggregate_to_period(d, "week", today = as.Date("2024-06-01"))
  expect_true(is.na(res$taeller))
})

test_that("manglende dato-kolonne fejler", {
  d <- data.frame(enhed = "A", taeller = 1)
  expect_error(aggregate_to_period(d, "week", today = as.Date("2024-06-01")))
})

# --- drop_incomplete --------------------------------------------------------

test_that("drop_incomplete fjerner den igangværende periode", {
  d <- data.frame(dato = as.Date(c("2024-01-15", "2024-02-15", "2024-03-15")),
                  enhed = "A", taeller = 1)
  # I marts er marts-bucket'en ufuldstændig → kun jan+feb
  res <- aggregate_to_period(d, "month", today = as.Date("2024-03-05"))
  expect_equal(sort(res$dato), as.Date(c("2024-01-01", "2024-02-01")))
  # I april er alle tre hele
  res2 <- aggregate_to_period(d, "month", today = as.Date("2024-04-02"))
  expect_equal(nrow(res2), 3)
})

test_that("drop_incomplete = FALSE beholder den igangværende periode", {
  d <- data.frame(dato = as.Date(c("2024-01-15", "2024-03-15")),
                  enhed = "A", taeller = 1)
  res <- aggregate_to_period(d, "month", drop_incomplete = FALSE,
                            today = as.Date("2024-03-05"))
  expect_equal(nrow(res), 2)
})

test_that("drop_incomplete kan tømme et slice helt", {
  # Serie hvis eneste data ligger i indeværende periode → 0 rækker, ikke fejl.
  d <- data.frame(dato = as.Date("2024-03-04"), enhed = "A", taeller = 1)
  res <- aggregate_to_period(d, "month", today = as.Date("2024-03-05"))
  expect_equal(nrow(res), 0)
})

# --- ugestart ---------------------------------------------------------------

test_that("ugestart følger lubridate.week.start-optionen", {
  # Afviger appen fra batch-pipelinen forskydes HVER uge-bucket en dag, og alle
  # signaler ændres. Derfor læses optionen (ej hardcoded 1).
  d <- data.frame(dato = as.Date("2024-01-07"), enhed = "A", taeller = 1)  # søndag
  withr::with_options(list(lubridate.week.start = 1), {   # mandag-start
    res <- aggregate_to_period(d, "week", today = as.Date("2024-06-01"))
    expect_equal(res$dato, as.Date("2024-01-01"))
  })
  withr::with_options(list(lubridate.week.start = 7), {   # søndag-start
    res <- aggregate_to_period(d, "week", today = as.Date("2024-06-01"))
    expect_equal(res$dato, as.Date("2024-01-07"))
  })
})

# --- vaerdi-værn (BFHmetadata-specifikt) ------------------------------------

test_that("vaerdi-kolonne + aggregering fejler højlydt", {
  # aggregate_to_period summerer kun taeller/naevner. En vaerdi-kolonne ville
  # havne i group_cols og gøre aggregeringen til en TAVS no-op.
  d <- data.frame(dato = as.Date("2024-01-01") + 0:6, enhed = "A",
                  vaerdi = 1:7, taeller = 1)
  expect_error(aggregate_to_period(d, "week", today = as.Date("2024-06-01")),
               "vaerdi")
  # day er stadig no-op — værnet må ikke ramme uaggregerede serier
  expect_silent(aggregate_to_period(d, "day", today = as.Date("2024-06-01")))
})

# --- dublerede (enhed, dato) ------------------------------------------------

test_that("dublerede (enhed, dato)-rækker summeres i samme bucket", {
  # Findes i virkelige data (fx dhdb_40_002 har flere rækker pr. enhed/måned).
  d <- data.frame(dato = as.Date(c("2024-01-15", "2024-01-15", "2024-02-15")),
                  enhed = "A", taeller = c(2, 3, 5), naevner = c(10, 10, 20))
  res <- aggregate_to_period(d, "month", today = as.Date("2024-06-01"))
  expect_equal(nrow(res), 2)
  expect_equal(res$taeller[res$dato == as.Date("2024-01-01")], 5)
  expect_equal(res$naevner[res$dato == as.Date("2024-01-01")], 20)
})

# --- filter_medians_by_period ----------------------------------------------
# Et knæk gemt som dato betyder en RÆKKEPOSITION. Skifter aggregeringen,
# lander knækket et andet sted (2025-03-17 -> 2025-04-01 under maaned).
# Derfor ignoreres knæk sat under en anden aggregering.

.meds <- function(...) data.frame(id = 1:3, diagram = 1L,
  laas_median = as.Date(c("2025-01-06", "2025-02-03", "2025-03-03")),
  ..., stringsAsFactors = FALSE)

test_that("knæk med matchende aggregering beholdes", {
  m <- .meds(aggregering = c("uge", "uge", "uge"))
  expect_equal(nrow(filter_medians_by_period(m, "uge")), 3)
})

test_that("knæk med afvigende aggregering frafiltreres", {
  m <- .meds(aggregering = c("uge", "maaned", "uge"))
  res <- filter_medians_by_period(m, "uge")
  expect_equal(nrow(res), 2)
  expect_equal(res$id, c(1L, 3L))
})

test_that("NA/tom aggregering regnes som match (bagudkompatibelt)", {
  m <- .meds(aggregering = c(NA_character_, "", "   "))
  expect_equal(nrow(filter_medians_by_period(m, "maaned")), 3)
})

test_that("sammenligning er normaliseret (maaned == måned)", {
  m <- .meds(aggregering = c("maaned", "måned", "MAANED"))
  expect_equal(nrow(filter_medians_by_period(m, "maaned")), 3)
})

test_that("df uden aggregering-kolonne passerer uændret", {
  m <- data.frame(id = 1L, diagram = 1L, laas_median = as.Date("2025-01-06"))
  expect_equal(nrow(filter_medians_by_period(m, "uge")), 1)
  # tom/NULL må ikke fejle
  expect_null(filter_medians_by_period(NULL, "uge"))
  expect_equal(nrow(filter_medians_by_period(m[0, ], "uge")), 0)
})

test_that("periode_choices: kanonisk ordforråd + ukendte DB-værdier bagest", {
  # Uden DB-værdier: hele det kanoniske ordforråd i varigheds-orden
  expect_identical(periode_choices(),
                   c("dag", "uge", "maaned", "kvartal", "aar"))
  # Værdier i brug der allerede er kanoniske dubleres ikke
  expect_identical(periode_choices(c("maaned", "uge")),
                   c("dag", "uge", "maaned", "kvartal", "aar"))
  # Legacy/stavevarianter (fx "måned") bevares — bagest, så de stadig kan
  # ses og genvælges
  expect_identical(periode_choices(c("måned", "uge")),
                   c("dag", "uge", "maaned", "kvartal", "aar", "måned"))
  # NA/tomme droppes
  expect_identical(periode_choices(c(NA, "", "uge")),
                   c("dag", "uge", "maaned", "kvartal", "aar"))
})
