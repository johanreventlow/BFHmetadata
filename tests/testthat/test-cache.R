# Persistent dags-cache for indikator-slices (fct_cache.R). Cachen lever på
# disk på tværs af sessioner; TTL = kalenderdag (data opdateres natligt).

test_that("slice_cache_key: deterministisk, dato-afhængig og filnavn-sikker", {
  k1 <- slice_cache_key("/data/parquet", "adhd;adhd_02_004;0",
                        date = as.Date("2026-08-11"))
  k2 <- slice_cache_key("/data/parquet", "adhd;adhd_02_004;0",
                        date = as.Date("2026-08-11"))
  expect_identical(k1, k2)                       # deterministisk
  expect_false(grepl("[;/\\\\: ]", k1))          # farlige tegn saneret væk
  # Ny dag → ny nøgle (dags-TTL: gårsdagens cache genbruges aldrig)
  k3 <- slice_cache_key("/data/parquet", "adhd;adhd_02_004;0",
                        date = as.Date("2026-08-12"))
  expect_false(identical(k1, k3))
  # Andet parquet-lager → anden nøgle (samme indikatornavn må ej kollidere)
  k4 <- slice_cache_key("/andet/lager", "adhd;adhd_02_004;0",
                        date = as.Date("2026-08-11"))
  expect_false(identical(k1, k4))
  # Sanering må ikke give kollision: "a;b" og "a_b" er forskellige indikatorer
  k5 <- slice_cache_key("/data/parquet", "adhd_adhd_02_004_0",
                        date = as.Date("2026-08-11"))
  expect_false(identical(k1, k5))
})

test_that("load_indicator_slice_cached: loader kaldes én gang, cache-hit derefter", {
  dir <- withr::local_tempdir()
  calls <- 0L
  loader <- function() {
    calls <<- calls + 1L
    data.frame(dato = as.Date("2020-01-01") + 0:2, vaerdi = 1:3, enhed = "e")
  }
  d1 <- load_indicator_slice_cached("/b", "ind", loader, cache_dir = dir)
  d2 <- load_indicator_slice_cached("/b", "ind", loader, cache_dir = dir)
  expect_equal(calls, 1L)                        # andet kald = disk-hit
  expect_equal(d1, d2)
  expect_equal(nrow(d2), 3L)
})

test_that("load_indicator_slice_cached: force=TRUE ignorerer cache og genskriver", {
  dir <- withr::local_tempdir()
  calls <- 0L
  loader <- function() {
    calls <<- calls + 1L
    data.frame(dato = as.Date("2020-01-01"), vaerdi = calls, enhed = "e")
  }
  load_indicator_slice_cached("/b", "ind", loader, cache_dir = dir)
  d2 <- load_indicator_slice_cached("/b", "ind", loader, cache_dir = dir,
                                    force = TRUE)
  expect_equal(calls, 2L)
  expect_equal(d2$vaerdi, 2)                     # force-resultatet er det nye
  # ...og force-resultatet er nu det cachede (næste normale kald = hit)
  d3 <- load_indicator_slice_cached("/b", "ind", loader, cache_dir = dir)
  expect_equal(calls, 2L)
  expect_equal(d3$vaerdi, 2)
})

test_that("load_indicator_slice_cached: NULL/tom fra loader caches ikke", {
  dir <- withr::local_tempdir()
  calls <- 0L
  loader <- function() { calls <<- calls + 1L; NULL }
  expect_null(load_indicator_slice_cached("/b", "mangler", loader, cache_dir = dir))
  expect_null(load_indicator_slice_cached("/b", "mangler", loader, cache_dir = dir))
  expect_equal(calls, 2L)                        # NULL må ALDRIG blive et dags-hit
  expect_equal(length(list.files(dir)), 0L)      # intet skrevet
})

test_that("load_indicator_slice_cached: ny dato → nyt load (dags-TTL)", {
  dir <- withr::local_tempdir()
  calls <- 0L
  loader <- function() {
    calls <<- calls + 1L
    data.frame(dato = as.Date("2020-01-01"), vaerdi = 1, enhed = "e")
  }
  load_indicator_slice_cached("/b", "ind", loader, cache_dir = dir,
                              date = as.Date("2026-08-11"))
  load_indicator_slice_cached("/b", "ind", loader, cache_dir = dir,
                              date = as.Date("2026-08-12"))
  expect_equal(calls, 2L)                        # i går tæller ikke som i dag
})

test_that("fingeraftryk-nøgle (key) vinder over dato: genbrug på tværs af dage, frisk ved ændring", {
  dir <- withr::local_tempdir()
  calls <- 0L
  loader <- function() {
    calls <<- calls + 1L
    data.frame(dato = as.Date("2020-01-01"), vaerdi = 1, enhed = "e")
  }
  fp <- "1271|dato=2026-06-26|3|1785500470"
  # Samme fingeraftryk, FORSKELLIGE dage → cache-hit (uændret kilde er gyldig
  # på tværs af dage — SundK-casen)
  load_indicator_slice_cached("/b", "ind", loader, cache_dir = dir,
                              date = as.Date("2026-08-11"), key = fp)
  load_indicator_slice_cached("/b", "ind", loader, cache_dir = dir,
                              date = as.Date("2026-08-12"), key = fp)
  expect_equal(calls, 1L)
  # Nyt fingeraftryk (intradag-regenerering) → frisk indlæsning samme dag
  load_indicator_slice_cached("/b", "ind", loader, cache_dir = dir,
                              date = as.Date("2026-08-12"),
                              key = "1272|dato=2026-08-12|3|1785600000")
  expect_equal(calls, 2L)
  # NA-nøgle (kilde kunne ikke fingeraftrykkes) → dags-adfærd, ingen fejl
  load_indicator_slice_cached("/b", "ind", loader, cache_dir = dir,
                              key = NA_character_)
  expect_equal(calls, 3L)
})

test_that("load_indicator_slice_cached: korrupt cache-fil → fald tilbage til loader", {
  dir <- withr::local_tempdir()
  key <- slice_cache_key("/b", "ind")
  writeLines("ikke en rds", file.path(dir, key))
  calls <- 0L
  loader <- function() {
    calls <<- calls + 1L
    data.frame(dato = as.Date("2020-01-01"), vaerdi = 1, enhed = "e")
  }
  d <- load_indicator_slice_cached("/b", "ind", loader, cache_dir = dir)
  expect_equal(calls, 1L)                        # korrupt fil må ej vælte scan
  expect_equal(nrow(d), 1L)
})

test_that("slice_cache_prune: fjerner gamle filer, beholder friske", {
  dir <- withr::local_tempdir()
  old <- file.path(dir, "gammel.rds"); saveRDS(1, old)
  new <- file.path(dir, "frisk.rds"); saveRDS(2, new)
  Sys.setFileTime(old, Sys.time() - 10 * 24 * 3600)   # 10 dage gammel
  slice_cache_prune(cache_dir = dir, max_age_days = 7)
  expect_false(file.exists(old))
  expect_true(file.exists(new))
})

test_that("slice_cache_prune: tom/ikke-eksisterende mappe → ingen fejl", {
  expect_no_error(slice_cache_prune(cache_dir = file.path(tempdir(), "findes_ej_xyz")))
})
