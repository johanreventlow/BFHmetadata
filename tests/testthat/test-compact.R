# Kompaktering af parquet-lageret (fct_compact.R): omskriv hver indikators
# mange dags-partitionsfiler til ÉN fil i et delt _compact/-spejl. Manifest
# (dags-TTL) afgør om læsere må bruge spejlet — skrives SIDST, så et afbrudt
# eller fejlet forløb aldrig efterlader et spejl der tages i brug.

# Fixture: make_store_fixture() — se helper-compact.R (deles med modul-tests).

test_that("compact_list_indicators: finder nested + top-level, skipper _compact/støj", {
  base <- make_store_fixture()
  items <- compact_list_indicators(base)
  expect_setequal(items$rel, c("gruppe/ind_a", "ind_b"))
  expect_true(all(dir.exists(items$src)))
  expect_false(any(grepl("_compact", items$rel)))
})

test_that("compact_indicator: mange dagsfiler → én fil med typet dato-kolonne", {
  base <- make_store_fixture()
  dest <- file.path(base, "_compact", "gruppe", "ind_a.parquet")
  res <- compact_indicator(file.path(base, "gruppe", "ind_a"), dest)
  expect_equal(res$status, "ok")
  expect_equal(res$rows, 3L)
  expect_true(file.exists(dest))
  d <- arrow::read_parquet(dest)
  expect_s3_class(d$dato, "Date")               # partitionsnøgle → rigtig Date
  expect_setequal(as.character(d$dato),
                  c("2020-01-01", "2020-01-02", "2020-01-03"))
  # Ingen efterladte temp-filer
  expect_equal(length(list.files(dirname(dest), pattern = "tmp")), 0L)
})

test_that("compact_indicator: gen-kompaktering overskriver eksisterende fil (Windows-rename)", {
  base <- make_store_fixture()
  dest <- file.path(base, "_compact", "ind_b.parquet")
  expect_equal(compact_indicator(file.path(base, "ind_b"), dest)$status, "ok")
  first_rows <- nrow(arrow::read_parquet(dest))
  # Kilden vokser en dag → gen-kompaktering skal lande de nye rækker
  arrow::write_parquet(data.frame(
    dato = as.Date("2020-01-08"), vaerdi = 7, enhed = "e"),
    file.path(base, "ind_b", "p2.parquet"))
  expect_equal(compact_indicator(file.path(base, "ind_b"), dest)$status, "ok")
  expect_equal(nrow(arrow::read_parquet(dest)), first_rows + 1L)
})

test_that("compact_indicator: tom/manglende kilde → status 'tom', ingen fil", {
  base <- withr::local_tempdir()
  dest <- file.path(base, "_compact", "findes_ej.parquet")
  res <- compact_indicator(file.path(base, "findes_ej"), dest)
  expect_equal(res$status, "tom")
  expect_false(file.exists(dest))
})

test_that("source_fingerprint: stabilt for uændret kilde; ændres ved nye/ændrede data", {
  base <- make_store_fixture()
  src <- file.path(base, "gruppe", "ind_a")
  fp1 <- source_fingerprint(src)
  expect_identical(fp1, source_fingerprint(src))          # deterministisk
  # Ny dags-partition → nyt fingeraftryk
  p <- file.path(src, "dato=2020-01-04")
  dir.create(p)
  arrow::write_parquet(data.frame(vaerdi = 2, enhed = "e"),
                       file.path(p, "part-0.parquet"))
  fp2 <- source_fingerprint(src)
  expect_false(identical(fp1, fp2))
  # Genskrivning i nyeste partition (samme antal filer) → mtime-delen ændres
  Sys.setFileTime(file.path(p, "part-0.parquet"), Sys.time() + 30)
  expect_false(identical(fp2, source_fingerprint(src)))
  # Manglende mappe → NA; flad indikator (filer direkte i mappen) virker også
  expect_true(is.na(source_fingerprint(file.path(base, "findes_ej"))))
  expect_false(is.na(source_fingerprint(file.path(base, "ind_b"))))
})

test_that("manifest v2: entries-roundtrip; korrupt/manglende → tom entry-liste", {
  base <- withr::local_tempdir()
  expect_equal(compact_manifest_entries(compact_manifest_read(base)), list())
  compact_manifest_write(base,
    list("gruppe/ind_a" = list(fingerprint = "3|dato=2020-01-03|1|123",
                               compacted_at = "t")),
    n_ok = 1L, n_failed = 0L)
  m <- compact_manifest_read(base)
  expect_equal(compact_manifest_entries(m)[["gruppe/ind_a"]]$fingerprint,
               "3|dato=2020-01-03|1|123")
  # Korrupt manifest må aldrig vælte en læser — blot "ingen entries"
  writeLines("ikke json", compact_manifest_path(base))
  expect_equal(compact_manifest_entries(compact_manifest_read(base)), list())
})

test_that("compact_changed_items: alt nyt uden manifest; intet efter kørsel; kun ændrede", {
  base <- make_store_fixture()
  expect_setequal(compact_changed_items(base)$rel, c("gruppe/ind_a", "ind_b"))
  run_compaction(base)
  expect_equal(nrow(compact_changed_items(base)), 0L)      # intet ændret
  Sys.setFileTime(file.path(base, "ind_b", "p.parquet"), Sys.time() + 30)
  expect_equal(compact_changed_items(base)$rel, "ind_b")   # kun den rørte
})

test_that("run_compaction er inkrementel: uændrede springes over, deres spejl røres ikke", {
  base <- make_store_fixture()
  r1 <- run_compaction(base)
  expect_equal(r1$n_ok, 2L)
  expect_equal(r1$n_skipped, 0L)
  ma <- file.path(base, "_compact", "gruppe", "ind_a.parquet")
  mt_a <- file.info(ma)$mtime
  Sys.setFileTime(file.path(base, "ind_b", "p.parquet"), Sys.time() + 30)
  r2 <- run_compaction(base)
  expect_equal(r2$n_ok, 1L)                    # kun ind_b rekompakteret
  expect_equal(r2$n_skipped, 1L)               # ind_a (SundK-casen) urørt...
  expect_identical(file.info(ma)$mtime, mt_a)  # ...også på disk
})

test_that("run_compaction: fejl stopper ikke resten; fejlede får INGEN manifest-entry", {
  base <- make_store_fixture()
  # Saboter ind_b's kilde så netop den fejler (fil i stedet for læsbar parquet)
  unlink(file.path(base, "ind_b", "p.parquet"))
  writeLines("ødelagt", file.path(base, "ind_b", "kaputt.parquet"))
  res <- run_compaction(base)
  expect_equal(res$n_ok, 1L)                    # ind_a lykkedes
  expect_equal(res$n_failed, 1L)                # ind_b fejlede, uden at vælte
  expect_true(file.exists(file.path(base, "_compact", "gruppe", "ind_a.parquet")))
  entries <- compact_manifest_entries(compact_manifest_read(base))
  expect_true("gruppe/ind_a" %in% names(entries))
  expect_false("ind_b" %in% names(entries))     # fejlet → læses råt fremover
})

test_that("parquet_load_indicator_best: fp-match → spejlet bruges (bevist via poisonet spejl)", {
  base <- make_store_fixture()
  run_compaction(base)
  # Overskriv spejl-filen med kendeligt indhold. KILDENS fingeraftryk er
  # uændret → læseren skal vælge spejlet og returnere netop dette indhold.
  arrow::write_parquet(data.frame(dato = as.Date("2099-01-01"), vaerdi = 999,
                                  enhed = "spejl", stringsAsFactors = FALSE),
                       file.path(base, "_compact", "ind_b.parquet"))
  d <- parquet_load_indicator_best(base, "ind_b")
  expect_equal(d$vaerdi, 999)
  expect_s3_class(d$dato, "Date")
})

test_that("parquet_load_indicator_best: ændret kilde → automatisk rå læsning (intradag)", {
  base <- make_store_fixture()
  run_compaction(base)
  # Regenerér data intradag: ny fil i kilden → nyt fingeraftryk → spejlet
  # (uden den nye række) må ikke bruges
  arrow::write_parquet(data.frame(
    dato = as.Date("2020-01-09"), vaerdi = 9, enhed = "e"),
    file.path(base, "ind_b", "p2.parquet"))
  d <- parquet_load_indicator_best(base, "ind_b")
  expect_equal(nrow(d), 7L)                     # 6 + den nye = læste råt
})

test_that("parquet_load_indicator_best: uændret kilde → spejl gyldigt OGSÅ næste dag; force → råt", {
  base <- make_store_fixture()
  run_compaction(base)
  # Poison spejlet så vi kan se hvilken sti der læses
  arrow::write_parquet(data.frame(dato = as.Date("2099-01-01"), vaerdi = 999,
                                  enhed = "spejl", stringsAsFactors = FALSE),
                       file.path(base, "_compact", "ind_b.parquet"))
  # Ingen dags-regel længere: fp-match er nok (dato indgår slet ikke)
  expect_equal(parquet_load_indicator_best(base, "ind_b")$vaerdi, 999)
  # force = escape hatch (fx ændringer i gammel historik uden fp-ændring)
  expect_equal(nrow(parquet_load_indicator_best(base, "ind_b", force = TRUE)), 6L)
})

test_that("parquet_load_indicator_best: indikator mangler i spejl → rå fallback", {
  base <- make_store_fixture()
  run_compaction(base)
  # Ny indikator dukker op EFTER kompaktering (fx nyt datasæt samme dag)
  dir.create(file.path(base, "ind_ny"))
  arrow::write_parquet(data.frame(
    dato = as.Date("2020-02-01"), vaerdi = 1, enhed = "e"),
    file.path(base, "ind_ny", "p.parquet"))
  expect_equal(nrow(parquet_load_indicator_best(base, "ind_ny")), 1L)
})

test_that("last_parquet_dir: roundtrip + NULL når intet gemt", {
  withr::local_options(list(bfhmeta.cache_dir = withr::local_tempdir()))
  expect_null(last_parquet_dir_read())
  last_parquet_dir_write("C:/et/lager")
  expect_equal(last_parquet_dir_read(), "C:/et/lager")
  last_parquet_dir_write("C:/andet")             # overskriv
  expect_equal(last_parquet_dir_read(), "C:/andet")
})
