# excelR-hjælpelag (fct_excel_table.R): rene funktioner der driver
# jspreadsheet-baseret inline-redigering. Kolonne-spec, payload-rekonstruktion
# og celle-diff — fuld unit-dækning uden session/JS.

.cfg_excel <- list(id = "t", table = "tblTest", pk = "Id", label = "Test",
  cols = list(
    list(col = "navn", type = "text", label = "Navn"),
    list(col = "niveau", type = "int", label = "Niveau"),
    list(col = "enhed", type = "fk", label = "Enhed")))

test_that("lookup_excel_columns: pk skjules, typer mappes, fk får dropdown-source", {
  fk <- list(enhed = data.frame(id = c(10L, 20L), label = c("E10", "E20")))
  cols <- lookup_excel_columns(.cfg_excel, c("Id", "navn", "niveau", "enhed"), fk)
  expect_equal(cols$title, c("Id", "navn", "niveau", "enhed"))
  # pk er "hidden": med i data/payload (diff-match) men aldrig synlig
  expect_equal(cols$type, c("hidden", "text", "numeric", "dropdown"))
  expect_equal(cols$readOnly, c(TRUE, FALSE, FALSE, FALSE))
  expect_true(all(cols$align == "left"))   # jexcel centrerer ellers
  # dropdown-source er {id, name}-objekter (vises som label, gemmes som id)
  src <- cols$source[[4]]
  expect_equal(src$id, c(10L, 20L))
  expect_equal(src$name, c("E10", "E20"))
})

test_that("lookup_excel_columns: kolonner uden cfg-meta (ej pk) er readOnly text", {
  cols <- lookup_excel_columns(.cfg_excel, c("Id", "navn", "ukendt"), list())
  expect_equal(cols$type[3], "text")
  expect_true(cols$readOnly[3])
})

test_that("lookup_excel_columns: fk uden source degraderer til readOnly text", {
  # fk_options fejlede → hellere låst kolonne end tom dropdown der sletter data
  cols <- lookup_excel_columns(.cfg_excel, c("Id", "enhed"), list())
  expect_equal(cols$type[2], "text")
  expect_true(cols$readOnly[2])
})


test_that("excel_col_widths: bredden følger fraktilen, ikke længste værdi", {
  d <- data.frame(
    navn = c(rep("kort", 19), strrep("x", 100)),
    stringsAsFactors = FALSE)
  w <- excel_col_widths(d)
  expect_lt(w[["navn"]], 200)     # én 100-tegns outlier må ikke styre bredden
  expect_gte(w[["navn"]], 70)
})

test_that("excel_col_widths: clamp til min/max og plads til kolonnetitlen", {
  d <- data.frame(
    x = "a",
    en_meget_lang_kolonnetitel = "a",
    lang = strrep("y", 500),
    stringsAsFactors = FALSE)
  w <- excel_col_widths(d, min_px = 70, max_px = 320)
  expect_equal(unname(w[["x"]]), 70)                # min-clamp
  expect_equal(unname(w[["lang"]]), 320)            # max-clamp
  expect_gt(w[["en_meget_lang_kolonnetitel"]], 70)  # titlen skal kunne læses
})

test_that("excel_col_widths: NA/tom kolonne falder til minimum", {
  d <- data.frame(x = NA_character_, stringsAsFactors = FALSE)
  expect_equal(unname(excel_col_widths(d)[["x"]]), 70)
})

test_that("lookup_excel_columns: data giver fraktil-bredder, fk målt på labels", {
  fk <- list(enhed = data.frame(id = c(10L, 20L),
    label = c("Kirurgisk Afdeling K123", "Medicinsk Afdeling M456")))
  d <- data.frame(Id = 1:2, navn = c("A", "B"), niveau = c(1L, 2L),
                  enhed = c(10L, 20L), stringsAsFactors = FALSE)
  cols <- lookup_excel_columns(.cfg_excel, names(d), fk, data = d)
  expect_true(is.numeric(cols$width))
  # fk-bredden skal afspejle LABEL-længden (~23 tegn), ikke id'et (2 tegn)
  expect_gt(cols$width[cols$title == "enhed"], 150)
  expect_lt(cols$width[cols$title == "Id"], 100)
  # uden data: ingen width-kolonne (jexcel auto)
  expect_null(lookup_excel_columns(.cfg_excel, names(d), fk)$width)
})

test_that("excel_payload_to_df: rækker rekonstrueres navne-baseret som character", {
  p <- list(
    colHeaders = list("Id", "navn", "niveau"),
    data = list(list(1, "A", 5), list(2, "B", NULL)))
  df <- excel_payload_to_df(p)
  expect_equal(names(df), c("Id", "navn", "niveau"))
  expect_equal(df$Id, c("1", "2"))
  expect_equal(df$navn, c("A", "B"))
  expect_equal(df$niveau, c("5", NA))          # NULL → NA
})

test_that("excel_payload_to_df: tomme strenge → NA; ugyldig payload → NULL", {
  p <- list(colHeaders = list("Id", "navn"), data = list(list(1, "")))
  expect_equal(excel_payload_to_df(p)$navn, NA_character_)
  expect_null(excel_payload_to_df(list()))
  expect_null(excel_payload_to_df(list(colHeaders = list("Id"))))
  # 0 rækker → tom df med kolonnenavne
  empty <- excel_payload_to_df(list(colHeaders = list("Id", "navn"), data = list()))
  expect_equal(nrow(empty), 0L)
  expect_equal(names(empty), c("Id", "navn"))
})

test_that("excel_selected_pk: læser pk fra fullData i grid'ets AKTUELLE orden", {
  p <- list(forSelectedVals = TRUE,
    selectedDataBoundary = list(borderTop = 0),
    fullData = list(data = list(list(7, "x"), list(3, "y"))))
  expect_identical(excel_selected_pk(p), "7")
  # Klient-sorteret orden: samme position peger nu på pk 3 — pk følger med
  p$fullData$data <- rev(p$fullData$data)
  expect_identical(excel_selected_pk(p), "3")
})

test_that("excel_selected_pk: ude-af-range/manglende data → NULL", {
  expect_null(excel_selected_pk(list(
    selectedDataBoundary = list(borderTop = 5),
    fullData = list(data = list(list(1))))))
  expect_null(excel_selected_pk(list(
    selectedDataBoundary = list(borderTop = 0))))
  expect_null(excel_selected_pk(list(fullData = list(data = list(list(1))))))
})

test_that("excel_diff_cells: finder ændrede celler via pk-match, ignorerer pk-kolonnen", {
  old <- data.frame(Id = 1:2, navn = c("A", "B"), niveau = c(1L, 2L),
                    stringsAsFactors = FALSE)
  new <- data.frame(Id = c("1", "2"), navn = c("A2", "B"), niveau = c("1", "7"),
                    stringsAsFactors = FALSE)
  ch <- excel_diff_cells(old, new, "Id")
  expect_equal(nrow(ch), 2L)
  expect_equal(ch$pk[ch$col == "navn"], "1")
  expect_equal(ch$value[ch$col == "navn"], "A2")
  expect_equal(ch$pk[ch$col == "niveau"], "2")
  expect_equal(ch$value[ch$col == "niveau"], "7")
})

test_that("excel_diff_cells: NA-håndtering (tømt celle og udfyldt NA)", {
  old <- data.frame(Id = 1L, navn = "A", note = NA_character_,
                    stringsAsFactors = FALSE)
  # navn tømmes (→ NA), note udfyldes
  new <- data.frame(Id = "1", navn = NA_character_, note = "ny",
                    stringsAsFactors = FALSE)
  ch <- excel_diff_cells(old, new, "Id")
  expect_equal(nrow(ch), 2L)
  expect_true(is.na(ch$value[ch$col == "navn"]))
  expect_equal(ch$value[ch$col == "note"], "ny")
  # NA → NA er IKKE en ændring
  same <- excel_diff_cells(old, data.frame(Id = "1", navn = "A",
    note = NA_character_, stringsAsFactors = FALSE), "Id")
  expect_equal(nrow(same), 0L)
})

test_that("excel_diff_cells: ukendt pk-række og NULL-input ignoreres roligt", {
  old <- data.frame(Id = 1L, navn = "A", stringsAsFactors = FALSE)
  new <- data.frame(Id = c("1", "99"), navn = c("A", "X"),
                    stringsAsFactors = FALSE)
  ch <- excel_diff_cells(old, new, "Id")
  expect_equal(nrow(ch), 0L)                    # række 99 findes ikke → drop
  expect_equal(nrow(excel_diff_cells(old, NULL, "Id")), 0L)
  expect_equal(nrow(excel_diff_cells(NULL, new, "Id")), 0L)
})
