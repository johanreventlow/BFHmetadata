# Rene hierarki-editor-hjælpere: felt-mapping, validering af inline-events
# (genbruges af excelR-grid'et) og grid-data/kolonne-byggere.

test_that("inline-felter mapper de fem viste felter til lagringsfelter", {
  cfg <- HIERARCHY_TABLES$org_struktur
  expect_identical(.hierarchy_inline_fields(cfg), c(
    teknisk = "organisatorisk_navn_teknisk",
    langt = "organisatorisk_navn_langt",
    kort = "organisatorisk_navn_kort",
    parent = "parent_Id",
    niveau = "organisatorisk_niveau"))
})

test_that("inline-opdatering bygger en komplet vaerdiliste", {
  db <- fake_hierarchy_db()
  result <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2, field = "organisatorisk_navn_kort", value = "Nyt"))
  expect_true(result$ok)
  expect_false(result$unchanged)
  expect_identical(result$id, 2L)
  expect_identical(result$values$organisatorisk_navn_kort, "Nyt")
  expect_identical(names(result$values), hierarchy_edit_cols(.hierarchy_cfg()))
})

test_that("tom tekst og tom foraelder normaliseres til korrekt NA-type", {
  db <- fake_hierarchy_db()
  text <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2, field = "organisatorisk_navn_kort", value = ""))
  root <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2, field = "parent_Id", value = ""))
  expect_true(is.na(text$values$organisatorisk_navn_kort))
  expect_type(text$values$organisatorisk_navn_kort, "character")
  expect_true(is.na(root$values$parent_Id))
  expect_type(root$values$parent_Id, "integer")
})

test_that("inline-opdatering afviser ukendt felt og tomt langt navn", {
  db <- fake_hierarchy_db()
  unknown <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2, field = "id", value = "99"))
  empty <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2, field = "organisatorisk_navn_langt", value = ""))
  expect_false(unknown$ok)
  expect_match(unknown$error, "felt", ignore.case = TRUE)
  expect_false(empty$ok)
  expect_match(empty$error, "obligatorisk", ignore.case = TRUE)
})

test_that("inline-foraelder afviser egen subtree", {
  db <- fake_hierarchy_db()
  self <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 1, field = "parent_Id", value = "1"))
  child <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 1, field = "parent_Id", value = "3"))
  expect_false(self$ok)
  expect_false(child$ok)
  expect_match(child$error, "cyklus|subtree|efterkommer", ignore.case = TRUE)
})

test_that("ugyldigt visningsfelt afvises uden en intern fejl", {
  db <- fake_hierarchy_db()
  cfg <- .hierarchy_cfg()
  cfg$display_col <- "findes_ikke"
  result <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), cfg,
    list(id = 2, field = "organisatorisk_navn_kort", value = "Nyt"))
  expect_false(result$ok)
  expect_match(result$error, "visningsfelt", ignore.case = TRUE)
})

test_that("raekkevaerdier bruger konfigureret aktiv-kolonne naar alias mangler", {
  db <- fake_hierarchy_db()
  cfg <- .hierarchy_cfg()
  cfg$aktiv_col <- "aktiv_flag"
  nodes <- db$list_nodes()
  nodes$aktiv_flag <- c(TRUE, FALSE, TRUE, FALSE)
  values <- .hierarchy_row_values(nodes[1, , drop = FALSE], cfg)
  expect_true(values$aktiv_flag)
  expect_identical(names(values), hierarchy_edit_cols(cfg))
})

test_that("inline-opdatering afviser ugyldige og ukendte id'er", {
  db <- fake_hierarchy_db()
  oversized <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2147483648, field = "organisatorisk_navn_kort", value = "Nyt"))
  unknown <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 99, field = "organisatorisk_navn_kort", value = "Nyt"))
  expect_false(oversized$ok)
  expect_match(oversized$error, "id", ignore.case = TRUE)
  expect_false(unknown$ok)
  expect_match(unknown$error, "fundet", ignore.case = TRUE)
})

test_that("inline-opdatering afviser ukendt foraelder og niveau", {
  db <- fake_hierarchy_db()
  parent <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2, field = "parent_Id", value = "99"))
  niveau <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 2, field = "organisatorisk_niveau", value = "99"))
  expect_false(parent$ok)
  expect_match(parent$error, "foraelder", ignore.case = TRUE)
  expect_false(niveau$ok)
  expect_match(niveau$error, "niveau", ignore.case = TRUE)
})

test_that("uaendret vaerdi bevarer raekken og advarer om niveau-spring", {
  db <- fake_hierarchy_db()
  result <- .prepare_hierarchy_inline_update(
    db$list_nodes(), db$niveau_options(), .hierarchy_cfg(),
    list(id = 3, field = "organisatorisk_navn_kort", value = "C"))
  expect_true(result$ok)
  expect_true(result$unchanged)
  expect_identical(result$warning, "Niveau er ikke dybere end forælderens niveau")
})

# --- excelR-grid-byggere -----------------------------------------------------

test_that("hierarchy_grid_fields mapper grid-titler til lagringskolonner", {
  fm <- hierarchy_grid_fields(.hierarchy_cfg())
  expect_identical(unname(fm[c("Teknisk navn", "Langt navn", "Kort navn")]),
                   c("organisatorisk_navn_teknisk", "organisatorisk_navn_langt",
                     "organisatorisk_navn_kort"))
  expect_identical(fm[["Forælder"]], "parent_Id")
  expect_identical(fm[["Niveau"]], "organisatorisk_niveau")
})

test_that("hierarchy_excel_data: grid i trae-orden med indrykning og id-værdier", {
  db <- fake_hierarchy_db()
  d <- hierarchy_order(db$list_nodes(), "id", "parent_id_raw",
                       .hierarchy_cfg()$display_col)
  g <- hierarchy_excel_data(d, .hierarchy_cfg())
  expect_equal(names(g), c("id", "Struktur", "Teknisk navn", "Langt navn",
                           "Kort navn", "Forælder", "Niveau"))
  expect_equal(g$id, c(1L, 2L, 3L, 4L))
  expect_equal(g$Struktur[1], "Rod A")                 # rod uindrykket
  expect_match(g$Struktur[3], "^ + +Barn C$") # depth 2 → indrykket
  expect_equal(g[["Forælder"]], c("", "1", "2", ""))   # NA-parent → "" (rod)
  expect_equal(g[["Niveau"]], c("10", "20", "10", "10"))
})

test_that("hierarchy_excel_data: NA-tekst vises tomt; tomt trae bevarer kolonner", {
  db <- fake_hierarchy_db()
  d <- hierarchy_order(db$list_nodes(), "id", "parent_id_raw",
                       .hierarchy_cfg()$display_col)
  d$organisatorisk_navn_kort[2] <- NA_character_
  g <- hierarchy_excel_data(d, .hierarchy_cfg())
  expect_equal(g[["Kort navn"]][2], "")
  empty <- hierarchy_excel_data(d[FALSE, ], .hierarchy_cfg())
  expect_equal(nrow(empty), 0L)
  expect_equal(names(empty), names(g))
})

test_that("hierarchy_excel_columns: dropdowns med (rod)/(vælg), ukendte id'er bevares", {
  db <- fake_hierarchy_db()
  d <- hierarchy_order(db$list_nodes(), "id", "parent_id_raw",
                       .hierarchy_cfg()$display_col)
  d$parent_id_raw[d$id == 4L] <- 999L
  d$niveau_id[d$id == 4L] <- 888L
  cols <- hierarchy_excel_columns(.hierarchy_cfg(), d, db$niveau_options())
  expect_equal(cols$title[1:2], c("id", "Struktur"))
  expect_identical(cols$type[1], "hidden")             # pk aldrig synlig
  expect_true(all(cols$readOnly[1:2]))                 # id + struktur låst
  expect_false(any(cols$readOnly[3:7]))                # resten redigerbar
  expect_true(all(cols$align == "left"))
  expect_true(is.numeric(cols$width))                  # fraktil-bredder sat
  p <- cols$source[[which(cols$title == "Forælder")]]
  expect_equal(p$name[p$id == ""], "(rod)")
  expect_true("Ukendt forælder #999" %in% p$name)      # ukendt id tabes ikke
  nv <- cols$source[[which(cols$title == "Niveau")]]
  expect_equal(nv$name[nv$id == ""], "(vælg)")
  expect_true("Ukendt niveau #888" %in% nv$name)
})
