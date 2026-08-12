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

test_that("teksteditor escaper attributter og indeholder stabil routing", {
  html <- .hierarchy_text_editor_html(
    function(x) paste0("org-", x), 7L, "organisatorisk_navn_langt",
    '\"><script>alert(1)</script>', depth = 2L)
  expect_match(html, "data-node-id=\"7\"")
  expect_match(html, "data-field=\"organisatorisk_navn_langt\"")
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
})

test_that("select-editor escaper labels og udelader subtree-valg", {
  choices <- c("Rod <A>" = "", "Barn & B" = "2")
  html <- .hierarchy_select_editor_html(
    function(x) paste0("org-", x), 7L, "parent_Id", "", choices,
    root = TRUE)
  expect_false(grepl("Rod <A>", html, fixed = TRUE))
  expect_match(html, "Rod &lt;A&gt;", fixed = TRUE)
  expect_match(html, "data-saved=\"\"")
})

test_that("lazy select-editor renderer kun den aktuelle vaerdi", {
  html <- .hierarchy_select_editor_html(
    function(x) paste0("org-", x), 7L, "parent_Id", "2",
    choices = c("(rod)" = "", "Barn & B" = "2"), root = TRUE,
    lazy = TRUE, current_label = "Barn & B")

  expect_match(html, 'data-lazy="true"', fixed = TRUE)
  expect_match(html, '<option value="2" selected>Barn &amp; B</option>',
               fixed = TRUE)
  expect_length(regmatches(html, gregexpr("<option", html, fixed = TRUE))[[1]],
                1L)
})

test_that("DT callback hydratiserer lazy dropdowns fra delte valg", {
  js <- as.character(.hierarchy_dt_callback(
    function(x) paste0("org-", x),
    parent_choices = data.frame(id = c(1L, 2L), label = c("Rod", "Barn")),
    level_choices = data.frame(id = 10L, label = "Niveau")))

  expect_match(js, "parentChoices", fixed = TRUE)
  expect_match(js, "levelChoices", fixed = TRUE)
  expect_match(js, "focus.hierarchy-editor", fixed = TRUE)
  expect_match(js, "data-lazy", fixed = TRUE)
  expect_match(js, "wouldCreateCycle", fixed = TRUE)
  expect_match(js, "parent_id", fixed = TRUE)
  expect_match(js, "new Map", fixed = TRUE)
  expect_match(js, "addedSaved", fixed = TRUE)
  expect_match(js, "editor.dataset.savedLabel", fixed = TRUE)
  expect_match(js, "(v\\u00e6lg)", fixed = TRUE)
  expect_false(grepl("vÃ¦lg", js, fixed = TRUE))
})

test_that("lazy select-editor bevarer label til ugyldig eksisterende vaerdi", {
  html <- .hierarchy_select_editor_html(
    identity, 7L, "parent_Id", "999", choices = character(), root = TRUE,
    lazy = TRUE, current_label = "Manglende forælder")

  expect_match(html, 'data-saved-label="Manglende forælder"', fixed = TRUE)
  expect_match(html,
    '<option value="999" selected>Manglende forælder</option>', fixed = TRUE)
})

test_that("DT callback bruger DataTables-argumentet og implementerer Enter Escape blur", {
  js <- as.character(.hierarchy_dt_callback(function(x) paste0("org-", x)))
  expect_match(js, "function(table)", fixed = TRUE)
  expect_match(js, "table.table().node()", fixed = TRUE)
  expect_false(grepl("this.api()", js, fixed = TRUE))
  expect_match(js, "Shiny.setInputValue", fixed = TRUE)
  expect_match(js, "org-inline_edit", fixed = TRUE)
  expect_match(js, "keydown", fixed = TRUE)
  expect_match(js, "Escape", fixed = TRUE)
  expect_match(js, "blur", fixed = TRUE)
})

test_that("DT callback sender valgt nodes stabile id uden at overtage raekkevalg", {
  js <- as.character(.hierarchy_dt_callback(function(x) paste0("org-", x)))
  expect_match(js, "org-selected_node_id", fixed = TRUE)
  expect_match(js, "tbody tr", fixed = TRUE)
  expect_match(js, ".hierarchy-editor[data-node-id]", fixed = TRUE)
  expect_match(js, "editor.dataset.nodeId", fixed = TRUE)
  expect_match(js, "classList.contains('selected')", fixed = TRUE)
  expect_match(js, "classList.contains('active')", fixed = TRUE)
  expect_match(js, "setTimeout", fixed = TRUE)
  expect_match(js, "priority: 'event'", fixed = TRUE)
  expect_false(grepl("stopPropagation", js, fixed = TRUE))
})

test_that("DT callback bevarer gemt baseline mens en redigering afventer", {
  js <- as.character(.hierarchy_dt_callback(function(x) paste0("org-", x)))
  expect_match(js, "classList.contains('hierarchy-saving')", fixed = TRUE)
  expect_false(grepl("editor.dataset.saved = editor.value", js, fixed = TRUE))
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
  expect_identical(result$warning, "Niveau er ikke dybere end for\u00e6lderens niveau")
})
