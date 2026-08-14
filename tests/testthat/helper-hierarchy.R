# testServer-tests for mod_hierarchy med fake-db (closures der logger kald).
# 4-node trae: 1 (rod, niveau 1) -> 2 (niveau 2) -> 3 (niveau 1, niveau-spring
# op) ; 4 er anden rod (niveau 1).

.hierarchy_cfg <- function() HIERARCHY_TABLES$org_struktur

# excelR onChange-payload for hierarki-grid'et: bygget fra den ægte
# grid-data-helper med én celle overskrevet (value = NULL → tømt celle).
.hierarchy_grid_edit <- function(tree_df, cfg, id, column, value) {
  g <- hierarchy_excel_data(tree_df, cfg)
  g[[column]][g$id == id] <- if (is.null(value)) NA_character_ else value
  list(
    colHeaders = as.list(names(g)),
    data = lapply(seq_len(nrow(g)), function(i) {
      lapply(g[i, ], function(v) {
        if (length(v) == 1 && is.na(v)) NULL else as.character(v)
      })
    }),
    forSelectedVals = FALSE)
}

# excelR onSelection-payload: borderTop er 0-baseret række i trae-orden;
# fullData bærer grid'ets rækkefølge (pk i første kolonne)
.hierarchy_grid_select <- function(row0, pks = c("1", "2", "3", "4")) {
  list(forSelectedVals = TRUE,
       selectedDataBoundary = list(borderTop = row0, borderBottom = row0,
                                   borderLeft = 0, borderRight = 0),
       fullData = list(data = lapply(pks, function(p) list(p))))
}

# Fake-db for indikator-hierarkiet (cfg med aktiv_col): 3-node trae
# 1 (rod/datapakke) -> 2 (datasaet, aktiv) + 3 (datasaet, inaktiv).
fake_ih_nodes <- function() data.frame(
  id = c(1L, 2L, 3L),
  parent_id_raw = c(NA_integer_, 1L, 1L),
  hierarki_navn = c("Rod", "Datasaet A", "Datasaet B"),
  hierarki_navn_kort = c("R", "A", "B"),
  beskrivelse_kort = c(NA_character_, "kort", NA_character_),
  beskrivelse_lang = c(NA_character_, NA_character_, NA_character_),
  kilde_id = c(NA_character_, NA_character_, NA_character_),
  aktiv = c(TRUE, TRUE, FALSE),
  niveau_id = c(10L, 20L, 20L),
  niveau_num = c(1L, 2L, 2L),
  niveau_navn = c("Datapakke", "Datasaet", "Datasaet"),
  stringsAsFactors = FALSE)

fake_ih_niveauer <- function() data.frame(
  id = c(10L, 20L), label = c("Datapakke", "Datasaet"),
  stringsAsFactors = FALSE)

fake_ih_db <- function() {
  nodes <- fake_ih_nodes()
  niveauer <- fake_ih_niveauer()
  calls <- list(created = NULL, updated = NULL, updates = list(), deleted = NULL)
  list(
    list_nodes = function() nodes,
    niveau_options = function() niveauer,
    create_node = function(values) { calls$created <<- values; 99L },
    update_node = function(id, values) {
      call <- list(id = as.integer(id), values = values)
      calls$updated <<- call
      calls$updates[[length(calls$updates) + 1L]] <<- call
      row <- match(as.integer(id), nodes$id)
      if (is.na(row)) stop("Node ikke fundet")
      for (col in names(values)) {
        node_col <- switch(col,
          parent_id = "parent_id_raw",
          indikator_niveau = "niveau_id",
          col)
        if (node_col %in% names(nodes)) nodes[[node_col]][row] <<- values[[col]]
      }
      1L
    },
    delete_node = function(id) {
      calls$deleted <<- as.integer(id)
      nodes <<- nodes[nodes$id != as.integer(id), , drop = FALSE]
      1L
    },
    child_count = function(id) sum(nodes$parent_id_raw %in% id, na.rm = TRUE),
    .calls = function() calls,
    .nodes = function() nodes
  )
}

fake_hierarchy_db <- function() {
  nodes <- data.frame(
    id = c(1L, 2L, 3L, 4L),
    parent_id_raw = c(NA_integer_, 1L, 2L, NA_integer_),
    organisatorisk_navn_teknisk = c("rod_a", "barn_b", "barn_c", "rod_d"),
    organisatorisk_navn_langt = c("Rod A", "Barn B", "Barn C", "Rod D"),
    organisatorisk_navn_kort = c("A", "B", "C", "D"),
    niveau_id = c(10L, 20L, 10L, 10L),
    niveau_num = c(1L, 2L, 1L, 1L),
    niveau_navn = c("Direktion", "Afdeling", "Direktion", "Direktion"),
    stringsAsFactors = FALSE)
  niveauer <- data.frame(id = c(10L, 20L),
                         label = c("Direktion", "Afdeling"),
                         stringsAsFactors = FALSE)
  calls <- list(created = NULL, updated = NULL, updates = list(), deleted = NULL)
  update_error <- FALSE
  delete_error <- FALSE
  list(
    list_nodes = function() nodes,
    niveau_options = function() niveauer,
    create_node = function(values) { calls$created <<- values; 99L },
    update_node = function(id, values) {
      call <- list(id = as.integer(id), values = values)
      calls$updated <<- call
      calls$updates[[length(calls$updates) + 1L]] <<- call
      if (isTRUE(update_error)) stop("Forceret update-fejl")

      row <- match(as.integer(id), nodes$id)
      if (is.na(row)) stop("Node ikke fundet")
      for (col in names(values)) {
        node_col <- switch(col,
          parent_Id = "parent_id_raw",
          organisatorisk_niveau = "niveau_id",
          col)
        if (node_col %in% names(nodes)) nodes[[node_col]][row] <<- values[[col]]
      }
      niveau_row <- match(nodes$niveau_id[row], niveauer$id)
      nodes$niveau_num[row] <<- if (is.na(niveau_row)) NA_integer_ else niveau_row
      nodes$niveau_navn[row] <<- if (is.na(niveau_row)) NA_character_ else
        niveauer$label[niveau_row]
      1L
    },
    delete_node = function(id) {
      calls$deleted <<- as.integer(id)
      if (isTRUE(delete_error)) stop("foreign key constraint")
      row <- match(as.integer(id), nodes$id)
      if (is.na(row)) stop("Node ikke fundet")
      nodes <<- nodes[-row, , drop = FALSE]
      1L
    },
    child_count = function(id) sum(nodes$parent_id_raw %in% id, na.rm = TRUE),
    .calls = function() calls,
    .nodes = function() nodes,
    .set_update_error = function(value) update_error <<- isTRUE(value),
    .set_delete_error = function(value) delete_error <<- isTRUE(value)
  )
}
