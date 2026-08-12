# Kontrakt-tests: pinner BFHddl's oprulnings-semantik (DATA_CONVENTIONS §4-6,
# db_organisations.R). Fejler disse efter en fremtidig ændring, er vendored
# kode drevet fra BFHddl — synkronisér med kilden.

.os <- function(...) {
  # org_struct-helper: byg df af (id, parent_id)-par
  m <- matrix(c(...), ncol = 2, byrow = TRUE)
  data.frame(id = as.integer(m[, 1]), parent_id = as.integer(m[, 2]))
}
.fl <- function(...) {
  # agg_flags-helper: byg df af (org_id, indikator_id, indgaar)-tripler
  m <- matrix(c(...), ncol = 3, byrow = TRUE)
  data.frame(org_id = as.integer(m[, 1]), indikator_id = as.integer(m[, 2]),
             indgaar = as.logical(m[, 3]))
}

test_that("find_aggregation_children: flagede boern bidrager", {
  os <- .os(2, 1,  3, 1)                       # 1 har boern 2 og 3
  fl <- .fl(2, 9, TRUE,  3, 9, TRUE)
  expect_setequal(find_aggregation_children(1L, 9L, os, fl), c(2L, 3L))
})

test_that("find_aggregation_children: FALSE-flag ekskluderer hele grenen", {
  os <- .os(2, 1,  4, 2)                       # 1 -> 2 -> 4
  fl <- .fl(2, 9, FALSE,  4, 9, TRUE)          # 2 er FALSE, 4 er TRUE
  expect_length(find_aggregation_children(1L, 9L, os, fl), 0L)
})

test_that("find_aggregation_children: gennemfald returnerer mellemniveauet (ikke leaf)", {
  # BFHddl-kilden (find_aggregation_children) resolver KUN eet niveau: et
  # uregistreret mellemniveau returneres selv som "barn", naar der findes en
  # flagget efterkommer laengere nede (afgjort af .has_flagged_descendant).
  # Den fulde nedstigning til data-baerende blade sker i den rekursive
  # kalder (data_aggregate_children_recursive i BFHddl - ikke vendored her),
  # ikke i find_aggregation_children selv. Se db_organisations.R:199-201.
  os <- .os(2, 1,  4, 2)                       # 1 -> 2 -> 4; 2 har INGEN raekke
  fl <- .fl(4, 9, TRUE)
  expect_equal(find_aggregation_children(1L, 9L, os, fl), 2L)
})

test_that("find_aggregation_children: TRUE-barn terminerer grenen (ingen dobbelttaelling)", {
  os <- .os(2, 1,  4, 2)                       # 1 -> 2 -> 4; baade 2 og 4 TRUE
  fl <- .fl(2, 9, TRUE,  4, 9, TRUE)
  expect_equal(find_aggregation_children(1L, 9L, os, fl), 2L)   # KUN 2
})

test_that("find_aggregation_children: andre indikatorers flag ignoreres; ingen boern -> tom", {
  os <- .os(2, 1)
  fl <- .fl(2, 8, TRUE)                        # flag paa ANDEN indikator
  expect_length(find_aggregation_children(1L, 9L, os, fl), 0L)
  expect_length(find_aggregation_children(99L, 9L, os, fl), 0L)  # ingen boern
})

test_that("find_aggregation_children: max_depth begraenser gennemfald-soegningen", {
  # find_aggregation_children returnerer selv kun det direkte strukturelle
  # barn (2) - max_depth styrer hvor dybt .has_flagged_descendant maa
  # soege efter en flagget efterkommer for at afgoere om 2 kvalificerer.
  os <- .os(2, 1,  3, 2,  4, 3)                # 1 -> 2 -> 3 -> 4 (kun 4 flaget)
  fl <- .fl(4, 9, TRUE)
  expect_equal(find_aggregation_children(1L, 9L, os, fl), 2L)
  expect_length(find_aggregation_children(1L, 9L, os, fl, max_depth = 1L), 0L)
})

test_that("aggregate_child_data: summerer pr. dato med na.rm=FALSE", {
  d <- data.frame(
    dato = as.Date(c("2024-01-01", "2024-01-01", "2024-02-01", "2024-02-01")),
    taeller = c(2, 3, 4, NA), naevner = c(10, 10, 10, 10),
    enhed = c("a", "b", "a", "b"))
  out <- aggregate_child_data(d, center_enhed = "center")
  out <- out[order(out$dato), ]
  expect_equal(out$taeller, c(5, NA))          # NA smitter (na.rm = FALSE)
  expect_equal(out$naevner, c(20, 20))
  expect_true(all(out$enhed == "center"))
})

test_that("aggregate_child_data: ikke-overlappende datoer = det ene barns vaerdi", {
  d <- data.frame(dato = as.Date(c("2024-01-01", "2024-02-01")),
                  taeller = c(2, 7), enhed = c("a", "b"))
  out <- aggregate_child_data(d, center_enhed = "c")
  expect_equal(out$taeller[order(out$dato)], c(2, 7))
  expect_false("naevner" %in% names(out))      # ingen naevner-kolonne ind -> ingen ud
})
