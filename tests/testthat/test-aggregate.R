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

test_that("find_aggregation_children: indgaar=NA paa mellemniveau blokerer gennemfald", {
  # Test-gap fra quality review: en eksplicit registreret raekke med NA
  # (ikke kun FALSE) skal ogsaa ekskludere hele grenen - koden bruger
  # `any(rows$indgaar %in% TRUE)` som er FALSE for NA, saa dette pinnes
  # her som kontrakt.
  os <- .os(2, 1,  4, 2)                       # 1 -> 2 -> 4
  fl <- .fl(2, 9, NA,  4, 9, TRUE)
  expect_length(find_aggregation_children(1L, 9L, os, fl), 0L)
})

# --- aggregate_slice_for_center ---------------------------------------

.variants <- function(...) {
  do.call(rbind, lapply(list(...), function(x) data.frame(
    org_id = x[[1]], teknisk = x[[2]], kort = NA, langt = NA,
    fra_data = if (length(x) > 2) x[[3]] else NA, stringsAsFactors = FALSE)))
}

test_that("aggregate_slice_for_center: fladt tilfaelde - to TRUE-boern med data", {
  os <- .os(2, 1,  3, 1)
  fl <- .fl(2, 9, TRUE,  3, 9, TRUE)
  vr <- .variants(list(2L, "afsnit_a"), list(3L, "afsnit_b"))
  slice <- data.frame(
    dato = as.Date(c("2024-01-01", "2024-01-01", "2024-02-01", "2024-02-01")),
    enhed = c("afsnit_a", "afsnit_b", "afsnit_a", "afsnit_b"),
    taeller = c(1, 2, 3, 4), naevner = c(10, 10, 10, 10))
  out <- aggregate_slice_for_center(slice, 1L, 9L, "center", os, fl, vr)
  out <- out[order(out$dato), ]
  expect_equal(out$taeller, c(3, 7))
  expect_equal(out$naevner, c(20, 20))
  expect_true(all(out$enhed == "center"))
})

test_that("aggregate_slice_for_center: rekursivt gennemfald - 1 -> 2 (ingen data) -> 4,5", {
  os <- .os(2, 1,  4, 2,  5, 2)                # 1 -> 2 -> {4,5}; 2 har INGEN flag-raekke
  fl <- .fl(4, 9, TRUE,  5, 9, TRUE)
  vr <- .variants(list(2L, "overafd"), list(4L, "afsnit_4"), list(5L, "afsnit_5"))
  slice <- data.frame(
    dato = as.Date(c("2024-01-01", "2024-01-01")),
    enhed = c("afsnit_4", "afsnit_5"),
    taeller = c(3, 5))
  out <- aggregate_slice_for_center(slice, 1L, 9L, "center", os, fl, vr)
  expect_equal(out$taeller, 8)
  expect_true(all(out$enhed == "center"))
})

test_that("aggregate_slice_for_center: TRUE-barn uden egne data men med flagede boernaeboern", {
  # Kilde-semantik (data_aggregate_children_recursive linje 820): rekursion
  # sker naar barnet mangler EGNE data, uanset OM barnet selv var TRUE-
  # flagget eller et gennemfald-mellemniveau. 2 er TRUE men findes ikke i
  # slicet -> koden forsoeger rekursivt at hente 2's boern (4).
  os <- .os(2, 1,  4, 2)
  fl <- .fl(2, 9, TRUE,  4, 9, TRUE)
  vr <- .variants(list(2L, "overafd"), list(4L, "afsnit_4"))
  slice <- data.frame(
    dato = as.Date("2024-01-01"), enhed = "afsnit_4", taeller = 6)
  out <- aggregate_slice_for_center(slice, 1L, 9L, "center", os, fl, vr)
  expect_equal(out$taeller, 6)
  expect_true(all(out$enhed == "center"))
})

test_that("aggregate_slice_for_center: NULL-tilfaelde", {
  os <- .os(2, 1)
  fl_false <- .fl(2, 9, FALSE)
  vr <- .variants(list(2L, "afsnit_a"))
  slice <- data.frame(dato = as.Date("2024-01-01"), enhed = "afsnit_a", taeller = 1)

  # FALSE-flag -> ingen boern -> NULL
  expect_null(aggregate_slice_for_center(slice, 1L, 9L, "center", os, fl_false, vr))

  # bidragyder uden data i slicet og uden boern -> NULL
  fl_true <- .fl(2, 9, TRUE)
  tom_slice <- data.frame(dato = as.Date("2024-01-01"), enhed = "andet", taeller = 1)
  expect_null(aggregate_slice_for_center(tom_slice, 1L, 9L, "center", os, fl_true, vr))

  # vaerdi-only slice (ingen taeller-kolonne) -> NULL
  vaerdi_slice <- data.frame(dato = as.Date("2024-01-01"), enhed = "afsnit_a", vaerdi = 1)
  expect_null(aggregate_slice_for_center(vaerdi_slice, 1L, 9L, "center", os, fl_true, vr))

  # taeller-kolonne findes men er 100% NA -> NULL
  na_slice <- data.frame(dato = as.Date("2024-01-01"), enhed = "afsnit_a", taeller = NA_real_)
  expect_null(aggregate_slice_for_center(na_slice, 1L, 9L, "center", os, fl_true, vr))

  # NULL full_slice/org_struct/agg_flags -> NULL
  expect_null(aggregate_slice_for_center(NULL, 1L, 9L, "center", os, fl_true, vr))
  expect_null(aggregate_slice_for_center(slice, 1L, 9L, "center", NULL, fl_true, vr))
  expect_null(aggregate_slice_for_center(slice, 1L, 9L, "center", os, NULL, vr))
})

test_that("aggregate_slice_for_center: max_depth begraenser rekursionen", {
  os <- .os(2, 1,  3, 2,  4, 3)                # 1 -> 2 -> 3 -> 4 (kun 4 TRUE, data paa 4)
  fl <- .fl(4, 9, TRUE)
  vr <- .variants(list(2L, "niv2"), list(3L, "niv3"), list(4L, "niv4"))
  slice <- data.frame(dato = as.Date("2024-01-01"), enhed = "niv4", taeller = 9)

  expect_null(aggregate_slice_for_center(slice, 1L, 9L, "center", os, fl, vr, max_depth = 1L))

  out <- aggregate_slice_for_center(slice, 1L, 9L, "center", os, fl, vr)
  expect_equal(out$taeller, 9)
  expect_true(all(out$enhed == "center"))
})

test_that("aggregate_slice_for_center: cyklisk org-trae terminerer rent (.visited-vaern)", {
  # Malformet org_struct med et 2<->3-kredslob under center 1. Kilden
  # (data_aggregate_children_recursive) short-circuiter allerede besoegte
  # noder via .visited; denne adapter skal terminere lige saa rent - uden
  # fejl/uendelig rekursion - og give NULL, fordi ingen enhed i cyklussen
  # har data i slicet (kun flaget, aldrig egne raekker).
  os <- .os(2, 1,  3, 2,  2, 3)                # 1 -> 2 -> 3 -> 2 (cyklus 2<->3)
  fl <- .fl(3, 9, TRUE)                        # 3 er TRUE, men har ingen data
  vr <- .variants(list(2L, "niv2"), list(3L, "niv3"))
  slice <- data.frame(dato = as.Date("2024-01-01"), enhed = "andet", taeller = 1)

  expect_null(aggregate_slice_for_center(slice, 1L, 9L, "center", os, fl, vr))
})
