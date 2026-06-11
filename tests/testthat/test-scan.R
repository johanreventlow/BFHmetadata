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
