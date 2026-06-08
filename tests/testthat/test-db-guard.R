test_that("write_enabled er FALSE som default", {
  withr::with_envvar(c(BFHMETA_WRITE = ""), {
    withr::with_options(list(bfhmeta.write_enabled = NULL), {
      expect_false(write_enabled())
    })
  })
})

test_that("write_enabled TRUE via env eller option", {
  withr::with_envvar(c(BFHMETA_WRITE = "1"), expect_true(write_enabled()))
  withr::with_envvar(c(BFHMETA_WRITE = ""), {
    withr::with_options(list(bfhmeta.write_enabled = TRUE),
                        expect_true(write_enabled()))
  })
})

test_that("assert_write_enabled fejler når disabled", {
  withr::with_envvar(c(BFHMETA_WRITE = ""), {
    withr::with_options(list(bfhmeta.write_enabled = NULL), {
      expect_error(assert_write_enabled(), "skrivning")
    })
  })
})
