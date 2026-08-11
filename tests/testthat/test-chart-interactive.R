test_that("interactive_run_chart returnerer girafe-htmlwidget med dato-data_id", {
  d <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                  vaerdi = c(rep(10, 12), rep(2, 12)), naevner = NA_real_)
  sig <- compute_signal(d, parts = 13L)
  g <- interactive_run_chart(sig$qic_result)
  expect_s3_class(g, "girafe")
  # data_id-strenge (ISO-datoer) skal optræde i den genererede SVG
  svg <- as.character(g$x$html)
  expect_match(svg, "2020-01-01")
})

test_that("interactive_run_chart: valgt dato (rigtigt datapunkt) fremhæves uden fejl", {
  d <- data.frame(dato = as.Date("2020-01-01") + 0:23 * 30,
                  vaerdi = c(rep(10, 12), rep(2, 12)), naevner = NA_real_)
  sig <- compute_signal(d, parts = 13L)
  # 2020-07-29 ER et datapunkt (offset 210 = 7*30) → highlight-grenen kører
  expect_s3_class(
    interactive_run_chart(sig$qic_result, selected_date = "2020-07-29"),
    "girafe")
})

# Stub der efterligner bfh_qic_result-strukturen (interactive_run_chart bruger
# kun $qic_data) — til degenererede tilfælde som bfh_qic ikke selv kan bygge.
.fake_qic <- function(y, cl = NULL, n = length(y)) {
  list(qic_data = data.frame(
    x = as.Date("2020-01-01") + seq_len(n) * 30,
    y = y,
    cl = rep(cl %||% stats::median(y, na.rm = TRUE), length.out = n),
    part = rep(1, n), anhoej.signal = rep(FALSE, n)))
}

test_that("interactive_run_chart: y helt uden endelige værdier → NULL, ALDRIG fejl", {
  # Produktions-crash: degenereret qic_data (min→Inf-advarsler) væltede
  # girafe-bygningen med 'diff.default(continuous_range_coord)' → Browse[1]
  # i dev. En graf uden tegnbare punkter skal give NULL (kalderen viser
  # venlig besked), aldrig en fejl.
  expect_null(interactive_run_chart(.fake_qic(rep(NA_real_, 6), cl = NA_real_)))
  expect_null(interactive_run_chart(.fake_qic(rep(Inf, 6), cl = NA_real_)))
  # Tom qic_data (0 rækker) må heller aldrig fejle
  fq <- .fake_qic(numeric(0), cl = numeric(0), n = 0)
  expect_null(interactive_run_chart(fq))
})

test_that("interactive_run_chart: enkelte NA/Inf-punkter droppes, resten tegnes", {
  g <- interactive_run_chart(.fake_qic(c(4, NA, 6, Inf, 5, 4), cl = 5))
  expect_s3_class(g, "girafe")
  svg <- as.character(g$x$html)
  expect_match(svg, "2020-01-31")            # første endelige punkt er med
})

test_that("interactive_run_chart: ét enkelt punkt og cl helt NA → tegnes uden fejl", {
  expect_s3_class(interactive_run_chart(.fake_qic(5, cl = NA_real_, n = 1)),
                  "girafe")
  # cl helt NA på flerpunkts-serie: median-laget droppes, punkterne tegnes
  expect_s3_class(interactive_run_chart(.fake_qic(c(4, 5, 6), cl = NA_real_)),
                  "girafe")
})
