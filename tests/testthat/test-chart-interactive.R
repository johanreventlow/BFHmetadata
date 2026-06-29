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
