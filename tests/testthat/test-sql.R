test_that("build_list_sql joiner alle 3 FK-parents med labels", {
  sql <- build_list_sql()
  expect_match(sql, 'FROM "tblIndikatorer"')
  expect_match(sql, '"tblIndikatorHierarki"')
  expect_match(sql, '"tblPersoner"')
  expect_match(sql, '"tblDatakilder"')
  expect_match(sql, "hierarki_navn")
  expect_match(sql, "datakilde_navn")
})

test_that("build_fk_options_sql bygger id+label select for parent", {
  sql <- build_fk_options_sql("tblDatakilder", "datakilde_navn")
  expect_match(sql, '"Id"')
  expect_match(sql, "datakilde_navn")
  expect_match(sql, 'FROM "tblDatakilder"')
})

test_that("build_update_sql bruger parametriserede placeholders", {
  res <- build_update_sql(c("indikator_navn", "mål"))
  expect_match(res, 'UPDATE "tblIndikatorer" SET')
  expect_match(res, '"indikator_navn" = \\$1')
  expect_match(res, '"mål" = \\$2')
  expect_match(res, 'WHERE "id" = \\$3')
})

test_that("build_insert_sql returnerer RETURNING id", {
  res <- build_insert_sql(c("indikator_navn", "datakilde"))
  expect_match(res, 'INSERT INTO "tblIndikatorer"')
  expect_match(res, "RETURNING \"id\"")
  expect_match(res, "\\$1, \\$2")
})
