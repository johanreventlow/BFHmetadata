# Fixture: lager med nested gruppe/indikator (dags-partitioneret), en
# top-level indikator (flad parquet) og støj der skal ignoreres.
# Deles af test-compact.R og test-mod-compact.R.
make_store_fixture <- function(env = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = env)
  # Nested: gruppe/ind_a med dags-partitioner (som sundk_databaser/...)
  for (d in c("2020-01-01", "2020-01-02", "2020-01-03")) {
    p <- file.path(base, "gruppe", "ind_a", paste0("dato=", d))
    dir.create(p, recursive = TRUE)
    arrow::write_parquet(
      data.frame(vaerdi = 1, enhed = "e", stringsAsFactors = FALSE),
      file.path(p, "part-0.parquet"))
  }
  # Top-level: ind_b med én flad parquet (som akutte_genindlaeggelser-stil)
  dir.create(file.path(base, "ind_b"))
  arrow::write_parquet(data.frame(
    dato = as.Date("2020-01-01") + 0:5, vaerdi = 1:6, enhed = "e",
    stringsAsFactors = FALSE), file.path(base, "ind_b", "p.parquet"))
  # Støj: tom gruppe, og et eksisterende _compact der ALDRIG må enumereres
  dir.create(file.path(base, "tom_gruppe"))
  dir.create(file.path(base, "_compact", "gammel"), recursive = TRUE)
  base
}
