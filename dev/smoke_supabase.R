# Manuel: verificér read + write-round-trip mod Supabase. KRÆVER .Renviron + BFHMETA_WRITE=1.
pkgload::load_all(".", helpers = FALSE)
pool <- db_connect(); on.exit(pool::poolClose(pool))
db <- make_db(pool)
cat("Antal indikatorer:", nrow(db$list_indikatorer()), "\n")
cat("FK-options datakilde:\n"); print(utils::head(db$fk_options()$datakilde, 3))
# Write-round-trip (kræver BFHMETA_WRITE=1):
if (write_enabled()) {
  id <- db$create_indikator(list(indikator_navn = "__smoke__", aktiv_indikator = TRUE))
  cat("Oprettet id:", id, "\n")
  db$soft_delete(id, FALSE); cat("Soft-deleted\n")
  DBI::dbExecute(pool, 'DELETE FROM "tblIndikatorer" WHERE "id"=$1', params = list(id))
  cat("Oprydning: hard-deleted smoke-række\n")
} else cat("BFHMETA_WRITE ej sat — springer write-test over\n")
