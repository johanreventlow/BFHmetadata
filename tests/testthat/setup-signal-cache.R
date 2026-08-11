# Testkørsler må ALDRIG skrive til brugerens rigtige dags-cache (R_user_dir):
# peg bfhmeta.cache_dir på en engangs-mappe for hele suiten.
withr::local_options(
  list(bfhmeta.cache_dir = withr::local_tempdir(.local_envir = teardown_env())),
  .local_envir = teardown_env()
)

# Kør ventende later-callbacks (progressivt scan) til køen er tom, så tests
# deterministisk kan vente på at et scan er kørt færdigt.
drain_scan <- function() {
  while (!later::loop_empty()) later::run_now()
}
