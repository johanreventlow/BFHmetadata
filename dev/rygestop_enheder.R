# Engangs-diagnostik: hvilke enheder er der RÅDATA for på indikatoren
# "Henvist til rygestopkursus" — og hvordan matcher de org-strukturen og de
# eksisterende diagram-rækker? Ren læsning: skriver INTET i databasen.
#
# Kørsel (kræver BFHmetadata installeret + SUPABASE_DB_PASSWORD i .Renviron):
#   Rscript dev/rygestop_enheder.R /sti/til/parquet-rod
# eller i R/RStudio:
#   source("dev/rygestop_enheder.R")   # bruger sidst anvendte parquet-sti
#
# Output er tre tabeller:
#   1) Enheder i rådata + match mod org-struktur + eksisterende diagram-række
#   2) Enheder i rådata UDEN org-match (kræver en Organisations-oversættelse)
#   3) Diagram-rækker for indikatoren UDEN rådata (til orientering)
# Kopiér hele outputtet tilbage til Claude, så kan de manglende rækker oprettes.

IND_TEKNISK <- "henvist_til_rygestopkursus"
IND_ID <- 2214L

`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = TRUE)
base <- if (length(args) >= 1 && nzchar(args[[1]])) {
  args[[1]]
} else {
  tryCatch(BFHmetadata:::last_parquet_dir_read(), error = function(e) NULL)
}
if (is.null(base) || !dir.exists(base)) {
  stop("Angiv parquet-rodmappen: Rscript dev/rygestop_enheder.R /sti/til/parquet",
       call. = FALSE)
}

# --- Rådata (læses som appen: samme sti-opslag og arrow-load) ----------------
sti <- BFHmetadata:::parquet_indicator_path(base, IND_TEKNISK)
slice <- BFHmetadata:::parquet_load_slice(sti)
if (is.null(slice) || nrow(slice) == 0) {
  stop(sprintf("Ingen parquet-data fundet under: %s", sti), call. = FALSE)
}
slice$dato <- as.Date(slice$dato)

har_naevner <- "naevner" %in% names(slice) && any(!is.na(slice$naevner))
agg <- do.call(rbind, lapply(split(slice, tolower(slice$enhed)), function(g) {
  data.frame(
    enhed = g$enhed[[1]],
    n_raekker = nrow(g),
    fra = min(g$dato), til = max(g$dato),
    sum_taeller = if ("taeller" %in% names(g)) sum(g$taeller, na.rm = TRUE) else NA,
    sum_naevner = if (har_naevner) sum(g$naevner, na.rm = TRUE) else NA,
    stringsAsFactors = FALSE
  )
}))
rownames(agg) <- NULL

# --- DB-opslag (org-navne, oversættelser, eksisterende rækker) ---------------
pool <- BFHmetadata:::db_connect()
on.exit(pool::poolClose(pool), add = TRUE)

orgs <- DBI::dbGetQuery(pool, paste0(
  'SELECT o."Id" AS org_id, o."organisatorisk_niveau" AS niveau, ',
  'o."parent_Id" AS parent_id, ',
  'COALESCE(o."organisatorisk_navn_langt", o."organisatorisk_navn_teknisk") ',
  'AS org_navn FROM "tblOrganisationStruktur" o'))
variants <- DBI::dbGetQuery(pool,
  BFHmetadata:::build_org_enhed_variants_sql())
# Én række pr. org (der KAN findes flere diagram-rækker pr. org/indikator —
# fx forskellige diagramtyper): id'er samles, flag som bool_or.
diagrammer <- DBI::dbGetQuery(pool, paste0(
  'SELECT d."organisatorisk_navn_teknisk" AS org_id, ',
  "string_agg(d.\"id\"::text, ',') AS diagram_id, ",
  'bool_or(d."diagram_aktivt") AS aktiv, ',
  'bool_or(d."indgaar_i_aggregering") AS indgaar ',
  'FROM "tblDiagrammer" d WHERE d."indikator" = ', IND_ID,
  ' GROUP BY d."organisatorisk_navn_teknisk"'))

# Lowercase navn -> org_id (samme match-grundlag som enhed_variants_for):
# teknisk/kort/langt + alle "fra data"-oversættelser.
navne <- rbind(
  data.frame(navn = variants$teknisk,  org_id = variants$org_id),
  data.frame(navn = variants$kort,     org_id = variants$org_id),
  data.frame(navn = variants$langt,    org_id = variants$org_id),
  data.frame(navn = variants$fra_data, org_id = variants$org_id))
navne <- navne[!is.na(navne$navn) & nzchar(navne$navn), , drop = FALSE]
navne$navn <- tolower(navne$navn)
navne <- unique(navne)

match_org <- function(enhed) {
  hits <- unique(navne$org_id[navne$navn == tolower(enhed)])
  if (length(hits) == 0) return(NA_integer_)
  if (length(hits) > 1) return(-1L) # flertydig — skal afklares manuelt
  hits
}

agg$org_id <- vapply(agg$enhed, match_org, integer(1))
agg <- merge(agg, orgs, by = "org_id", all.x = TRUE, sort = FALSE)
agg <- merge(agg, diagrammer, by = "org_id", all.x = TRUE, sort = FALSE)
agg$har_diagramraekke <- !is.na(agg$diagram_id)
agg <- agg[order(is.na(agg$org_id), agg$niveau, agg$enhed), c(
  "enhed", "n_raekker", "fra", "til", "sum_taeller", "sum_naevner",
  "org_id", "org_navn", "niveau", "har_diagramraekke", "aktiv", "indgaar"), ]

uden_match <- agg[is.na(agg$org_id) | agg$org_id == -1L, , drop = FALSE]

# Diagram-rækker uden rådata (orientering: fx aggregat-niveauer — helt ok)
data_org_ids <- agg$org_id[!is.na(agg$org_id) & agg$org_id > 0]
uden_data <- merge(
  diagrammer[!diagrammer$org_id %in% data_org_ids, , drop = FALSE],
  orgs, by = "org_id", all.x = TRUE, sort = FALSE)

options(width = 220)
cat("\n=== 1) Enheder i rådata (", IND_TEKNISK, ") ===\n", sep = "")
print(agg, row.names = FALSE)
cat("\n=== 2) Enheder UDEN org-match (kræver Organisations-oversættelse;",
    "org_id -1 = flertydigt navn) ===\n")
if (nrow(uden_match) == 0) cat("(ingen)\n") else
  print(uden_match[, c("enhed", "n_raekker", "sum_taeller")], row.names = FALSE)
cat("\n=== 3) Diagram-rækker uden egne rådata (typisk aggregat-niveauer) ===\n")
if (nrow(uden_data) == 0) cat("(ingen)\n") else
  print(uden_data[, c("diagram_id", "org_id", "org_navn", "niveau",
                      "aktiv", "indgaar")], row.names = FALSE)
cat("\nKopiér hele outputtet ovenfor tilbage til Claude.\n")
