# =============================================================================
# dev/bulk_probe.R — Fase 0-verifikation. Køres manuelt, IKKE del af pakken.
# =============================================================================
# Beviser to ting før bulk bygges:
#   V1: hvordan RPostgres binder en vektor af id'er
#   V2: at SELECT ... FOR UPDATE faktisk låser mod en anden forbindelse
#
# Kør:  BFHMETA_WRITE=1 Rscript dev/bulk_probe.R
# Skriver ALDRIG til rigtige tabeller — kun til en unikt navngivet engangstabel
# (navnet inkluderer PID + timestamp, så to samtidige kørsler ikke kan droppe
# hinandens tabel, og en efterladt tabel er nem at identificere manuelt).
#
# NB 1: hele proben ligger i main() og kaldes til sidst. on.exit() registrerer
# oprydning på den *aktive funktions call frame* — på topniveau i et Rscript
# (uden for enhver funktion) gør on.exit() derfor INTET, og hverken
# dbDisconnect() eller DROP TABLE ville nogensinde køre. Wrapperen er
# rettelsen, ikke et stilvalg.
#
# NB 2: Garanti-forbehold. on.exit() dækker normalt scriptophør, stop()/fejl
# og Ctrl-C, men IKKE et voldsomt procesafbrud (SIGKILL, OOM-kill,
# maskinnedbrud e.l.) — R får i så fald ingen chance for at køre exit-
# handlere overhovedet. Sker det, kan tabellen (_bulk_probe_<pid>_<ts>)
# blive efterladt i databasen. Det unikke navn gør den nem at finde og
# droppe manuelt bagefter; det er ikke noget scriptet selv kan afbøde.
# =============================================================================

suppressMessages({library(DBI); library(RPostgres)})

if (!identical(Sys.getenv("BFHMETA_WRITE"), "1")) {
  stop("BFHMETA_WRITE=1 skal være sat for at køre denne probe (den skriver ",
       "til databasen, om end kun til sin egen engangstabel). Kør:\n",
       "  BFHMETA_WRITE=1 Rscript dev/bulk_probe.R")
}

main <- function() {
  conn <- DBI::dbConnect(RPostgres::Postgres(),
    host = "aws-0-eu-west-1.pooler.supabase.com", port = 5432,
    dbname = "postgres", user = "postgres.ijgwlqpbjcfffdmxeahh",
    password = Sys.getenv("SUPABASE_DB_PASSWORD"), sslmode = "require")
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Unikt navn pr. kørsel (PID + timestamp) — undgår kollision mellem
  # samtidige kørsler og gør en evt. efterladt tabel identificerbar.
  TBL <- sprintf("_bulk_probe_%d_%d", Sys.getpid(), as.integer(Sys.time()))
  TBL_Q <- sprintf('"public"."%s"', TBL)   # eksplicit skema-kvalificeret, quoted

  # Defensiv oprydning af en evt. tidligere efterladt tabel med samme navn.
  # Praktisk talt umuligt med det unikke navn, men billigt og harmløst.
  DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", TBL_Q))

  # Oprydningen registreres FØR CREATE TABLE (ikke bagefter), så et afbrud i
  # vinduet mellem CREATE og registrering ikke kan efterlade tabellen uden en
  # on.exit-handler klar til at rydde op.
  #
  # Oprydningen sker på en FRISK, separat forbindelse — ikke på `conn` eller
  # `c2`. Begrundelse:
  #   1) V2 åbner en transaktion på `conn` (dbBegin/dbRollback). Fejler noget
  #      uventet i det vindue, EFTER at FOR UPDATE har taget sin lås, men FØR
  #      dbRollback(conn) når at køre, står `conn` tilbage i "aborted" state
  #      MENS den stadig holder ROW SHARE-låsen (transaktionen er aborted,
  #      men ikke afsluttet — kun ROLLBACK/COMMIT/disconnect frigiver låsen).
  #      DROP TABLE kræver ACCESS EXCLUSIVE og ville derfor blokere permanent
  #      mod den lås, hvis DROP forsøgtes på en frisk forbindelse UDEN først
  #      at rulle `conn` (og `c2`, af samme grund) tilbage.
  #   2) Derfor: rul `conn` og `c2` tilbage FØRST (try(), silent — de har
  #      måske ingen åben transaktion, hvilket ellers selv ville fejle), og
  #      brug SÅ en frisk forbindelse til selve DROP.
  #   3) `cleanup_conn` får eksplicit lock_timeout + statement_timeout, så en
  #      uventet resterende konflikt fejler højlydt i stedet for at hænge.
  on.exit({
    t0 <- Sys.time()
    try(DBI::dbRollback(conn), silent = TRUE)
    if (exists("c2", inherits = FALSE)) try(DBI::dbRollback(c2), silent = TRUE)

    cleanup_conn <- tryCatch(
      DBI::dbConnect(RPostgres::Postgres(),
        host = "aws-0-eu-west-1.pooler.supabase.com", port = 5432,
        dbname = "postgres", user = "postgres.ijgwlqpbjcfffdmxeahh",
        password = Sys.getenv("SUPABASE_DB_PASSWORD"), sslmode = "require"),
      error = function(e) {
        warning("Oprydning: kunne ikke åbne oprydningsforbindelse: ", conditionMessage(e))
        NULL
      })
    if (!is.null(cleanup_conn)) {
      try(DBI::dbExecute(cleanup_conn, "SET lock_timeout = '5s'"), silent = TRUE)
      try(DBI::dbExecute(cleanup_conn, "SET statement_timeout = '10s'"), silent = TRUE)
      tryCatch(
        DBI::dbExecute(cleanup_conn, sprintf("DROP TABLE IF EXISTS %s", TBL_Q)),
        error = function(e) warning("Oprydning: DROP TABLE fejlede: ", conditionMessage(e)))
      DBI::dbDisconnect(cleanup_conn)
    }
    cat(sprintf("[oprydning] fuldført på %.3f sekunder\n",
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }, add = TRUE, after = FALSE)

  DBI::dbExecute(conn, sprintf("CREATE TABLE %s (id int primary key, v text, ver int not null default 0)", TBL_Q))
  DBI::dbExecute(conn, sprintf("INSERT INTO %s SELECT g, 'start', 0 FROM generate_series(1,5) g", TBL_Q))

  ids <- c(1L, 2L, 3L)

  cat("\n=== V1a: params = list(vektor) mod ÉN fast række — tæller faktiske eksekveringer ===\n")
  # dbExecute() returnerer SUMMEN af berørte rækker, ikke antal eksekveringer
  # — 3 kan lige så godt betyde "3 eksekveringer x 1 række" som "1 eksekvering
  # x 3 rækker". For at skelne rammer hver eksekvering den SAMME ene række
  # (id = 1); $1 bruges kun til en IS NOT NULL-betingelse, så værdien er
  # ligegyldig, men VEKTORLÆNGDEN afgør antal eksekveringer. Kun 3 separate
  # eksekveringer kan give 3 berørte rækker, når hver eksekvering højst kan
  # ramme én (den samme) række. `ver`-tælleren er en uafhængig kontrol: den
  # kan kun nå 3 via tre sekventielle read-modify-write-eksekveringer.
  sql_a <- sprintf("UPDATE %s SET v = 'a', ver = ver + 1 WHERE id = 1 AND $1::int IS NOT NULL", TBL_Q)
  n_a <- DBI::dbExecute(conn, sql_a, params = list(ids))
  ver_a <- DBI::dbGetQuery(conn, sprintf("SELECT ver FROM %s WHERE id = 1", TBL_Q))$ver
  cat("SQL:", sql_a, "\n")
  cat("dbExecute returnerede (summerede berørte rækker):", n_a, "\n")
  cat("ver for id=1 efter kørsel (uafhængig tælling af faktiske eksekveringer):", ver_a, "\n")
  stopifnot(
    "V1a: forventede 3 berørte rækker i alt (3 separate eksekveringer x 1 fast række)" = n_a == 3,
    "V1a: ver-tælleren bekræfter ikke 3 separate UPDATE-eksekveringer" = ver_a == 3)
  cat("BEKRÆFTET: 3 separate eksekveringer (rows-affected OG ver-tælleren stemmer begge til 3)\n")

  cat("\n=== V1b: array-literal + cast ===\n")
  id_array <- paste0("{", paste(sort(unique(ids)), collapse = ","), "}")
  n_b <- DBI::dbExecute(conn, sprintf("UPDATE %s SET v = 'b' WHERE id = ANY($1::int[])", TBL_Q),
                        params = list(id_array))
  cat("dbExecute returnerede:", n_b, "— array-literal:", id_array, "\n")
  stopifnot("V1b: forventede 3 berørte rækker fra én eksekvering (array-literal)" = n_b == 3)

  cat("\n=== V1c: dynamiske placeholders (designets valgte mønster B) ===\n")
  ph <- paste(sprintf("$%d", seq_along(ids) + 1L), collapse = ", ")
  sql_c <- sprintf("UPDATE %s SET v = $1 WHERE id IN (%s)", TBL_Q, ph)
  n_c <- DBI::dbExecute(conn, sql_c, params = c(list("c"), as.list(ids)))
  cat("SQL:", sql_c, "\n")
  cat("dbExecute returnerede:", n_c, "\n")
  stopifnot("V1c: forventede 3 berørte rækker fra én eksekvering (dynamiske placeholders)" = n_c == 3)

  cat("\n=== V1d: den ugyldige kombination (skalar + vektor) ===\n")
  res_d <- tryCatch({
    DBI::dbExecute(conn, sprintf("UPDATE %s SET v = $1 WHERE id = $2", TBL_Q),
                   params = list("d", ids))
    "INGEN FEJL — undersøg hvad der faktisk skete"
  }, error = function(e) paste("FEJL (forventet):", conditionMessage(e)))
  cat(res_d, "\n")
  stopifnot("V1d: forventede en fejl pga. ulige parameterlængder, men fik ingen" =
              startsWith(res_d, "FEJL (forventet)"))

  cat("\n=== V2: FOR UPDATE mod en konkurrerende forbindelse ===\n")
  c2 <- DBI::dbConnect(RPostgres::Postgres(),
    host = "aws-0-eu-west-1.pooler.supabase.com", port = 5432,
    dbname = "postgres", user = "postgres.ijgwlqpbjcfffdmxeahh",
    password = Sys.getenv("SUPABASE_DB_PASSWORD"), sslmode = "require")
  on.exit(DBI::dbDisconnect(c2), add = TRUE)

  DBI::dbBegin(conn)
  DBI::dbGetQuery(conn, sprintf("SELECT id, v FROM %s WHERE id = 1 FOR UPDATE", TBL_Q))
  DBI::dbExecute(c2, "SET lock_timeout = '2s'")
  res_v2 <- tryCatch({
    DBI::dbExecute(c2, sprintf("UPDATE %s SET v = 'anden' WHERE id = 1", TBL_Q))
    "IKKE BLOKERET — FOR UPDATE virkede ikke som forventet"
  }, error = function(e) {
    msg <- conditionMessage(e)
    # Kræv den KONKRETE lock-timeout-fejl, ikke en vilkårlig fejl — en
    # syntaksfejl, manglende rettigheder eller en forsvundet tabel skal IKKE
    # tælle som "blokeret som forventet".
    if (grepl("lock timeout", msg, fixed = TRUE)) {
      paste("BLOKERET som forventet (lock timeout):", msg)
    } else {
      stop("V2: UVENTET fejltype (ikke lock timeout) — kan ikke tolkes som bevis for FOR UPDATE-låsning: ", msg)
    }
  })
  cat(res_v2, "\n")
  stopifnot("V2: forventede at blive blokeret af FOR UPDATE-låsen med en lock-timeout-fejl" =
              startsWith(res_v2, "BLOKERET som forventet"))

  # Tosidet bevis: rul `conn` tilbage (frigiver FOR UPDATE-låsen), og gentag
  # SAMME UPDATE på c2 — den skal nu lykkes øjeblikkeligt, ikke bare fejle
  # korrekt før.
  DBI::dbRollback(conn)
  n_retry <- DBI::dbExecute(c2, sprintf("UPDATE %s SET v = 'anden' WHERE id = 1", TBL_Q))
  cat("Efter dbRollback(conn): samme UPDATE på c2 lykkedes, rows affected =", n_retry, "\n")
  stopifnot("V2: UPDATE skulle lykkes øjeblikkeligt efter lås-frigivelse (tosidet bevis)" = n_retry == 1)

  cat("\n=== KONKLUSION: noter hvilket mønster Task 16 skal bruge ===\n")
}

main()
