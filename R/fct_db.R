#' Læs supabase-DB-config fra rod-config.yml
#' @noRd
db_config <- function() {
  path <- if (file.exists("config.yml")) "config.yml" else app_sys("../config.yml")
  yaml::read_yaml(path)$default$supabase
}

#' Er skrivning aktiveret? (write-guard — bevidst friktion mod forkert target)
#' @noRd
write_enabled <- function() {
  isTRUE(getOption("bfhmeta.write_enabled")) ||
    identical(Sys.getenv("BFHMETA_WRITE"), "1")
}

#' Stop hvis skrivning ej aktiveret
#' @noRd
assert_write_enabled <- function() {
  if (!write_enabled()) {
    stop("DB-skrivning er deaktiveret. Sæt BFHMETA_WRITE=1 eller ",
         "options(bfhmeta.write_enabled=TRUE) efter at have bekræftet target.",
         call. = FALSE)
  }
}

#' Opret pool mod Supabase (postgres-rolle, bypasser RLS — admin-tooling)
#' @noRd
db_connect <- function() {
  cfg <- db_config()
  pw <- Sys.getenv("SUPABASE_DB_PASSWORD")
  if (!nzchar(pw)) stop("SUPABASE_DB_PASSWORD mangler i .Renviron", call. = FALSE)
  pool::dbPool(RPostgres::Postgres(), host = cfg$host, port = cfg$port,
    dbname = cfg$dbname, user = cfg$user, password = pw, sslmode = cfg$sslmode)
}

#' Byg db-accessor-liste bundet til pool (dependency injection til modul/test)
#' @noRd
make_db <- function(pool) {
  list(
    list_indikatorer = function() DBI::dbGetQuery(pool, build_list_sql()),
    fk_options = function() {
      stats::setNames(lapply(.fk_fields(), function(f)
        DBI::dbGetQuery(pool, build_fk_options_sql(f$parent, f$label))),
        vapply(.fk_fields(), function(f) f$col, ""))
    },
    create_indikator = function(values) {
      assert_write_enabled()
      cols <- names(values)
      DBI::dbGetQuery(pool, build_insert_sql(cols), params = unname(values))$id[1]
    },
    update_indikator = function(id, values) {
      assert_write_enabled()
      cols <- names(values)
      DBI::dbExecute(pool, build_update_sql(cols), params = c(unname(values), list(id)))
    },
    soft_delete = function(id, active = FALSE) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_soft_delete_sql(), params = list(active, id))
    },
    get_junction = function(indikator_id, key) {
      j <- INDIKATOR_JUNCTIONS[[key]]
      res <- DBI::dbGetQuery(pool, build_junction_select_sql(j),
                             params = list(indikator_id))
      res[[j$fk]]
    },
    junction_options = function(key) {
      j <- INDIKATOR_JUNCTIONS[[key]]
      DBI::dbGetQuery(pool, build_junction_options_sql(j))
    },
    set_junction = function(indikator_id, key, parent_ids) {
      assert_write_enabled()
      j <- INDIKATOR_JUNCTIONS[[key]]
      parent_ids <- parent_ids[!is.na(parent_ids)]
      pool::poolWithTransaction(pool, function(conn) {
        DBI::dbExecute(conn, build_junction_delete_sql(j),
                       params = list(indikator_id))
        if (length(parent_ids)) {
          DBI::dbExecute(conn, build_junction_insert_sql(j, length(parent_ids)),
                         params = c(list(indikator_id), as.list(parent_ids)))
        }
      })
    },
    # Samlet gem: scalar-UPDATE + replace af alle junctions i ÉN transaktion.
    # Sikrer at modal-gem aldrig efterlader delvist skrevet tilstand (fx ved
    # netværksfejl midt i gem). picks = named list (junction-key → parent-ids).
    save_indikator = function(id, values, picks) {
      assert_write_enabled()
      pool::poolWithTransaction(pool, function(conn) {
        cols <- names(values)
        DBI::dbExecute(conn, build_update_sql(cols),
                       params = c(unname(values), list(id)))
        for (key in names(picks)) {
          j <- INDIKATOR_JUNCTIONS[[key]]
          ids <- picks[[key]][!is.na(picks[[key]])]
          DBI::dbExecute(conn, build_junction_delete_sql(j), params = list(id))
          if (length(ids)) {
            DBI::dbExecute(conn, build_junction_insert_sql(j, length(ids)),
                           params = c(list(id), as.list(ids)))
          }
        }
      })
    }
  )
}
