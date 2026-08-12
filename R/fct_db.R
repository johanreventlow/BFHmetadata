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
      call. = FALSE
    )
  }
}

#' Opret pool mod Supabase (postgres-rolle, bypasser RLS — admin-tooling)
#' @noRd
db_connect <- function() {
  cfg <- db_config()
  pw <- Sys.getenv("SUPABASE_DB_PASSWORD")
  if (!nzchar(pw)) stop("SUPABASE_DB_PASSWORD mangler i .Renviron", call. = FALSE)
  pool::dbPool(RPostgres::Postgres(),
    host = cfg$host, port = cfg$port,
    dbname = cfg$dbname, user = cfg$user, password = pw, sslmode = cfg$sslmode,
    # Supabase-pooleren lukker inaktive forbindelser server-side. Uden
    # validationInterval = 0 udleveres en checket-ud forbindelse ofte uden
    # validering (default 600s), hvilket gav "server closed the connection
    # unexpectedly" midt i en query efter en pause i app-brug.
    validationInterval = 0, idleTimeout = 30, minSize = 1
  )
}

#' Byg db-accessor-liste bundet til pool (dependency injection til modul/test)
#' @noRd
make_db <- function(pool) {
  list(
    list_indikatorer = function() DBI::dbGetQuery(pool, build_list_sql()),
    fk_options = function() {
      stats::setNames(
        lapply(.fk_fields(), function(f) {
          DBI::dbGetQuery(pool, build_fk_options_sql(f$parent, f$label))
        }),
        vapply(.fk_fields(), function(f) f$col, "")
      )
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
        params = list(indikator_id)
      )
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
          params = list(indikator_id)
        )
        if (length(parent_ids)) {
          DBI::dbExecute(conn, build_junction_insert_sql(j, length(parent_ids)),
            params = c(list(indikator_id), as.list(parent_ids))
          )
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
          params = c(unname(values), list(id))
        )
        for (key in names(picks)) {
          j <- INDIKATOR_JUNCTIONS[[key]]
          ids <- picks[[key]][!is.na(picks[[key]])]
          DBI::dbExecute(conn, build_junction_delete_sql(j), params = list(id))
          if (length(ids)) {
            DBI::dbExecute(conn, build_junction_insert_sql(j, length(ids)),
              params = c(list(id), as.list(ids))
            )
          }
        }
      })
    },
    # Samlet opret: INSERT (RETURNING id) + junction-inserts i ÉN transaktion.
    # Returnerer ny id. Bruges af modal-flow ved oprettelse af blank indikator.
    create_indikator_full = function(values, picks) {
      assert_write_enabled()
      pool::poolWithTransaction(pool, function(conn) {
        cols <- names(values)
        newid <- DBI::dbGetQuery(conn, build_insert_sql(cols),
          params = unname(values)
        )$id[1]
        for (key in names(picks)) {
          j <- INDIKATOR_JUNCTIONS[[key]]
          ids <- picks[[key]][!is.na(picks[[key]])]
          if (length(ids)) {
            DBI::dbExecute(conn, build_junction_insert_sql(j, length(ids)),
              params = c(list(newid), as.list(ids))
            )
          }
        }
        newid
      })
    },
    # --- Signal-gennemgang: diagram-indeks + median-knæk ------------------
    list_active_seriediagrammer = function() {
      DBI::dbGetQuery(pool, build_diagram_index_sql())
    },
    diagram_medians = function(diagram_id) {
      DBI::dbGetQuery(pool, build_median_list_sql(), params = list(diagram_id))
    },
    # Alle median-knæk for en vektor af diagram-id'er i ÉT kald. Bruges af
    # signal-scannet, hvor per-diagram-opslag ellers giver ~600 round-trips.
    # Ids sendes som array-literal ("{1,2,3}") — se build_median_batch_sql
    # for hvorfor en rå vektor ikke kan bindes som array.
    diagram_medians_batch = function(diagram_ids) {
      ids <- unique(as.integer(diagram_ids))
      ids <- ids[!is.na(ids)]
      if (length(ids) == 0) {
        return(data.frame(
          id = integer(0), diagram = integer(0),
          laas_median = as.Date(character(0))
        ))
      }
      DBI::dbGetQuery(pool, build_median_batch_sql(),
        params = list(pg_int_array(ids))
      )
    },
    # aggregering = diagrammets periode_aggregering PÅ SÆTTE-TIDSPUNKTET
    # (dansk værdi, fx "uge"/"maaned"). Gør knækket selvbeskrivende, så en
    # senere periode-ændring kan opdages i stedet for at flytte faseskiftet
    # tavst.
    add_median_break = function(diagram_id, dato, aggregering = NA_character_) {
      assert_write_enabled()
      agg <- if (is.null(aggregering) || length(aggregering) == 0L ||
        is.na(aggregering) || !nzchar(trimws(aggregering))) {
        NA_character_
      } else {
        trimws(as.character(aggregering))
      }
      DBI::dbGetQuery(pool, build_median_insert_sql(),
        params = list(
          diagram_id, as.character(as.Date(dato)),
          agg
        )
      )[[1]][1]
    },
    delete_median_break = function(median_id) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_median_delete_sql(), params = list(median_id))
    },
    org_enhed_variants = function() {
      DBI::dbGetQuery(pool, build_org_enhed_variants_sql())
    },
    # Hele org-træet + aggregerings-flag til hierarki-oprulning. Hentes én
    # gang pr. scan (se fct_aggregate.R).
    org_struct = function() {
      DBI::dbGetQuery(pool, build_org_struct_sql())
    },
    aggregation_flags = function() {
      DBI::dbGetQuery(pool, build_aggregation_flags_sql())
    },
    # --- Diagram-CRUD (admin) --------------------------------------------
    list_diagrams_admin = function() {
      DBI::dbGetQuery(pool, build_diagram_admin_sql())
    },
    diagram_periode_choices = function() {
      DBI::dbGetQuery(pool, build_diagram_periode_sql())[[1]]
    },
    # id+label-choices til diagram-formularens tre FK-dropdowns.
    # tblIndikatorer har pk "id" (småt) → egen SQL frem for build_fk_options_sql.
    diagram_form_options = function() {
      list(
        indikator = DBI::dbGetQuery(pool, paste0(
          'SELECT "id" AS id, "indikator_navn" AS label ',
          'FROM "tblIndikatorer" ORDER BY 2'
        )),
        org = DBI::dbGetQuery(pool, build_fk_options_sql(
          "tblOrganisationStruktur",
          'COALESCE("organisatorisk_navn_langt","organisatorisk_navn_teknisk")'
        )),
        type = DBI::dbGetQuery(pool, build_fk_options_sql(
          "tblDiagramTyper", '"diagram_type"'
        ))
      )
    },
    diagram_duplicate_count = function(indikator, org, type, exclude_id = -1L) {
      as.integer(DBI::dbGetQuery(pool, build_diagram_duplicate_sql(),
        params = list(indikator, org, type, exclude_id)
      )$n[1])
    },
    diagram_median_count = function(diagram_id) {
      as.integer(DBI::dbGetQuery(pool, build_median_count_sql(),
        params = list(diagram_id)
      )$n[1])
    },
    # values: named list med alle DIAGRAM_COLS (rækkefølge håndhæves her)
    create_diagram = function(values) {
      assert_write_enabled()
      DBI::dbGetQuery(pool, build_diagram_insert_sql(),
        params = unname(values[DIAGRAM_COLS])
      )$id[1]
    },
    update_diagram = function(id, values) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_diagram_update_sql(),
        params = c(unname(values[DIAGRAM_COLS]), list(id))
      )
    },
    delete_diagram = function(id) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_diagram_delete_sql(), params = list(id))
    }
  )
}

#' Byg db-accessors for én simpel opslagstabel (inline-redigering).
#' cfg = element fra LOOKUP_TABLES. Genbruger pool + write-guard.
#' @noRd
make_lookup_db <- function(pool, cfg) {
  list(
    list_rows = function() DBI::dbGetQuery(pool, build_lookup_list_sql(cfg$table, cfg$pk)),
    add_row = function() {
      assert_write_enabled()
      DBI::dbGetQuery(pool, build_lookup_insert_sql(cfg$table, cfg$pk))[[1]][1]
    },
    update_cell = function(pk_val, col, value) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_lookup_update_sql(cfg$table, cfg$pk, col),
        params = list(value, pk_val)
      )
    },
    delete_row = function(pk_val) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_lookup_delete_sql(cfg$table, cfg$pk),
        params = list(pk_val)
      )
    },
    # 0 hvis ingen ref_check defineret; ellers antal child-rækker der peger hertil
    ref_count = function(pk_val) {
      rc <- cfg$ref_check
      if (is.null(rc)) {
        return(0L)
      }
      as.integer(DBI::dbGetQuery(pool, build_lookup_refcount_sql(rc$child, rc$col),
        params = list(pk_val)
      )[[1]][1])
    },
    # id+label for en FK-kolonnes dropdown (NULL hvis kolonnen ej er fk)
    fk_options = function(col) {
      fc <- Find(function(c) identical(c$type, "fk") && c$col == col, cfg$cols)
      if (is.null(fc)) {
        return(NULL)
      }
      DBI::dbGetQuery(pool, build_fk_options_sql(fc$parent, fc$label_expr))
    }
  )
}
