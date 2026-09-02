#' Find Supabase-DB-konfiguration uden at pakke hemmeligheder.
#' @noRd
db_config_path <- function(path = NULL) {
  if (!is.null(path)) return(path)
  explicit <- Sys.getenv("BFHMETA_DB_CONFIG")
  if (nzchar(explicit)) return(explicit)
  app_sys("db-config.yml")
}

#' Læs og validér Supabase-DB-konfiguration.
#' @noRd
db_config <- function(path = NULL) {
  path <- db_config_path(path)
  if (length(path) != 1L || is.na(path) || !nzchar(path) || !file.exists(path)) {
    stop("DB-konfigurationen mangler i installationen", call. = FALSE)
  }
  cfg <- yaml::read_yaml(path)$default$supabase
  required <- c("host", "port", "dbname", "user", "sslmode")
  if (!is.list(cfg) || !all(required %in% names(cfg)) ||
      any(vapply(cfg[required], function(x) length(x) != 1L || is.na(x),
                 logical(1)))) {
    stop("DB-konfigurationen er ufuldst\u00E6ndig", call. = FALSE)
  }
  cfg[required]
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
    stop("DB-skrivning er deaktiveret. S\u00E6t BFHMETA_WRITE=1 eller ",
      "options(bfhmeta.write_enabled=TRUE) efter at have bekr\u00E6ftet target.",
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

# --- Bulk-redigering: batch-kontrakt (Leverance 2 af -------------------------
# docs/plans/2026-08-30-bulk-redigering-design.md) ---------------------------
# Standard-navne på audit-schemaets tabeller (Leverance 3-migrationen, samme
# dok §4). make_db() bruger disse defaults; integrationstests kan overrides
# til engangstabeller efter dev/bulk_probe.R-mønstret, indtil migrationen er
# kørt (se tests/testthat/test-db-bulk.R).
.AUDIT_BATCH_TABLE <- '"audit"."tbl_batch"'
.AUDIT_ROW_TABLE   <- '"audit"."tbl_batch_raekke"'

#' Struktureret konflikt-condition til bulk-flowet. type: "duplicate" |
#' "missing" | "stale" | "undo_conflict". ids: de berørte id'er (character).
#' Fanges med tryCatch(..., bulk_conflict = function(e) ...) — e$type/e$ids
#' giver kalderen data til en konfliktrapport uden at parse fejlbeskeden.
#' @noRd
bulk_conflict <- function(type, ids, message = NULL) {
  msg <- message %||% switch(type,
    duplicate = sprintf("Id'et/id'erne er angivet flere gange: %s",
                        paste(ids, collapse = ", ")),
    missing = sprintf("Følgende rækker findes ikke længere: %s",
                      paste(ids, collapse = ", ")),
    stale = sprintf(paste("Følgende rækker er ændret af en anden",
                          "bruger siden forhåndsvisningen: %s"),
                    paste(ids, collapse = ", ")),
    undo_conflict = sprintf(paste("Følgende rækker er ændret siden",
                                  "batchen og kan ikke fortrydes: %s"),
                            paste(ids, collapse = ", ")),
    sprintf("Konflikt: %s", paste(ids, collapse = ", "))
  )
  structure(
    class = c("bulk_conflict", "error", "condition"),
    list(message = msg, call = NULL, type = type, ids = ids)
  )
}

#' Sæt ÉT allowlistet felt på et eksplicit id-sæt, atomisk, med audit +
#' konfliktkontrol (design-dok §5). expected_before: named liste/vektor,
#' navne = as.character(ids), værdier = de førværdier UI'et sidst viste
#' (forhåndsvisningen) — bruges til at opdage at grid'et er blevet stale
#' siden (krav 7). Skal indeholde præcis ét element pr. id.
#' Returnerer list(batch_id, n, skipped) ved succes (rækker hvor værdien
#' allerede var målværdien er "skipped", ikke skrevet/auditeret). Kaster en
#' "bulk_conflict"-condition ved dubleret/manglende/stale id — HELE batchen
#' rulles tilbage (poolWithTransaction ruller tilbage på enhver fejl).
#' @noRd
.bulk_update_impl <- function(pool, tabel_key, ids, felt, vaerdi, expected_before,
                              audit_batch_tbl = .AUDIT_BATCH_TABLE,
                              audit_row_tbl = .AUDIT_ROW_TABLE,
                              tables_cfg = BULK_TABLES) {
  assert_write_enabled()

  tbl <- tables_cfg[[tabel_key]]
  if (is.null(tbl)) stop(sprintf("Ukendt bulk-tabel: '%s'", tabel_key), call. = FALSE)
  fld <- bulk_field_config(tabel_key, felt, tables = tables_cfg)
  if (is.null(fld)) {
    stop(sprintf("Feltet '%s' er ikke tilladt i bulk-redigering for '%s'",
                 felt, tabel_key), call. = FALSE)
  }

  if (length(ids) == 0L || anyNA(ids)) {
    stop("Bulk kræver mindst ét gyldigt id", call. = FALSE)
  }
  ids_chr <- as.character(ids)
  dup <- unique(ids_chr[duplicated(ids_chr)])
  if (length(dup) > 0L) stop(bulk_conflict("duplicate", dup))
  if (!setequal(names(expected_before), ids_chr)) {
    stop("expected_before skal indeholde præcis én førværdi pr. id",
         call. = FALSE)
  }

  # Konvertér ALT (målværdi + forventede førværdier) FØR nogen forbindelse
  # åbnes — en typefejl skal aldrig kunne åbne en transaktion.
  target <- bulk_coerce_value(fld, vaerdi)
  expected_typed <- stats::setNames(
    lapply(ids_chr, function(k) bulk_coerce_value(fld, expected_before[[k]], allow_blank = TRUE)),
    ids_chr
  )
  ids_int <- as.integer(ids)

  pool::poolWithTransaction(pool, function(conn) {
    locked <- DBI::dbGetQuery(conn, build_bulk_lock_sql(tbl$table, tbl$pk, felt),
                              params = list(pg_int_array(ids_int)))
    locked_ids <- as.character(locked[[tbl$pk]])
    missing <- setdiff(ids_chr, locked_ids)
    if (length(missing) > 0L) stop(bulk_conflict("missing", missing))

    current <- stats::setNames(locked[[felt]], locked_ids)
    stale <- ids_chr[!vapply(ids_chr, function(k) {
      identical(current[[k]], expected_typed[[k]])
    }, logical(1))]
    if (length(stale) > 0L) stop(bulk_conflict("stale", stale))

    unchanged <- ids_chr[vapply(ids_chr, function(k) identical(current[[k]], target), logical(1))]
    write_ids <- setdiff(ids_chr, unchanged)
    if (length(write_ids) == 0L) {
      return(list(batch_id = NA_character_, n = 0L, skipped = as.integer(unchanged)))
    }
    write_ids_int <- as.integer(write_ids)

    DBI::dbExecute(conn, build_bulk_update_sql(tbl$table, tbl$pk, felt),
                   params = list(target, pg_int_array(write_ids_int)))

    batch_id <- DBI::dbGetQuery(conn, build_audit_batch_insert_sql(audit_batch_tbl),
                                params = list(tbl$table, felt))$batch_id[1]
    row_params <- c(list(batch_id), unlist(lapply(write_ids_int, function(id) {
      list(id, bulk_value_to_text(fld$kind, current[[as.character(id)]]),
           bulk_value_to_text(fld$kind, target))
    }), recursive = FALSE))
    DBI::dbExecute(conn, build_audit_row_insert_sql(length(write_ids_int), audit_row_tbl),
                   params = row_params)

    list(batch_id = batch_id, n = length(write_ids_int), skipped = as.integer(unchanged))
  })
}

#' Fortryd én batch: ny transaktion, samme id-låserækkefølge, tjekker at
#' NUVÆRENDE værdi stadig er vaerdi_efter for HVER ramt række — én afvigelse
#' afviser fortrydelsen af HELE batchen (ingen delvis fortryd, krav 7).
#' Returnerer list(batch_id, n) ved succes. Kaster almindelig error ved
#' ukendt/allerede-fortrudt batch_id, "bulk_conflict" (type="undo_conflict")
#' ved værdikonflikt.
#' @noRd
.bulk_undo_impl <- function(pool, batch_id, audit_batch_tbl = .AUDIT_BATCH_TABLE,
                            audit_row_tbl = .AUDIT_ROW_TABLE,
                            tables_cfg = BULK_TABLES) {
  assert_write_enabled()

  pool::poolWithTransaction(pool, function(conn) {
    header <- DBI::dbGetQuery(conn, build_audit_batch_select_sql(audit_batch_tbl),
                              params = list(batch_id))
    if (nrow(header) == 0L) stop(sprintf("Ukendt batch: %s", batch_id), call. = FALSE)
    if (!is.na(header$fortrudt_ts[1])) {
      stop(sprintf("Batch %s er allerede fortrudt", batch_id), call. = FALSE)
    }

    tabel_key <- Find(function(k) identical(tables_cfg[[k]]$table, header$tabel[1]),
                      names(tables_cfg))
    fld <- if (is.null(tabel_key)) NULL else bulk_field_config(tabel_key, header$felt[1], tables = tables_cfg)
    if (is.null(fld)) {
      stop(sprintf(paste("Batch %s peger på en tabel/felt der ikke",
                         "længere er tilladt i bulk"), batch_id), call. = FALSE)
    }
    tbl <- tables_cfg[[tabel_key]]

    rows <- DBI::dbGetQuery(conn, build_audit_rows_select_sql(audit_row_tbl),
                            params = list(batch_id))
    if (nrow(rows) == 0L) {
      stop(sprintf("Batch %s har ingen rækker at fortryde", batch_id), call. = FALSE)
    }
    ids_int <- as.integer(rows$row_id)
    ids_chr <- as.character(ids_int)

    locked <- DBI::dbGetQuery(conn, build_bulk_lock_sql(tbl$table, tbl$pk, header$felt[1]),
                              params = list(pg_int_array(ids_int)))
    locked_ids <- as.character(locked[[tbl$pk]])
    missing <- setdiff(ids_chr, locked_ids)
    if (length(missing) > 0L) stop(bulk_conflict("undo_conflict", missing))

    current <- stats::setNames(locked[[header$felt[1]]], locked_ids)
    conflicted <- ids_chr[!vapply(seq_along(ids_chr), function(i) {
      identical(current[[ids_chr[i]]], bulk_untext_value(fld$kind, rows$vaerdi_efter[i]))
    }, logical(1))]
    if (length(conflicted) > 0L) stop(bulk_conflict("undo_conflict", conflicted))

    for (i in seq_len(nrow(rows))) {
      restore <- bulk_untext_value(fld$kind, rows$vaerdi_foer[i])
      DBI::dbExecute(conn, build_bulk_update_sql(tbl$table, tbl$pk, header$felt[1]),
                     params = list(restore, pg_int_array(ids_int[i])))
    }
    DBI::dbExecute(conn, build_audit_mark_undone_sql(audit_batch_tbl), params = list(batch_id))

    list(batch_id = batch_id, n = nrow(rows))
  })
}

#' Byg db-accessor-liste bundet til pool (dependency injection til modul/test)
#' @noRd
make_db <- function(pool) {
  list(
    list_indikatorer = function() DBI::dbGetQuery(pool, build_list_sql()),
    fk_options = function() {
      stats::setNames(
        lapply(.fk_fields(), function(f) {
          sql <- if (is.null(f$aktiv_col)) {
            build_fk_options_sql(f$parent, f$label,
                                 parent_col = f$parent_col)
          } else {
            build_fk_options_aktiv_sql(f$parent, f$label, f$aktiv_col,
                                       parent_col = f$parent_col)
          }
          DBI::dbGetQuery(pool, sql)
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
    # indikator medtager niveau-udledt datasaet (per-raekke-filtrering af
    # diagram-grid'ets Indikator-dropdown).
    diagram_form_options = function() {
      list(
        indikator = DBI::dbGetQuery(pool, build_diagram_indikator_options_sql()),
        org = DBI::dbGetQuery(pool, build_fk_options_sql(
          "tblOrganisationStruktur",
          'COALESCE("organisatorisk_navn_langt","organisatorisk_navn_teknisk")'
        )),
        type = DBI::dbGetQuery(pool, build_fk_options_sql(
          "tblDiagramTyper", '"diagram_type"'
        )),
        maalgruppe = DBI::dbGetQuery(pool, build_fk_options_sql(
          "tblMaalgrupper", '"maalgruppe_navn"'
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
    },
    # --- Mål-styring (admin) ----------------------------------------------
    list_maal_admin = function() {
      DBI::dbGetQuery(pool, build_maal_admin_sql())
    },
    # values: named list med alle MAAL_COLS (rækkefølge håndhæves her)
    create_maal = function(values) {
      assert_write_enabled()
      DBI::dbGetQuery(pool, build_maal_insert_sql(),
        params = unname(values[MAAL_COLS])
      )$id[1]
    },
    update_maal = function(id, values) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_maal_update_sql(),
        params = c(unname(values[MAAL_COLS]), list(id))
      )
    },
    delete_maal = function(id) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_maal_delete_sql(), params = list(id))
    },
    # --- Bulk-redigering (batch-kontrakt, Leverance 2) ---------------------
    bulk_update = function(tabel_key, ids, felt, vaerdi, expected_before) {
      .bulk_update_impl(pool, tabel_key, ids, felt, vaerdi, expected_before)
    },
    bulk_undo = function(batch_id) {
      .bulk_undo_impl(pool, batch_id)
    }
  )
}

#' Byg db-accessors for én simpel opslagstabel (inline-redigering).
#' cfg = element fra LOOKUP_TABLES. Genbruger pool + write-guard.
#' @noRd
make_lookup_db <- function(pool, cfg) {
  list(
    list_rows = function() DBI::dbGetQuery(pool, build_lookup_list_sql(cfg$table, cfg$pk)),
    get_row = function(pk_val) {
      DBI::dbGetQuery(pool, build_lookup_get_sql(cfg$table, cfg$pk),
        params = list(pk_val)
      )
    },
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
    # id+label for en FK-kolonnes dropdown (NULL hvis kolonnen ej er fk).
    # parent_col i cfg → parent_id medtages (indrykket trae-dropdown).
    fk_options = function(col) {
      fc <- Find(function(c) identical(c$type, "fk") && c$col == col, cfg$cols)
      if (is.null(fc)) {
        return(NULL)
      }
      DBI::dbGetQuery(pool, build_fk_options_sql(fc$parent, fc$label_expr,
                                                 parent_col = fc$parent_col))
    }
  )
}

#' Byg db-accessors for én hierarki-tabel. cfg = element fra HIERARCHY_TABLES.
#' values = named list i hierarchy_edit_cols(cfg)-orden.
#' @noRd
make_hierarchy_db <- function(pool, cfg) {
  cols <- hierarchy_edit_cols(cfg)
  list(
    list_nodes = function() {
      DBI::dbGetQuery(pool, build_hierarchy_list_sql(cfg))
    },
    niveau_options = function() {
      DBI::dbGetQuery(pool, build_fk_options_sql(cfg$level$parent,
                                                 cfg$level$label_expr))
    },
    create_node = function(values) {
      assert_write_enabled()
      DBI::dbGetQuery(pool, build_hierarchy_insert_sql(cfg),
                      params = unname(values[cols]))[[1]][1]
    },
    update_node = function(id, values) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_hierarchy_update_sql(cfg),
                     params = c(unname(values[cols]), list(id)))
    },
    delete_node = function(id) {
      assert_write_enabled()
      DBI::dbExecute(pool, build_hierarchy_delete_sql(cfg), params = list(id))
    },
    child_count = function(id) {
      as.integer(DBI::dbGetQuery(pool, build_hierarchy_child_count_sql(cfg),
                                 params = list(id))$n[1])
    }
  )
}
