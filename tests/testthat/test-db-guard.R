test_that("write_enabled er FALSE som default", {
  withr::with_envvar(c(BFHMETA_WRITE = ""), {
    withr::with_options(list(bfhmeta.write_enabled = NULL), {
      expect_false(write_enabled())
    })
  })
})

test_that("write_enabled TRUE via env eller option", {
  withr::with_envvar(c(BFHMETA_WRITE = "1"), expect_true(write_enabled()))
  withr::with_envvar(c(BFHMETA_WRITE = ""), {
    withr::with_options(list(bfhmeta.write_enabled = TRUE),
                        expect_true(write_enabled()))
  })
})

test_that("assert_write_enabled fejler når disabled", {
  withr::with_envvar(c(BFHMETA_WRITE = ""), {
    withr::with_options(list(bfhmeta.write_enabled = NULL), {
      expect_error(assert_write_enabled(), "skrivning")
    })
  })
})

test_that("pakket DB-konfiguration virker uden udviklingsrod", {
  cfg <- withr::with_dir(withr::local_tempdir(), {
    db_config(app_sys("db-config.yml"))
  })
  expect_named(cfg, c("host", "port", "dbname", "user", "sslmode"),
               ignore.order = TRUE)
  expect_false("password" %in% names(cfg))
  expect_true(nzchar(cfg$host))
  expect_true(nzchar(cfg$user))
})

test_that("bar db_config bruger pakket config uden for udviklingsrod", {
  cfg <- withr::with_envvar(c(BFHMETA_DB_CONFIG = ""), {
    withr::with_dir(withr::local_tempdir(), db_config())
  })

  expect_identical(cfg$user, "postgres.ijgwlqpbjcfffdmxeahh")
})

test_that("bar db_config ignorerer uvedkommende config.yml", {
  unrelated <- withr::local_tempdir()
  writeLines(c(
    "default:", "  supabase:", "    host: unrelated.invalid",
    "    port: 5432", "    dbname: unrelated", "    user: unrelated",
    "    sslmode: disable"
  ), file.path(unrelated, "config.yml"))

  cfg <- withr::with_envvar(c(BFHMETA_DB_CONFIG = ""), {
    withr::with_dir(unrelated, db_config())
  })

  expect_identical(cfg$user, "postgres.ijgwlqpbjcfffdmxeahh")
})

test_that("bar db_config ignorerer en spoofet BFHmetadata-udviklingsrod", {
  spoofed_root <- withr::local_tempdir()
  writeLines(c("Package: BFHmetadata", "Version: 0.0.0"),
             file.path(spoofed_root, "DESCRIPTION"))
  writeLines(c(
    "default:", "  supabase:", "    host: development.invalid",
    "    port: 5432", "    dbname: development", "    user: development",
    "    sslmode: disable"
  ), file.path(spoofed_root, "config.yml"))

  cfg <- withr::with_envvar(c(BFHMETA_DB_CONFIG = ""), {
    withr::with_dir(spoofed_root, db_config())
  })

  expect_identical(cfg$user, "postgres.ijgwlqpbjcfffdmxeahh")
})

test_that("BFHMETA_DB_CONFIG vælger eksplicit fil", {
  p <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(
    "default:", "  supabase:", "    host: localhost",
    "    port: 5432", "    dbname: postgres", "    user: tester",
    "    sslmode: require"
  ), p)
  withr::with_envvar(c(BFHMETA_DB_CONFIG = p), {
    expect_identical(db_config()$user, "tester")
  })
})

test_that("manglende eller ufuldstændig DB-konfiguration fejler lukket", {
  expect_error(db_config(file.path(tempdir(), "findes-ikke.yml")),
               "DB-konfigurationen mangler")
  p <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("default:", "  supabase:", "    host: localhost"), p)
  expect_error(db_config(p), "DB-konfigurationen er ufuldstændig")
})
