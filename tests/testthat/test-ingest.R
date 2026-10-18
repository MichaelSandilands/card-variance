raw_example <- function() {
  tibble::tibble(
    REPORTINGPERIOD = c("2026-07-31T00:00:00", "2026-08-31T00:00:00", "2026-07-31T00:00:00", "2026-08-31T00:00:00"),
    SERIES_KEY = c("K.EUR", "K.EUR", "K.PN", "K.PN"),
    OBSERVATION = c("4430.12", "4500.00", "180.5", "182.25"),
    CONFIDENTIALITY_FLAG = "F",
    UNITS = c("Euro, millions", "Euro, millions", "Number, millions", "Number, millions"),
    FREQ = "M", FREQ_DESC = "Monthly",
    REF_AREA = "IE", REF_AREA_DESC = "Ireland",
    RMT_INTTN = c("NR", "R", "NR", "R"), RMT_INTTN_DESC = c("Non-remote", "Remote", "Non-remote", "Remote"),
    UNIT_MEASURE = c("EUR", "EUR", "PN", "PN"), UNIT_MEASURE_DESC = c("Euro", "Euro", "Pure number", "Pure number")
  )
}

test_that("profile keeps labels, hides codes, constants and one-to-one repeats", {
  p <- profile_dataset(raw_example(), labels = list(RMT_INTTN = "Remote or non-remote"))
  expect_equal(p$date_col, "REPORTINGPERIOD")
  expect_equal(p$value_col, "OBSERVATION")
  expect_equal(p$dims, "RMT_INTTN")
  expect_equal(p$unit_dim, "UNIT_MEASURE")
  r <- p$roles
  expect_equal(r$role[r$column == "UNITS"], "redundant")
  expect_equal(r$role[r$column == "REF_AREA_DESC"], "constant")
  expect_equal(r$role[r$column == "RMT_INTTN"], "code")
  expect_false(r$kept[r$column == "FREQ_DESC"])
  expect_equal(p$constants[["FREQ"]], "Monthly")
})

test_that("tidy parses periods to months, values to numbers, and records decimals", {
  raw <- raw_example()
  t <- tidy_dataset(raw, profile_dataset(raw))
  expect_equal(t$data$date[1], as.Date("2026-07-01"))
  expect_equal(t$data$value[1], 4430.12)
  expect_equal(t$decimals, 2L)
  expect_true(all(c("RMT_INTTN", "unit") %in% names(t$data)))
  expect_equal(sort(unique(t$data$unit)), c("Euro", "Pure number"))
})

test_that("period parsing copes with the formats the Bank has used", {
  expect_equal(parse_period(c("2026-08-31T00:00:00", "2026-08-31", "31/08/2026", "2026-08")),
               rep(as.Date("2026-08-01"), 4))
})

test_that("schema check stops on a missing column and passes on a match", {
  dir <- withr::local_tempdir()
  readr::write_csv(tibble::tibble(column = names(raw_example())), file.path(dir, "x.csv"))
  expect_equal(check_schema(raw_example(), "x", dir)$status, "pass")
  bad <- raw_example()[, -3]
  s <- check_schema(bad, "x", dir)
  expect_equal(s$status, "stop")
  expect_equal(s$missing, "OBSERVATION")
  expect_equal(check_schema(raw_example(), "new", dir)$status, "warn")
  expect_true(file.exists(file.path(dir, "new.observed.csv")))
})

test_that("county names normalise to the 26 counties", {
  x <- c("Co. Dublin", "D\u00fan Laoghaire\u2013Rathdown", "County Cork", "Laoighis",
         "Tipperary North", "Galway City", "Fingal")
  expect_equal(normalise_county(x),
               c("DUBLIN", "DUBLIN", "CORK", "LAOIS", "TIPPERARY", "GALWAY", "DUBLIN"))
})
