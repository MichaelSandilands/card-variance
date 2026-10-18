# A small card-payments-like table: World = Domestic + Rest of the world,
# and Contactless sits inside the Total channel (overlapping, not additive).
recon_data <- function(break_month = NULL) {
  months <- monthly("2024-01-01", "2026-08-01")
  base <- expand.grid(date = months, COUNT_AREA = c("Domestic", "Rest of the world"),
                      INTTN_CHNNL = c("Total", "Contactless"), stringsAsFactors = FALSE)
  base$value <- with(base, ifelse(COUNT_AREA == "Domestic", 4430, 678.57) *
                       ifelse(INTTN_CHNNL == "Total", 1, 0.68))
  world <- base |>
    group_by(date, INTTN_CHNNL) |>
    summarise(value = sum(value), .groups = "drop") |>
    mutate(COUNT_AREA = "World")
  d <- bind_rows(base, world) |> mutate(unit = "Euro")
  if (!is.null(break_month)) {
    hit <- d$date == as.Date(break_month) & d$COUNT_AREA == "World" & d$INTTN_CHNNL == "Total"
    d$value[hit] <- d$value[hit] + 5
  }
  d$series_key <- paste(d$COUNT_AREA, d$INTTN_CHNNL, d$unit)
  d
}
recon_codes <- tibble::tibble(
  dim = c(rep("COUNT_AREA", 3), rep("INTTN_CHNNL", 2)),
  code = c("W0", "W1", "W2", "_T", "CTLS"),
  label = c("World", "Rest of the world", "Domestic", "Total", "Contactless")
)

test_that("the August 2026 release figures reconcile", {
  # Total EUR 9.72bn = domestic 7.83bn + non-domestic 1.89bn; domestic = POS 4.43bn + online 3.40bn.
  expect_equal(4.43 + 3.40, 7.83)
  expect_equal(7.83 + 1.89, 9.72)
  expect_lte(abs(0.67857 + 1.21 - 1.89), 0.005)    # non-domestic, within rounding
})

test_that("a learned sum rule passes clean data and catches a break", {
  cfg <- test_config()
  dims <- c("COUNT_AREA", "INTTN_CHNNL")
  ok <- reconcile_dataset("t", recon_data(), dims, recon_codes, 2, cfg$labels, cfg)
  sum_check <- ok[ok$check == "sum_COUNT_AREA", ]
  expect_equal(nrow(sum_check), 1)
  expect_equal(sum_check$status, "pass")
  bad <- reconcile_dataset("t", recon_data("2025-06-01"), dims, recon_codes, 2, cfg$labels, cfg)
  b <- bad[bad$check == "sum_COUNT_AREA", ]
  expect_equal(b$status, "fail")
  expect_equal(b$n_failed, 1L)
  expect_equal(b$details[[1]]$date, as.Date("2025-06-01"))
})

test_that("overlapping parts get a containment check, not a sum rule", {
  cfg <- test_config()
  out <- reconcile_dataset("t", recon_data(), c("COUNT_AREA", "INTTN_CHNNL"), recon_codes, 2, cfg$labels, cfg)
  expect_false("sum_INTTN_CHNNL" %in% out$check)
  sub <- out[out$check == "subset_INTTN_CHNNL", ]
  expect_equal(sub$status, "pass")
  expect_equal(sub$severity, "warn")
})

test_that("gaps, duplicates and negatives are caught by validate_dataset", {
  raw <- tibble::tibble(
    REPORTINGPERIOD = c("2026-05-31", "2026-06-30", "2026-08-31", "2026-08-31"),
    SERIES_KEY = "A", OBSERVATION = c("10", "-1", "12", "12"),
    FREQ = "M", FREQ_DESC = "Monthly"
  )
  prof <- profile_dataset(raw)
  tidy <- tidy_dataset(raw, prof)
  var <- compute_variances(tidy$data, "flow")
  schema <- list(status = "pass", observed = names(raw), missing = character(), extra = character(), message = "")
  v <- validate_dataset("t", raw, prof, tidy, var, compare_releases(tidy, NULL), schema, test_config())
  get <- function(id) v[v$check == id, ]
  expect_equal(as.character(get("unique")$status), "stop")
  expect_equal(as.character(get("negative")$status), "fail")
  expect_equal(as.character(get("gaps")$status), "fail")
  expect_equal(get("gaps")$details[[1]]$missing_month, as.Date("2026-07-01"))
})

test_that("revisions classify revised, removed and new rows", {
  old <- list(version = "v1", data = tibble::tibble(
    series_key = c("A", "A", "B"), date = as.Date(c("2026-06-01", "2026-07-01", "2026-07-01")),
    value = c(10, 20, 5)))
  cur <- list(decimals = 2, data = tibble::tibble(
    series_key = c("A", "A", "A"), date = as.Date(c("2026-06-01", "2026-07-01", "2026-08-01")),
    value = c(10, 21, 30)))
  r <- compare_releases(cur, old)
  expect_true(r$available)
  expect_equal(r$summary$revised, 1L)
  expect_equal(r$summary$removed, 1L)
  expect_equal(r$summary$months_added, 1L)
  expect_equal(r$changes$difference[r$changes$change == "revised"], 1)
})

test_that("an unusual latest month is flagged and an annotation explains it", {
  dates <- monthly("2022-03-01", "2026-08-01")
  set.seed(1)
  vals <- 100 * (1 + 0.1 * sin(seq_along(dates) * 2 * pi / 12)) + stats::rnorm(length(dates), 0, 1)
  vals[length(vals)] <- vals[length(vals)] * 1.6
  d <- series_df(dates, vals, "ACC")
  none <- tibble::tibble(dataset = character(), month = character(), series_pattern = character(), note = character())
  a <- latest_anomalies("t", d, test_config(), none)
  expect_equal(a$status, "warn")
  note <- tibble::tibble(dataset = "t", month = "2026-08", series_pattern = "ACC", note = "Festival")
  b <- latest_anomalies("t", d, test_config(), note)
  expect_equal(b$status, "pass")
  expect_equal(b$details[[1]]$explanation, "Festival")
})
