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
  d <- series_df(dates, vals, "ACC") |> mutate(unit = "Euro")
  none <- tibble::tibble(dataset = character(), month = character(), series_pattern = character(), note = character())
  a <- latest_anomalies("t", d, test_config(), none)
  expect_equal(a$status, "warn")
  note <- tibble::tibble(dataset = "t", month = "2026-08", series_pattern = "ACC", note = "Festival")
  b <- latest_anomalies("t", d, test_config(), note)
  expect_equal(b$status, "pass")
  expect_equal(b$details[[1]]$explanation, "Festival")
})

test_that("counties that sum to the total are learned even when some months have a blank county", {
  cfg <- test_config()
  months <- monthly("2024-01-01", "2026-08-01")
  parts <- expand.grid(date = months, COUNTY = c("Cork", "Dublin", "Leitrim"), stringsAsFactors = FALSE)
  parts$value <- c(Cork = 300, Dublin = 900, Leitrim = 20)[parts$COUNTY]
  total <- parts |> group_by(date) |> summarise(value = sum(value), .groups = "drop") |> mutate(COUNTY = "All")
  d <- bind_rows(parts, total) |> mutate(unit = "Euro")
  blank <- d$COUNTY == "Leitrim" & d$date < as.Date("2024-09-01")   # suppressed for 8 of 32 months
  d$value[blank] <- NA
  always <- d |> mutate(unit = "Pure number", value = ifelse(COUNTY == "Leitrim", NA, value))  # blank every month
  d <- bind_rows(d, always)
  codes <- tibble::tibble(dim = "COUNTY", code = c("_T", "CO", "DU", "LM"),
                          label = c("All", "Cork", "Dublin", "Leitrim"))
  ok <- reconcile_dataset("t", d, "COUNTY", codes, 2, cfg$labels, cfg)
  s <- ok[ok$check == "sum_COUNTY", ]
  expect_equal(s$status, "pass")
  expect_equal(s$n_tested, sum(months >= as.Date("2024-09-01")))   # blank months skipped
  d$value[d$COUNTY == "All" & d$unit == "Euro" & d$date == as.Date("2026-03-01")] <- 1300
  bad <- reconcile_dataset("t", d, "COUNTY", codes, 2, cfg$labels, cfg)
  b <- bad[bad$check == "sum_COUNTY", ]
  expect_equal(b$status, "fail")
  expect_equal(b$details[[1]]$date, as.Date("2026-03-01"))
  expect_false("complete" %in% names(b$details[[1]]))
})

test_that("the Diagnostics fit flags the same latest month as the anomaly check", {
  dates <- monthly("2022-03-01", "2026-08-01")
  set.seed(1)
  vals <- 100 * (1 + 0.1 * sin(seq_along(dates) * 2 * pi / 12)) + stats::rnorm(length(dates), 0, 1)
  vals[length(vals)] <- vals[length(vals)] * 1.6
  fit <- stl_fences(dates, vals, 3)
  expect_true(all(c("observed", "season", "trend", "remainder", "recomposed_l1", "recomposed_l2", "anomaly") %in% names(fit)))
  expect_equal(fit$anomaly[nrow(fit)], "Yes")
  flat <- stl_fences(dates, rep(5, length(dates)), 3)
  expect_true(all(flat$anomaly == "No"))
})

test_that("a gap fails in a material series and is for review in a tiny one", {
  dates <- monthly("2025-01-01", "2026-08-01")
  big <- series_df(dates, rep(1e6, length(dates)), "BIG")
  tiny <- series_df(dates[dates != as.Date("2026-02-01")], rep(5, length(dates) - 1), "TINY")
  run <- function(d) {
    raw <- tibble::tibble(REPORTINGPERIOD = format(d$date, "%Y-%m-%d"), SERIES_KEY = d$series_key,
                          OBSERVATION = as.character(d$value), FREQ = "M", FREQ_DESC = "Monthly")
    prof <- profile_dataset(raw)
    tidy <- tidy_dataset(raw, prof)
    schema <- list(status = "pass", observed = names(raw), missing = character(), extra = character(), message = "")
    v <- validate_dataset("t", raw, prof, tidy, compute_variances(tidy$data, "flow"),
                          compare_releases(tidy, NULL), schema, test_config())
    v[v$check == "gaps", ]
  }
  g <- run(bind_rows(big, tiny))
  expect_equal(as.character(g$status), "warn")
  expect_false(g$details[[1]]$material)
  bigger_gap <- bind_rows(big[big$date != as.Date("2026-02-01"), ], tiny)
  expect_equal(as.character(run(bigger_gap)$status), "fail")
})

test_that("a dataset can override a threshold", {
  cfg <- test_config()
  cfg$datasets <- list(mpmc = list(thresholds = list(materiality_share = 0.01)))
  expect_equal(dataset_thresholds(cfg, "mpmc")$materiality_share, 0.01)
  expect_equal(dataset_thresholds(cfg, "mpmc")$mom_pct, 0.2)
  expect_equal(dataset_thresholds(cfg, "mpcp")$materiality_share, 0.001)
})

test_that("the anomaly check skips immaterial series", {
  dates <- monthly("2022-03-01", "2026-08-01")
  set.seed(1)
  wave <- 1 + 0.1 * sin(seq_along(dates) * 2 * pi / 12) + stats::rnorm(length(dates), 0, 0.01)
  spike <- wave
  spike[length(spike)] <- spike[length(spike)] * 1.6
  d <- bind_rows(series_df(dates, 1e6 * wave, "BIG"), series_df(dates, 10 * spike, "TINY")) |> mutate(unit = "Euro")
  none <- tibble::tibble(dataset = character(), month = character(), series_pattern = character(), note = character())
  a <- latest_anomalies("t", d, test_config(), none)
  expect_equal(a$n_tested, 1L)
  expect_equal(a$status, "pass")
})
