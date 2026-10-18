test_that("a missing month gives no comparison instead of comparing with an older month", {
  d <- series_df(c("2026-05-01", "2026-06-01", "2026-08-01"), c(100, 110, 130))
  v <- calc_mom(d)
  aug <- v[v$date == as.Date("2026-08-01"), ]
  expect_equal(aug$status, "no comparison")
  expect_true(is.na(aug$comparison_value))
  jun <- v[v$date == as.Date("2026-06-01"), ]
  expect_equal(jun$abs_change, 10)
  expect_equal(jun$pct_change, 0.1)
})

test_that("QoQ uses complete quarters only: Q1 2022 and Q3 2026 never appear", {
  dates <- monthly("2022-03-01", "2026-08-01")
  d <- series_df(dates, seq_along(dates))
  q <- calc_qoq(d, measure_type = "flow")
  expect_false(any(q$date == as.Date("2022-01-01")))
  expect_false(any(q$date == as.Date("2026-07-01")))
  first_ok <- q[q$status == "ok", ][1, ]
  expect_equal(first_ok$date, as.Date("2022-07-01"))               # Q3 2022 vs Q2 2022
  expect_equal(first_ok$value, sum(5:7))                           # Jul-Sep 2022 = months 5..7
  expect_equal(first_ok$comparison_value, sum(2:4))                # Apr-Jun 2022
  latest <- q[q$date == max(q$date), ]
  expect_equal(latest$date, as.Date("2026-04-01"))                 # Q2 2026 is the latest
})

test_that("stock QoQ takes the quarter-end month, not the sum", {
  dates <- monthly("2025-01-01", "2025-06-01")
  d <- series_df(dates, c(10, 11, 12, 13, 14, 15))
  q <- calc_qoq(d, measure_type = "stock")
  q2 <- q[q$date == as.Date("2025-04-01"), ]
  expect_equal(q2$value, 15)
  expect_equal(q2$comparison_value, 12)
})

test_that("flow YTD has no valid 2023 comparison and matches a hand sum later", {
  dates <- monthly("2022-03-01", "2026-08-01")
  vals <- seq_along(dates) * 10
  d <- series_df(dates, vals)
  y <- calc_ytd(d, measure_type = "flow")
  y2023 <- y[lubridate::year(y$date) == 2023, ]
  expect_true(all(y2023$status != "ok"))
  aug26 <- y[y$date == as.Date("2026-08-01"), ]
  jan_aug_26 <- vals[dates >= as.Date("2026-01-01") & dates <= as.Date("2026-08-01")]
  jan_aug_25 <- vals[dates >= as.Date("2025-01-01") & dates <= as.Date("2025-08-01")]
  expect_equal(aug26$value, sum(jan_aug_26))
  expect_equal(aug26$comparison_value, sum(jan_aug_25))
  expect_equal(aug26$status, "ok")
})

test_that("flow YTD with a gap since January is incomplete", {
  dates <- monthly("2025-01-01", "2026-08-01")
  dates <- dates[dates != as.Date("2026-02-01")]
  y <- calc_ytd(series_df(dates, rep(1, length(dates))), measure_type = "flow")
  expect_equal(y$status[y$date == as.Date("2026-08-01")], "incomplete period")
  expect_equal(y$status[y$date == as.Date("2026-01-01")], "ok")
})

test_that("stock YTD averages January to August 2026 against January to August 2025", {
  dates <- monthly("2025-01-01", "2026-08-01")
  vals <- seq(100, by = 5, length.out = length(dates))
  y <- calc_ytd(series_df(dates, vals), measure_type = "stock")
  aug <- y[y$date == as.Date("2026-08-01"), ]
  expect_equal(aug$comparison_date, as.Date("2025-08-01"))
  expect_equal(aug$value, mean(vals[dates >= as.Date("2026-01-01")]))
  expect_equal(aug$comparison_value, mean(vals[dates <= as.Date("2025-08-01")]))
  expect_equal(aug$status, "ok")
})

test_that("stock YTD with an incomplete prior year has no comparison", {
  dates <- monthly("2025-11-01", "2026-08-01")
  y <- calc_ytd(series_df(dates, seq_along(dates)), measure_type = "stock")
  expect_equal(y$status[y$date == as.Date("2026-08-01")], "no comparison")
})

test_that("zero base gives no percentage and a negative base keeps the sign", {
  v <- calc_mom(series_df(c("2026-01-01", "2026-02-01"), c(0, 5)))
  expect_equal(v$status[2], "zero base")
  expect_true(is.na(v$pct_change[2]))
  n <- calc_mom(series_df(c("2026-01-01", "2026-02-01"), c(-10, -5)))
  expect_equal(n$pct_change[2], 0.5)
})

test_that("two series with the same dates never cross-join", {
  d <- bind_rows(series_df(c("2026-01-01", "2026-02-01"), c(1, 2), "A"),
                 series_df(c("2026-01-01", "2026-02-01"), c(100, 300), "B"))
  v <- calc_mom(d)
  expect_equal(nrow(v), 4)
  expect_equal(v$comparison_value[v$series_key == "B" & v$date == as.Date("2026-02-01")], 100)
})

test_that("compute_variances returns all four types with readable labels", {
  dates <- monthly("2024-01-01", "2026-08-01")
  v <- compute_variances(series_df(dates, seq_along(dates)), "flow")
  expect_setequal(unique(v$variance_type), c("MoM", "QoQ", "YoY", "YTD"))
  lab <- variance_labels(c("QoQ", "YTD", "MoM", "YoY"), as.Date(c("2026-04-01", "2026-08-01", "2026-08-01", "2026-08-01")), NA)
  expect_equal(lab$period, c("Q2 2026", "Jan\u2013Aug 2026", "Aug 2026", "Aug 2026"))
  expect_equal(lab$comparison, c("Q1 2026", "Jan\u2013Aug 2025", "Jul 2026", "Aug 2025"))
})
