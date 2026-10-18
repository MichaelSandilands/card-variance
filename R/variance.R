# MoM, QoQ (complete quarters only), YoY, YTD -----------------------------------
#
# Every variance joins the current period to its comparison period on the date,
# so a missing month yields "no comparison" instead of silently comparing the
# wrong periods. Flows and stocks differ only in how quarters and YTD are built.

shift_join <- function(df, months_back, keys = "series_key") {
  base <- df |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      comparison_date = date,
      date = lubridate::add_with_rollback(date, mths(months_back)),
      comparison_value = value
    )
  dplyr::left_join(df, base, by = c(keys, "date"))
}

finish_variance <- function(df) {
  df |>
    dplyr::mutate(
      abs_change = value - comparison_value,
      pct_change = dplyr::if_else(comparison_value == 0, NA_real_,
                                  abs_change / abs(comparison_value)),
      status = dplyr::case_when(
        is.na(value)            ~ "incomplete period",
        is.na(comparison_value) ~ "no comparison",
        comparison_value == 0   ~ "zero base",
        TRUE                    ~ "ok"
      )
    )
}

calc_mom <- function(df, keys = "series_key") {
  shift_join(df, 1, keys) |> finish_variance()
}

calc_yoy <- function(df, keys = "series_key") {
  shift_join(df, 12, keys) |> finish_variance()
}

to_quarters <- function(df, keys = "series_key", measure_type = "flow") {
  df |>
    dplyr::mutate(qtr = lubridate::floor_date(date, "quarter")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys)), qtr) |>
    dplyr::summarise(
      n_months = dplyr::n_distinct(date[!is.na(value)]),
      value = if (measure_type == "flow") sum(value) else value[which.max(date)],
      .groups = "drop"
    ) |>
    dplyr::filter(n_months == 3) |>          # complete quarters only
    dplyr::rename(date = qtr) |>
    dplyr::select(-n_months)
}

calc_qoq <- function(df, keys = "series_key", measure_type = "flow") {
  to_quarters(df, keys, measure_type) |>
    shift_join(3, keys) |>
    finish_variance()
}

# January-to-month this year against the same months a year earlier: flows are
# summed over those months, stocks averaged.
calc_ytd <- function(df, keys = "series_key", measure_type = "flow") {
  to_date <- if (measure_type == "stock") dplyr::cummean else cumsum
  df |>
    dplyr::arrange(date) |>
    dplyr::mutate(year = lubridate::year(date), m = lubridate::month(date)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys)), year) |>
    dplyr::mutate(value = to_date(value),
                  complete = m == dplyr::row_number()) |>   # no gap since January
    dplyr::ungroup() |>
    dplyr::mutate(value = dplyr::if_else(complete, value, NA_real_)) |>
    dplyr::select(-year, -m, -complete) |>
    shift_join(12, keys) |>
    finish_variance()
}

# All four variances for one dataset, in one long table.
compute_variances <- function(data, measure_type = "flow") {
  base <- data |>
    dplyr::filter(!is.na(date)) |>
    dplyr::distinct(series_key, date, .keep_all = TRUE) |>
    dplyr::select(series_key, date, value)
  dplyr::bind_rows(
    calc_mom(base) |> dplyr::mutate(variance_type = "MoM"),
    calc_qoq(base, measure_type = measure_type) |> dplyr::mutate(variance_type = "QoQ"),
    calc_yoy(base) |> dplyr::mutate(variance_type = "YoY"),
    calc_ytd(base, measure_type = measure_type) |> dplyr::mutate(variance_type = "YTD")
  ) |>
    dplyr::select(series_key, variance_type, date, comparison_date, value,
                  comparison_value, abs_change, pct_change, status) |>
    dplyr::arrange(series_key, variance_type, date)
}


variance_definitions <- function(measure_type = "flow") {
  flow <- measure_type == "flow"
  tibble::tibble(
    variance_type = c("MoM", "QoQ", "YoY", "YTD"),
    definition = c(
      "Month compared with the previous month.",
      if (flow) "Sum of a complete quarter (3 months published) compared with the previous complete quarter."
      else "Quarter-end month of a complete quarter compared with the previous quarter-end.",
      "Month compared with the same month a year earlier.",
      if (flow) "Cumulative January-to-month total compared with the same months a year earlier; only when every month since January is published in both years."
      else "Average of January to this month compared with the average of the same months a year earlier; only when every month since January is published in both years."
    )
  )
}
