# One long table per dataset ----------------------------------------------------

parse_period <- function(x) {
  x <- trimws(x)
  d <- suppressWarnings(lubridate::parse_date_time(
    x, orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY HM", "dmY", "Ym", "bY", "Ymd z"),
    quiet = TRUE
  ))
  lubridate::floor_date(as.Date(d), "month")
}

parse_value <- function(x) {
  x <- gsub(",", "", x, fixed = TRUE)
  x <- gsub("\u00a0", "", x, fixed = TRUE)
  suppressWarnings(as.numeric(trimws(x)))
}

# Returns:
#   data   series_key, date, value, value_raw, flag, unit, one column per filter
#   codes  dim, code, label - for rules written with codes
#   decimals of the source values (sets the rounding tolerance)
tidy_dataset <- function(raw, profile, cfg = list()) {
  n <- nrow(raw)
  get <- function(col) if (is.na(col)) rep(NA_character_, n) else raw[[col]]
  unit <- if (is.na(profile$unit_dim)) rep("Value", n) else raw[[profile$label_col[[profile$unit_dim]]]]

  data <- tibble::tibble(
    series_key = get(profile$series_col),
    date = parse_period(raw[[profile$date_col]]),
    date_raw = raw[[profile$date_col]],
    value_raw = raw[[profile$value_col]],
    value = parse_value(raw[[profile$value_col]]),
    flag = get(profile$flag_col),
    unit = unit
  )
  for (d in profile$dims) data[[d]] <- raw[[profile$label_col[[d]]]]

  # Without a series key, build one from the filters and unit.
  if (all(is.na(data$series_key))) {
    data$series_key <- do.call(paste, c(data[c(profile$dims, "unit")], sep = "."))
  }

  codes <- dplyr::bind_rows(lapply(c(profile$dims, stats::na.omit(profile$unit_dim)), function(d) {
    code_col <- if (d %in% names(raw)) d else profile$label_col[[d]]
    tibble::tibble(dim = d, code = raw[[code_col]], label = raw[[profile$label_col[[d]]]]) |>
      dplyr::distinct()
  }))

  # The UNITS column often carries the multiplier ("Euro, millions"); keep it per unit for axis titles.
  unit_notes <- if ("UNITS" %in% names(raw)) {
    u <- unique(data.frame(unit = unit, note = raw[["UNITS"]], stringsAsFactors = FALSE))
    u <- u[!duplicated(u$unit), ]
    stats::setNames(u$note, u$unit)
  } else character()

  list(
    data = data,
    codes = codes,
    decimals = source_decimals(data$value_raw),
    dims = profile$dims,
    unit_notes = unit_notes
  )
}

# Series lookup: one row per series with its unit and filter values.
series_table <- function(data, dims) {
  data |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c("series_key", "unit", dims)))) |>
    dplyr::distinct(series_key, .keep_all = TRUE)
}
