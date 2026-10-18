# Validation checks ------------------------------------------------------------
#
# Each check returns one row: what was tested, how many items failed, and the
# failing rows themselves (details). Status is "pass" when nothing failed,
# otherwise the check's severity:
#   stop - the release is not published over the previous one
#   fail - published and shown red
#   warn - published and shown amber, for review
#   info - context only

new_check <- function(dataset, check, title, category, severity, n_tested,
                      failures = NULL, note = "", rule_source = "Built-in",
                      status = NULL) {
  failures <- failures %||% tibble::tibble()
  n_failed <- nrow(failures)
  status <- status %||% if (n_failed == 0) "pass" else severity
  tibble::tibble(
    dataset = dataset, check = check, title = title, category = category,
    severity = severity, status = status,
    n_tested = as.integer(n_tested), n_failed = as.integer(n_failed),
    note = note, rule_source = rule_source,
    details = list(utils::head(failures, 5000))
  )
}

check_schema <- function(raw, id, schema_dir = file.path("config", "schemas")) {
  observed <- names(raw)
  path <- file.path(schema_dir, paste0(id, ".csv"))
  if (!file.exists(path)) {
    dir.create(schema_dir, recursive = TRUE, showWarnings = FALSE)
    obs_path <- file.path(schema_dir, paste0(id, ".observed.csv"))
    readr::write_csv(tibble::tibble(column = observed), obs_path)
    return(list(status = "warn", approved = FALSE, observed = observed,
                missing = character(), extra = character(),
                message = paste0("No approved column list yet. Observed columns were written to ",
                                 obs_path, "; review it and rename it to ", id, ".csv.")))
  }
  approved <- toupper(readr::read_csv(path, show_col_types = FALSE)$column)
  missing <- setdiff(approved, observed)
  extra <- setdiff(observed, approved)
  ok <- !length(missing) && !length(extra)
  list(status = if (ok) "pass" else "stop", approved = TRUE, observed = observed,
       missing = missing, extra = extra,
       message = if (ok) "Columns match the approved list." else
         "Columns differ from the approved list in config/schemas.")
}

read_annotations <- function(path = file.path("config", "annotations.csv")) {
  if (!file.exists(path)) return(tibble::tibble(dataset = character(), month = character(),
                                                 series_pattern = character(), note = character()))
  readr::read_csv(path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
}

# Mark failures that a known event explains (config/annotations.csv).
explain_failures <- function(failures, id, annotations) {
  failures$explanation <- rep(NA_character_, nrow(failures))
  if (!nrow(failures)) return(failures)
  a <- annotations[annotations$dataset %in% c(id, "*"), , drop = FALSE]
  for (i in seq_len(nrow(a))) {
    hit <- format(failures$date, "%Y-%m") == a$month[i] &
      grepl(a$series_pattern[i] %||% ".", failures$series_key)
    failures$explanation[hit & is.na(failures$explanation)] <- a$note[i]
  }
  failures
}

# Totals that should equal, or at least contain, their parts --------------------

total_label <- function(codes, d) {
  labs <- unique(codes[codes$dim == d, c("code", "label")])
  tot <- unique(labs$label[is_total_like(labs$code, labs$label)])
  if (length(tot) == 1) tot else NA_character_
}

group_totals <- function(data, d, parent, children = NULL, dims) {
  keys <- c("date", "unit", setdiff(dims, d))
  x <- data[!is.na(data$value) & !is.na(data$date), c(keys, d, "value")]
  names(x)[names(x) == d] <- ".dim"
  if (!is.null(children)) x <- x[x$.dim %in% c(parent, children), ]
  x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      total = sum(value[.dim == parent]),
      n_total = sum(.dim == parent),
      sum_of_parts = sum(value[.dim != parent]),
      n_parts = sum(.dim != parent),
      largest_part = if (any(.dim != parent)) max(value[.dim != parent]) else NA_real_,
      largest_part_name = if (any(.dim != parent)) .dim[.dim != parent][which.max(value[.dim != parent])] else NA_character_,
      .groups = "drop"
    ) |>
    dplyr::filter(n_total == 1, n_parts >= 1)
}

reconcile_dataset <- function(id, data, dims, codes, decimals, labels, config) {
  tol_unit <- 0.5 * 10^(-decimals)
  share <- config$thresholds$sum_rule_share %||% 0.95
  checks <- list()
  for (d in dims) {
    tot <- total_label(codes, d)
    if (is.na(tot)) next
    g <- group_totals(data, d, tot, dims = dims)
    if (!nrow(g)) next
    g <- g |>
      dplyr::mutate(difference = total - sum_of_parts,
                    tolerance = tol_unit * (n_parts + 1) + 1e-9 * abs(total))
    hist <- g[g$date < max(g$date) & g$n_parts >= 2, ]
    held <- if (nrow(hist) >= 3) mean(abs(hist$difference) <= hist$tolerance) else 0
    dl <- dim_label(d, labels)
    is_sum <- held >= share
    if (is_sum) {
      fails <- g[g$n_parts >= 2 & abs(g$difference) > g$tolerance, ]
      checks[[length(checks) + 1]] <- new_check(
        id, paste0("sum_", d), paste0(dl, ": \u201c", tot, "\u201d equals the sum of its parts"),
        "Reconciliation", "fail", nrow(g[g$n_parts >= 2, ]), fails,
        note = sprintf("Learned rule: held in %.0f%% of earlier months. Tolerance is the source rounding.", 100 * held),
        rule_source = "Learned from history"
      )
    }
    over <- g[!is.na(g$largest_part) & g$largest_part > g$total + g$tolerance, ]
    checks[[length(checks) + 1]] <- new_check(
      id, paste0("subset_", d), paste0(dl, ": no part exceeds \u201c", tot, "\u201d"),
      "Reconciliation", if (is_sum) "fail" else "warn", nrow(g), over,
      note = if (is_sum) "Parts add up to the total, so none may exceed it."
             else "Parts overlap (for example contactless within point of sale), so only containment is checked.",
      rule_source = "Learned from history"
    )
  }
  # Explicit rules from config.
  for (r in Filter(function(r) identical(r$dataset, id), config$rules %||% list())) {
    if (!r$dim %in% dims) next
    g <- group_totals(data, r$dim, r$parent, unlist(r$children), dims = dims) |>
      dplyr::filter(n_parts == length(r$children)) |>
      dplyr::mutate(difference = total - sum_of_parts,
                    tolerance = tol_unit * (n_parts + 1) + 1e-9 * abs(total))
    fails <- if (identical(r$type, "subset")) g[g$largest_part > g$total + g$tolerance, ]
             else g[abs(g$difference) > g$tolerance, ]
    checks[[length(checks) + 1]] <- new_check(
      id, paste0("rule_", make.names(r$name)), r$name, "Reconciliation",
      r$severity %||% "fail", nrow(g), fails, rule_source = "config/datasets.yml"
    )
  }
  dplyr::bind_rows(checks)
}

# Plausibility of the latest month -----------------------------------------------

latest_movements <- function(id, variances, series, config, annotations) {
  th <- config$thresholds
  mat <- th$materiality_share %||% 0.001
  latest <- max(variances$date[variances$variance_type == "MoM"], na.rm = TRUE)
  v <- variances |>
    dplyr::filter(date == latest, variance_type %in% c("MoM", "YoY"), status == "ok") |>
    dplyr::left_join(series[c("series_key", "unit")], by = "series_key") |>
    dplyr::group_by(unit) |>
    dplyr::mutate(material = abs(value) >= mat * suppressWarnings(max(abs(value), na.rm = TRUE))) |>
    dplyr::ungroup()
  if (!nrow(v)) {
    return(new_check(id, "large_movement", paste0("Large movements in ", month_label(latest)),
                     "Plausibility", "warn", 0, NULL, note = "No comparable series in the latest month."))
  }
  limit <- ifelse(v$variance_type == "MoM", th$mom_pct %||% 0.2, th$yoy_pct %||% 0.3)
  fails <- v[v$material & abs(v$pct_change) > limit, ] |>
    dplyr::select(series_key, unit, variance_type, date, value, comparison_value, pct_change)
  fails <- explain_failures(fails, id, annotations)
  unexplained <- fails[is.na(fails$explanation), ]
  new_check(
    id, "large_movement", paste0("Large movements in ", month_label(latest)),
    "Plausibility", "warn", sum(v$material), fails,
    note = sprintf("MoM beyond %.0f%% or YoY beyond %.0f%%, for series above %.1f%% of the largest in their unit. %d explained by annotations.",
                   100 * (th$mom_pct %||% 0.2), 100 * (th$yoy_pct %||% 0.3), 100 * mat,
                   nrow(fails) - nrow(unexplained)),
    status = if (nrow(unexplained)) "warn" else "pass"
  )
}

# STL remainder outside IQR fences, the method timetk::anomalize() uses.
latest_anomalies <- function(id, data, config, annotations) {
  k <- config$thresholds$anomaly_iqr_factor %||% 3
  latest <- max(data$date, na.rm = TRUE)
  d <- data[!is.na(data$value) & !is.na(data$date), c("series_key", "date", "value")]
  d <- d[order(d$series_key, d$date), ]
  tested <- 0L
  rows <- lapply(split(d, d$series_key), function(s) {
    n <- nrow(s)
    if (n < 24 || s$date[n] != latest) return(NULL)
    if (n != length(seq(s$date[1], latest, by = "month"))) return(NULL)
    if (stats::sd(s$value) == 0) return(NULL)
    fit <- tryCatch(stats::stl(stats::ts(s$value, frequency = 12), s.window = "periodic", robust = TRUE),
                    error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    tested <<- tested + 1L
    rem <- as.numeric(fit$time.series[, "remainder"])
    q <- stats::quantile(rem, c(0.25, 0.75), names = FALSE)
    iqr <- q[2] - q[1]
    lo <- q[1] - k * iqr
    hi <- q[2] + k * iqr
    r <- rem[n]
    if (iqr > 0 && (r < lo || r > hi)) {
      tibble::tibble(series_key = s$series_key[1], date = latest, value = s$value[n],
                     remainder = r, lower = lo, upper = hi)
    }
  })
  fails <- dplyr::bind_rows(rows)
  fails <- explain_failures(fails, id, annotations)
  unexplained <- if (nrow(fails)) fails[is.na(fails$explanation), ] else fails
  new_check(
    id, "anomaly", paste0("Unusual values in ", month_label(latest)),
    "Plausibility", "warn", tested, fails,
    note = sprintf("STL decomposition with IQR fences (k = %s), on series with 24+ unbroken months.", k),
    status = if (nrow(unexplained)) "warn" else "pass"
  )
}

# Value / count pairs: average transaction value.
atv_check <- function(id, data, dims, config) {
  units <- unique(data$unit)
  cur <- units[grepl("euro|eur\\b", units, ignore.case = TRUE)]
  cnt <- units[grepl("pure number|number|^pn$|count", units, ignore.case = TRUE)]
  if (length(cur) != 1 || length(cnt) != 1) return(NULL)
  w <- data[!is.na(data$date) & data$unit %in% c(cur, cnt), c("date", "unit", dims, "value")] |>
    dplyr::distinct(dplyr::across(c("date", "unit", dplyr::all_of(dims))), .keep_all = TRUE) |>
    tidyr::pivot_wider(names_from = unit, values_from = value)
  names(w)[names(w) == cur] <- ".value"
  names(w)[names(w) == cnt] <- ".count"
  if (!all(c(".value", ".count") %in% names(w))) return(NULL)
  w <- w[!is.na(w$.value) & !is.na(w$.count) & w$.count > 0, ]
  w$atv <- w$.value / w$.count
  latest <- max(w$date)
  prev <- lubridate::add_with_rollback(latest, mths(-1))
  a <- dplyr::inner_join(
    w[w$date == latest, c(dims, "atv", ".value")],
    w[w$date == prev, c(dims, "atv")] |> dplyr::rename(atv_previous = atv),
    by = dims
  )
  mat <- config$thresholds$materiality_share %||% 0.001
  a <- a[abs(a$.value) >= mat * max(abs(a$.value)), ]
  a$pct_change <- a$atv / a$atv_previous - 1
  limit <- config$thresholds$atv_mom_pct %||% 0.15
  fails <- a[abs(a$pct_change) > limit, ] |>
    dplyr::mutate(date = latest) |>
    dplyr::select(dplyr::all_of(dims), date, atv, atv_previous, pct_change)
  new_check(
    id, "atv", paste0("Average transaction value in ", month_label(latest)),
    "Plausibility", "warn", nrow(a), fails,
    note = sprintf("Value \u00f7 count compared with the previous month; flagged beyond %.0f%%.", 100 * limit)
  )
}

# All checks for one dataset ---------------------------------------------------

validate_dataset <- function(id, raw, profile, tidy, variances, revisions, schema, config) {
  data <- tidy$data
  dims <- profile$dims
  annotations <- read_annotations()
  checks <- list()

  sch_fail <- dplyr::bind_rows(
    tibble::tibble(column = schema$missing, problem = rep("Missing from the file", length(schema$missing))),
    tibble::tibble(column = schema$extra, problem = rep("New column, not approved", length(schema$extra)))
  )
  checks$schema <- new_check(id, "schema", "Columns match the approved list", "Structure", "stop",
                             length(schema$observed), sch_fail, note = schema$message,
                             status = schema$status)

  bad_dates <- data[is.na(data$date), c("series_key", "date_raw")]
  checks$dates <- new_check(id, "dates", "Every reporting period parses to a month", "Structure",
                            "stop", nrow(data), bad_dates)

  bad_values <- data[is.na(data$value) & nzchar(data$value_raw), c("series_key", "date", "value_raw")]
  checks$values <- new_check(id, "numeric", "Every observation is a number", "Structure",
                             "stop", nrow(data), bad_values)

  dupes <- data |>
    dplyr::count(series_key, date, name = "rows") |>
    dplyr::filter(rows > 1)
  checks$unique <- new_check(id, "unique", "One row per series and month", "Structure", "stop",
                             nrow(data), dupes)

  freq <- profile$constants[["FREQ_DESC"]] %||% profile$constants[["FREQ"]] %||% NA_character_
  freq_ok <- is.na(freq) || grepl("^m$|month", freq, ignore.case = TRUE)
  checks$freq <- new_check(id, "frequency", "Frequency is monthly", "Structure", "fail", 1,
                           if (freq_ok) NULL else tibble::tibble(frequency = freq))

  empty <- data[!nzchar(data$value_raw), c("series_key", "date", "flag")]
  checks$missing <- new_check(id, "missing", "Observations are published (not blank)", "Completeness",
                              "warn", nrow(data), empty,
                              note = "Blank values are often confidential; the flag column shows why.")

  ok <- data[!is.na(data$date), ]
  gaps <- ok |>
    dplyr::distinct(series_key, date) |>
    dplyr::group_by(series_key) |>
    dplyr::summarise(first = min(date), last = max(date),
                     present = list(date), .groups = "drop")
  gap_rows <- dplyr::bind_rows(lapply(seq_len(nrow(gaps)), function(i) {
    want <- seq(gaps$first[i], gaps$last[i], by = "month")
    miss <- want[!want %in% gaps$present[[i]]]
    if (length(miss)) tibble::tibble(series_key = gaps$series_key[i], missing_month = miss)
  }))
  checks$gaps <- new_check(id, "gaps", "No missing months inside a series", "Completeness", "fail",
                           nrow(gaps), gap_rows)

  latest <- max(ok$date)
  stale <- gaps[gaps$last < latest, c("series_key", "last")]
  checks$stale <- new_check(id, "stale", paste0("Every series reaches ", month_label(latest)),
                            "Completeness", "warn", nrow(gaps), stale,
                            note = "A series that stops early may have been discontinued.")

  neg <- data[!is.na(data$value) & data$value < 0, c("series_key", "date", "value")]
  checks$negative <- new_check(id, "negative", "No negative values", "Plausibility", "fail",
                               sum(!is.na(data$value)), neg)

  checks$recon <- reconcile_dataset(id, ok, dims, tidy$codes, tidy$decimals, config$labels, config)
  series <- series_table(data, dims)
  checks$moves <- latest_movements(id, variances, series, config, annotations)
  checks$anom <- latest_anomalies(id, ok, config, annotations)
  checks$atv <- atv_check(id, ok, dims, config)

  if (isTRUE(revisions$available)) {
    s <- revisions$summary
    removed <- revisions$changes[revisions$changes$change == "removed", ]
    revised <- revisions$changes[revisions$changes$change == "revised", ]
    checks$removed <- new_check(id, "removed", "No published values removed since the previous release",
                                "Revisions", "fail", s$compared, removed)
    checks$revised <- new_check(id, "revised", "Earlier values unchanged since the previous release",
                                "Revisions", "warn", s$compared, revised,
                                note = "Revisions are normal but should be explained before sign-off.")
    checks$advance <- new_check(id, "advance", "Release adds at most one new month", "Revisions",
                                "warn", 1,
                                if (s$months_added > 1) tibble::tibble(months_added = s$months_added) else NULL)
  } else {
    checks$revised <- new_check(id, "revised", "Earlier values unchanged since the previous release",
                                "Revisions", "info", 0, NULL,
                                note = "No previous release on the board yet.", status = "info")
  }

  dplyr::bind_rows(checks) |>
    dplyr::mutate(status = factor(status, levels = status_levels))
}

# Checks across files -------------------------------------------------------------

cross_file_checks <- function(results, geo, config) {
  results <- stats::setNames(results, vapply(results, function(r) r$id, ""))
  checks <- list()
  ends <- tibble::tibble(
    dataset = names(results),
    title = vapply(results, function(r) r$info$title, ""),
    last_month = do.call(c, lapply(results, function(r) r$info$last_month))
  )
  odd <- ends[ends$last_month != max(ends$last_month), ]
  checks$ends <- new_check("all", "same_end", "All files end in the same month", "Cross-file",
                           "warn", nrow(ends), odd)

  for (r in results) {
    cd <- r$info$county_dim
    if (is.null(cd) || !cd %in% r$dims) next
    labs <- r$codes[r$codes$dim == cd, ]
    labs <- labs[!is_total_like(labs$code, labs$label), ]
    keys <- normalise_county(labs$label)
    unmatched <- labs[!keys %in% geo$county_key, c("code", "label")]
    unmatched <- unmatched[!grepl("unallocated|unknown|not allocated|online|other", unmatched$label,
                                  ignore.case = TRUE), ]
    checks[[paste0("county_", r$id)]] <- new_check(
      r$id, "county_match", "Every county in the data has a boundary", "Cross-file", "fail",
      nrow(labs), unmatched)
    no_data <- tibble::tibble(county = geo$county[!geo$county_key %in% keys])
    checks[[paste0("county_cov_", r$id)]] <- new_check(
      r$id, "county_coverage", "Every county boundary has data", "Cross-file", "warn",
      nrow(geo), no_data)
  }

  for (cc in config$cross_checks %||% list()) {
    checks[[make.names(cc$name)]] <- configured_cross_check(cc, results)
  }
  dplyr::bind_rows(checks) |>
    dplyr::mutate(status = factor(status, levels = status_levels))
}

configured_cross_check <- function(cc, results) {
  pull_side <- function(side) {
    r <- results[[side$dataset]]
    if (is.null(r)) stop("Cross-check '", cc$name, "' names unknown dataset ", side$dataset)
    d <- dplyr::left_join(r$obs, r$series, by = "series_key")
    for (k in names(side$where %||% list())) d <- d[d[[k]] %in% unlist(side$where[[k]]), ]
    if (!is.null(side$sum_over)) {
      tl <- total_label(r$codes, side$sum_over)
      if (!is.na(tl)) d <- d[d[[side$sum_over]] != tl, ]
    }
    d |> dplyr::group_by(date, unit) |> dplyr::summarise(value = sum(value), .groups = "drop")
  }
  l <- pull_side(cc$left)
  r <- pull_side(cc$right)
  j <- dplyr::inner_join(l, r, by = c("date", "unit"), suffix = c("_left", "_right")) |>
    dplyr::mutate(difference = value_left - value_right,
                  pct = difference / abs(value_right))
  tol <- cc$tolerance_pct %||% 0.001
  new_check("all", paste0("cross_", make.names(cc$name)), cc$name, "Cross-file",
            cc$severity %||% "warn", nrow(j), j[abs(j$pct) > tol, ],
            rule_source = "config/datasets.yml")
}
