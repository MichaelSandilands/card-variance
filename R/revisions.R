# What changed since the previous release ---------------------------------------

compare_releases <- function(tidy, previous) {
  empty <- tibble::tibble(series_key = character(), date = as.Date(character()),
                          old = numeric(), new = numeric(), change = character(),
                          difference = numeric(), pct_change = numeric())
  if (is.null(previous)) {
    return(list(available = FALSE, previous_version = NA_character_, changes = empty,
                summary = tibble::tibble(compared = 0L, months_added = 0L, revised = 0L,
                                         removed = 0L, new_history = 0L)))
  }
  tol <- 0.5 * 10^(-tidy$decimals) + 1e-9
  slim <- function(d, col) {
    d |>
      dplyr::filter(!is.na(date)) |>
      dplyr::distinct(series_key, date, .keep_all = TRUE) |>
      dplyr::select(series_key, date, !!col := value)
  }
  old <- slim(previous$data, "old")
  cur <- slim(tidy$data, "new")
  prev_latest <- max(old$date)
  j <- dplyr::full_join(old, cur, by = c("series_key", "date")) |>
    dplyr::mutate(change = dplyr::case_when(
      is.na(old) & !is.na(new) & date > prev_latest ~ "new period",
      is.na(old) & !is.na(new) ~ "new history",
      !is.na(old) & is.na(new) ~ "removed",
      !is.na(old) & !is.na(new) & abs(new - old) > tol ~ "revised",
      TRUE ~ "unchanged"
    ))
  changes <- j |>
    dplyr::filter(change %in% c("revised", "removed", "new history")) |>
    dplyr::mutate(difference = new - old,
                  pct_change = dplyr::if_else(is.na(old) | old == 0, NA_real_, difference / abs(old))) |>
    dplyr::arrange(dplyr::desc(date), series_key)
  list(
    available = TRUE,
    previous_version = previous$version,
    previous_latest = prev_latest,
    current_latest = max(cur$date),
    changes = changes,
    summary = tibble::tibble(
      compared = sum(!is.na(j$old) & j$date <= prev_latest),
      months_added = length(unique(j$date[j$change == "new period"])),
      revised = sum(j$change == "revised"),
      removed = sum(j$change == "removed"),
      new_history = sum(j$change == "new history")
    )
  )
}
