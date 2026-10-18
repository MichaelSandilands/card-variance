root <- normalizePath(file.path("..", ".."))
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
source(file.path(root, "app", "R", "storage.R"))
source(file.path(root, "app", "R", "shared.R"))
helpers <- file.path(root, "app", "R", "helpers.R")
if (file.exists(helpers)) source(helpers)

suppressPackageStartupMessages(library(dplyr))

monthly <- function(from, to) seq(as.Date(from), as.Date(to), by = "month")

series_df <- function(dates, values, key = "A") {
  tibble::tibble(series_key = key, date = as.Date(dates), value = values)
}

test_config <- function() {
  list(
    thresholds = list(mom_pct = 0.2, yoy_pct = 0.3, atv_mom_pct = 0.15,
                      sum_rule_share = 0.95, anomaly_iqr_factor = 3, materiality_share = 0.001),
    labels = list(COUNT_AREA = "Counterpart area"),
    rules = list(), cross_checks = list()
  )
}
