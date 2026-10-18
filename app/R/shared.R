# Functions used by both the pipeline and the app (the pipeline sources this file).

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# A value that stands for "everything" in its dimension.
is_total_like <- function(code, label) {
  code <- toupper(trimws(as.character(code)))
  label <- trimws(as.character(label))
  code %in% c("_T", "W0") |
    grepl("^(total|all|world)\\b", label, ignore.case = TRUE) &
      !grepl("exclud|except|other", label, ignore.case = TRUE)
}

# Display name for a dimension code, e.g. RMT_INTTN -> "Remote or non-remote".
dim_label <- function(dim, labels = list()) {
  out <- labels[[dim]]
  if (!is.null(out)) return(out)
  x <- tolower(gsub("_", " ", dim))
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

status_levels <- c("stop", "fail", "warn", "info", "pass")

month_label <- function(d) format(d, "%b %Y")

quarter_label <- function(d) paste0("Q", lubridate::quarter(d), " ", lubridate::year(d))

utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# A period of n months (avoids relying on lubridate being attached).
mths <- function(n) lubridate::period(n, units = "month")

# Labels such as "Q2 2026" and "Jan-Aug 2026 vs Jan-Aug 2025".
variance_labels <- function(variance_type, date, comparison_date, measure_type = "flow") {
  cmp_missing <- is.na(comparison_date)
  prior_year <- lubridate::add_with_rollback(date, mths(-12))
  ytd_span <- function(d) paste0("Jan\u2013", format(d, "%b %Y"))
  period <- dplyr::case_when(
    variance_type == "QoQ" ~ quarter_label(date),
    variance_type == "YTD" & measure_type == "flow" ~ ytd_span(date),
    TRUE ~ month_label(date)
  )
  comparison <- dplyr::case_when(
    variance_type == "QoQ" ~ quarter_label(lubridate::add_with_rollback(date, mths(-3))),
    variance_type == "YTD" & measure_type == "flow" ~ ytd_span(prior_year),
    variance_type == "YTD" ~ month_label(as.Date(paste0(lubridate::year(date) - 1, "-12-01"))),
    variance_type == "MoM" ~ month_label(lubridate::add_with_rollback(date, mths(-1))),
    TRUE ~ month_label(prior_year)
  )
  list(period = period, comparison = comparison)
}

# County names -------------------------------------------------------------------

COUNTIES_26 <- c(
  "CARLOW", "CAVAN", "CLARE", "CORK", "DONEGAL", "DUBLIN", "GALWAY", "KERRY",
  "KILDARE", "KILKENNY", "LAOIS", "LEITRIM", "LIMERICK", "LONGFORD", "LOUTH",
  "MAYO", "MEATH", "MONAGHAN", "OFFALY", "ROSCOMMON", "SLIGO", "TIPPERARY",
  "WATERFORD", "WESTMEATH", "WEXFORD", "WICKLOW"
)

COUNTY_ALIASES <- c(
  "LAOIGHIS" = "LAOIS", "QUEENS" = "LAOIS", "KINGS" = "OFFALY",
  "FINGAL" = "DUBLIN", "SOUTH DUBLIN" = "DUBLIN", "DUBLIN CITY" = "DUBLIN",
  "DUN LAOGHAIRE-RATHDOWN" = "DUBLIN", "DUN LAOGHAIRE RATHDOWN" = "DUBLIN",
  "NORTH TIPPERARY" = "TIPPERARY", "SOUTH TIPPERARY" = "TIPPERARY",
  "TIPPERARY NORTH" = "TIPPERARY", "TIPPERARY SOUTH" = "TIPPERARY",
  "CORK CITY" = "CORK", "GALWAY CITY" = "GALWAY",
  "LIMERICK CITY" = "LIMERICK", "WATERFORD CITY" = "WATERFORD"
)

# Upper case, no fadas, no "Co." prefix or "County"/"City" suffix, merged local authorities.
normalise_county <- function(x) {
  # Latin-ASCII drops fadas and turns en dashes into hyphens, whatever the locale.
  x <- stringi::stri_trans_general(enc2utf8(as.character(x)), "Latin-ASCII")
  x <- toupper(trimws(gsub("[^A-Za-z -]", "", x)))
  x <- sub("^(CO|COUNTY)\\s+", "", x)
  x <- sub("\\s+(COUNTY COUNCIL|CITY AND COUNTY COUNCIL|CITY COUNCIL|COUNTY)$", "", x)
  x <- gsub("\\s+", " ", x)
  hit <- x %in% names(COUNTY_ALIASES)
  x[hit] <- COUNTY_ALIASES[x[hit]]
  x
}

