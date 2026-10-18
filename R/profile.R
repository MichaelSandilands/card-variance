# Column roles: which fields become filters and which are hidden, and why ------

profile_dataset <- function(raw, cfg = list(), labels = list()) {
  cols <- names(raw)
  pick <- function(override, pattern) {
    if (!is.null(override)) return(toupper(override))
    hit <- grep(pattern, cols, value = TRUE)
    if (!length(hit)) NA_character_ else hit[1]
  }
  date_col <- pick(cfg$date_col, "^REPORTING_?PERIOD$")
  value_col <- pick(cfg$value_col, "^OBSERVATION")
  series_col <- pick(cfg$series_col, "^SERIES_KEY$")
  flag_col <- pick(cfg$flag_col, "^CONFIDENTIALITY_FLAG$")
  if (is.na(date_col)) stop("No reporting period column found. Set date_col in config.")
  if (is.na(value_col)) stop("No observation column found. Set value_col in config.")

  special <- c(date_col, value_col, series_col, flag_col)
  rows <- list()
  add <- function(column, role, kept, reason) {
    v <- raw[[column]]
    ex <- utils::head(unique(v[nzchar(v)]), 3)
    rows[[length(rows) + 1]] <<- tibble::tibble(
      column = column, role = role, kept = kept, reason = reason,
      n_distinct = length(unique(v)), examples = paste(ex, collapse = " | ")
    )
  }
  add(date_col, "period", TRUE, "Reporting period, parsed to month")
  add(value_col, "value", TRUE, "Observation value")
  if (!is.na(series_col)) add(series_col, "series", TRUE, "Series identifier, shown in tables")
  if (!is.na(flag_col)) add(flag_col, "flag", TRUE, "Confidentiality flag, checked in validation")

  rest <- setdiff(cols, special)
  label_cols <- rest[grepl("_DESC$", rest)]
  code_cols <- sub("_DESC$", "", label_cols)
  paired <- code_cols[code_cols %in% rest]

  # Each logical field: a code column with its label, or a lone column.
  fields <- unique(c(paired, setdiff(rest, c(paired, paste0(paired, "_DESC")))))
  dims <- character()
  label_col <- character()
  unit_dim <- NA_character_
  constants <- list()

  is_one_to_one <- function(a, b) {
    nrow(unique(data.frame(a, b))) == length(unique(a)) &&
      length(unique(a)) == length(unique(b))
  }

  for (f in fields) {
    lab <- if (f %in% paired) paste0(f, "_DESC") else f
    vals <- raw[[lab]]
    if (f %in% paired) {
      add(f, "code", FALSE, paste0("Code; its label ", lab, " is used instead"))
    }
    if (f == "UNIT_MEASURE") {
      unit_dim <- f
      label_col[f] <- lab
      add(lab, "unit", TRUE, "Unit of measure, shown as the Measure selector")
      next
    }
    if (length(unique(vals)) <= 1) {
      constants[[f]] <- vals[1] %||% ""
      add(lab, "constant", FALSE, paste0("Same value on every row: ", vals[1] %||% ""))
      next
    }
    # A field that repeats a kept field (e.g. UNITS alongside UNIT_MEASURE_DESC).
    twin <- NA_character_
    for (k in c(if (!is.na(unit_dim)) unit_dim, dims)) {
      if (is_one_to_one(vals, raw[[label_col[[k]]]])) { twin <- label_col[[k]]; break }
    }
    if (!is.na(twin)) {
      add(lab, "redundant", FALSE, paste0("Repeats ", twin, " one-to-one"))
      next
    }
    dims <- c(dims, f)
    label_col[f] <- lab
    add(lab, "filter", TRUE, paste0("Filter: ", dim_label(f, labels)))
  }

  # UNITS can come before UNIT_MEASURE in the file; re-check it against the unit now.
  roles <- dplyr::bind_rows(rows)
  if (!is.na(unit_dim) && "UNITS" %in% dims &&
      is_one_to_one(raw[["UNITS"]], raw[[label_col[[unit_dim]]]])) {
    dims <- setdiff(dims, "UNITS")
    roles$role[roles$column == "UNITS"] <- "redundant"
    roles$kept[roles$column == "UNITS"] <- FALSE
    roles$reason[roles$column == "UNITS"] <- paste0("Repeats ", label_col[[unit_dim]], " one-to-one")
  }

  list(
    date_col = date_col, value_col = value_col, series_col = series_col,
    flag_col = flag_col, unit_dim = unit_dim, dims = dims,
    label_col = label_col, constants = constants, roles = roles,
    columns = cols
  )
}
