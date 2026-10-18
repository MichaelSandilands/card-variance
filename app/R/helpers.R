# App helpers: loading, formatting, palette, downloads ---------------------------

# Validated categorical order (dataviz reference palette). Never cycled: the
# app plots at most this many series at once.
SERIES_COLOURS <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                    "#e87ba4", "#008300", "#6250d6", "#e34948")
MAX_SERIES <- length(SERIES_COLOURS)
SEQ_RAMP <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
YEAR_COLOURS <- SEQ_RAMP[3:7]    # ordinal, oldest to latest; the lightest steps fail 2:1 on white
DIV_RAMP <- c("#9e2b2b", "#e34948", "#f0efec", "#3987e5", "#1c5cab")   # fall <- 0 -> rise
STATUS_COLOURS <- c(stop = "#d03b3b", fail = "#d03b3b", warn = "#fab219",
                    info = "#8a8986", pass = "#0ca30c")
STATUS_LABELS <- c(stop = "Stop", fail = "Fail", warn = "Review", info = "Info", pass = "Pass")
INK <- "#0b0b0b"
INK_2 <- "#52514e"
GRID <- "#e6e5e1"

# Session-independent cache: pins downloads are slow from object storage.
.app_cache <- new.env(parent = emptyenv())
cached <- function(key, expr) {
  if (!exists(key, envir = .app_cache, inherits = FALSE)) {
    assign(key, force(expr), envir = .app_cache)
  }
  get(key, envir = .app_cache, inherits = FALSE)
}

list_releases <- function(board) {
  v <- pin_versions_desc(board, pin_names$release)
  if (is.null(v) || !nrow(v)) return(NULL)
  v$label <- paste0("Released ", format(as.POSIXct(v$created, tz = "UTC"), "%d %b %Y %H:%M"), " UTC")
  v$label[1] <- paste(v$label[1], "(latest)")
  v
}

read_release <- function(board, version) {
  cached(paste0("release:", version), pins::pin_read(board, pin_names$release, version = version))
}

read_dataset <- function(board, id, version) {
  cached(paste0("dataset:", id, ":", version),
         pins::pin_read(board, pin_names$dataset(id), version = version))
}

read_runlog <- function(board) {
  tryCatch(pins::pin_read(board, pin_names$runlog), error = function(e) NULL)
}

# Formatting -------------------------------------------------------------------

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "\u2013", formatC(x, format = "f", digits = digits, big.mark = ","))
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "\u2013", paste0(formatC(100 * x, format = "f", digits = digits, flag = "+"), "%"))
}

# The unit, with the UNITS multiplier when there is one ("Euro, millions"). The
# Bank's files say "Exact Amount" for every unit, which only means unscaled.
unit_title <- function(ds, unit) {
  note <- unname(ds$info$unit_notes[unit])
  if (!length(note) || is.na(note) || !grepl("thousand|million|billion", note, ignore.case = TRUE)) {
    return(unit)
  }
  if (grepl(tolower(unit), tolower(note), fixed = TRUE)) note else paste0(unit, ", ", tolower(note))
}

# Decimal places for a unit: none when every value is whole (counts), two otherwise.
unit_digits <- function(ds, unit) {
  v <- ds$obs$value[ds$obs$series_key %in% ds$series$series_key[ds$series$unit == unit]]
  if (all(v == round(v), na.rm = TRUE)) 0L else 2L
}

# Thresholds for this dataset: the release's, with the dataset's own overrides.
ds_thresholds <- function(ds, rel) ds$info$thresholds %||% rel$thresholds

# The checks behind a dataset's sign-off: its own and the cross-file checks covering it.
signoff_checks <- function(ds, rel) {
  x <- rel$cross_checks
  if (is.null(x)) return(ds$validation)
  dplyr::bind_rows(ds$validation, x[x$dataset %in% c("all", ds$id), ])
}

# Releases published before 18 Oct 2026 compared a stock's YTD with the previous
# December; reopened, they keep the labels that match their numbers.
ytd_since_december <- function(ds) {
  if (!identical(ds$info$measure_type, "stock")) return(FALSE)
  y <- ds$variances[ds$variances$variance_type == "YTD" & !is.na(ds$variances$comparison_date), ]
  any(lubridate::month(y$comparison_date) != lubridate::month(y$date))
}

status_chip <- function(status, n = NULL) {
  status <- as.character(status)
  label <- STATUS_LABELS[[status]] %||% status
  if (!is.null(n)) label <- paste(n, label)
  shiny::span(class = paste0("status-chip status-", status), label)
}

# A descriptive name for one series: every filter value that varies in the dataset.
series_full_names <- function(series, all_series, dims) {
  varying <- dims[vapply(dims, function(d) length(unique(all_series[[d]])) > 1, logical(1))]
  if (!length(varying)) return(series$series_key)
  vapply(seq_len(nrow(series)), function(i) {
    vals <- vapply(varying, function(d) as.character(series[[d]][i]), "")
    keep <- vals[!is_total_like("", vals)]          # "Total" is implied, as in a statistical table
    if (!length(keep)) "Total" else paste(keep, collapse = ", ")
  }, "")
}

# Keep each series on the same colour while it stays selected; new series take free slots.
assign_colours <- function(keys, previous = integer()) {
  out <- stats::setNames(rep(NA_integer_, length(keys)), keys)
  carry <- intersect(keys, names(previous))
  out[carry] <- previous[carry]
  for (k in keys[is.na(out)]) out[k] <- setdiff(seq_along(SERIES_COLOURS), out[!is.na(out)])[1]
  out
}

# Default selection for a filter: its total-like value if there is one.
default_choice <- function(choices, codes, dim) {
  labs <- codes[codes$dim == dim, ]
  tot <- labs$label[is_total_like(labs$code, labs$label)]
  hit <- intersect(tot, choices)
  if (length(hit)) hit[1] else choices[1]
}

# Short names for plotted series: the filter values that differ between them.
series_names <- function(series, dims) {
  if (!nrow(series)) return(character())
  varying <- dims[vapply(dims, function(d) length(unique(series[[d]])) > 1, logical(1))]
  if (!length(varying)) return(rep("Selected series", nrow(series)))
  do.call(paste, c(lapply(varying, function(d) series[[d]]), sep = ", "))
}

# Rows of the variance table, with labels, for the selected series.
variance_rows <- function(ds, series, vtype, range = NULL) {
  v <- ds$variances[ds$variances$series_key %in% series$series_key &
                      ds$variances$variance_type == vtype, ]
  if (!is.null(range)) v <- v[v$date >= range[1] & v$date <= range[2], ]
  if (!nrow(v)) return(v)
  lab <- variance_labels(v$variance_type, v$date, v$comparison_date, ytd_since_december(ds))
  v$period <- lab$period
  v$compared_with <- lab$comparison
  v$series <- series_full_names(series, ds$series, ds$dims)[match(v$series_key, series$series_key)]
  v
}

# Download metadata sheet: what, from where, when, with which filters.
download_metadata <- function(state) {
  ds <- state$ds()
  rel <- state$release()
  f <- state$filters()
  filters <- if (length(f)) paste(vapply(names(f), function(k) {
    paste0(ds$dim_labels[[k]] %||% k, ": ", if (length(f[[k]])) paste(f[[k]], collapse = ", ") else "All")
  }, ""), collapse = "; ") else "None"
  def <- ds$definitions$definition[ds$definitions$variance_type == state$vtype()]
  tibble::tibble(
    field = c("Dataset", "Source", "Source file SHA-256", "Raw file version", "Retrieved at",
              "Release", "Release created", "Data from", "Data to", "Measure", "Filters",
              "Period range", "Variance", "Variance definition", "Generated at", "Data source mode"),
    value = c(ds$info$title, ds$info$resource$url %||% ds$info$dataset_url, ds$info$sha256,
              ds$info$raw_version %||% "", ds$info$retrieved_at %||% "", state$release_version(),
              rel$created, format(ds$info$first_month, "%b %Y"), format(ds$info$last_month, "%b %Y"),
              unit_title(ds, state$unit()), filters,
              paste(format(state$range(), "%b %Y"), collapse = " to "),
              state$vtype(), def %||% "", utc_now(), rel$source %||% "")
  )
}

download_buttons <- function(ns, id = "dl") {
  shiny::div(class = "dl-row",
    shiny::downloadButton(ns(paste0(id, "_csv")), "CSV", class = "btn-sm btn-outline-secondary"),
    shiny::downloadButton(ns(paste0(id, "_xlsx")), "Excel", class = "btn-sm btn-outline-secondary")
  )
}

# data_fn returns a data frame, or a named list of data frames for Excel sheets.
download_handlers <- function(output, state, data_fn, stem, id = "dl") {
  name <- function(ext) function() {
    paste0(stem(), "_", format(Sys.time(), "%Y%m%d-%H%M"), ".", ext)
  }
  output[[paste0(id, "_csv")]] <- shiny::downloadHandler(
    filename = name("csv"),
    content = function(file) {
      d <- data_fn()
      if (!is.data.frame(d)) d <- d[[1]]
      readr::write_csv(d, file, na = "")
    }
  )
  output[[paste0(id, "_xlsx")]] <- shiny::downloadHandler(
    filename = name("xlsx"),
    content = function(file) {
      d <- data_fn()
      sheets <- if (is.data.frame(d)) list(data = d) else d
      writexl::write_xlsx(c(sheets, list(metadata = download_metadata(state))), file)
    }
  )
}

empty_note <- function(...) shiny::div(class = "empty-note", ...)

# plotly defaults shared by every chart: recessive grid, text in ink tokens.
plotly_base <- function(p, ytitle = NULL, legend = TRUE) {
  p |>
    plotly::layout(
      font = list(family = "system-ui, -apple-system, Segoe UI, Roboto, sans-serif", color = INK, size = 12),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = "", gridcolor = GRID, zeroline = FALSE, showline = FALSE,
                   tickfont = list(color = INK_2)),
      yaxis = list(title = list(text = ytitle, font = list(color = INK_2)), gridcolor = GRID,
                   zeroline = FALSE, tickfont = list(color = INK_2), separatethousands = TRUE),
      hovermode = "x unified",
      hoverlabel = list(bgcolor = "#ffffff", bordercolor = GRID, font = list(color = INK)),
      showlegend = legend,
      legend = list(orientation = "h", x = 0, y = 1.12, font = list(color = INK)),
      margin = list(l = 64, r = 16, t = 36, b = 36)
    ) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

# Latest value and the latest valid MoM, QoQ, YoY and YTD for one series.
headline_figures <- function(ds, key) {
  o <- ds$obs[ds$obs$series_key == key & !is.na(ds$obs$value), ]
  if (!nrow(o)) return(NULL)
  v <- ds$variances[ds$variances$series_key == key & ds$variances$status == "ok", ]
  list(
    latest = o[which.max(o$date), ],
    variances = lapply(c("MoM", "QoQ", "YoY", "YTD"), function(t) {
      x <- v[v$variance_type == t, ]
      if (!nrow(x)) return(list(type = t, pct = NA, abs = NA))
      x <- x[which.max(x$date), ]
      lab <- variance_labels(t, x$date, x$comparison_date, ytd_since_december(ds))
      list(type = t, pct = x$pct_change, abs = x$abs_change,
           period = lab$period, comparison = lab$comparison)
    })
  )
}

# One cell of the headline ledger line.
ledger_cell <- function(label, value, context, detail = NULL, lead = FALSE, active = FALSE) {
  shiny::div(
    class = paste("ledger-cell", if (lead) "ledger-lead", if (active) "ledger-active"),
    shiny::div(class = "ledger-label", label),
    shiny::div(class = "ledger-value", value),
    shiny::div(class = "ledger-context", context),
    if (!is.null(detail)) shiny::div(class = "ledger-detail", detail)
  )
}

# Plain-language result of a release's checks, for the header.
check_sentence <- function(status) {
  status <- as.character(status)
  n <- function(s) sum(status == s)
  parts <- c(
    if (n("stop")) sprintf("%d %s blocked", n("stop"), if (n("stop") == 1) "check" else "checks"),
    if (n("fail")) sprintf("%d %s failed", n("fail"), if (n("fail") == 1) "check" else "checks"),
    if (n("warn")) sprintf("%d %s review", n("warn"), if (n("warn") == 1) "needs" else "need")
  )
  if (!length(parts)) return(sprintf("All %d checks passed.", n("pass")))
  sentence <- if (length(parts) == 1) parts else paste(paste(parts[-length(parts)], collapse = ", "), "and", parts[length(parts)])
  paste0(sentence, " before sign-off.")
}
