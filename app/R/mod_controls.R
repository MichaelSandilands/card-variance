# Sidebar: dataset, release, measure, filters, variance type, period ------------
#
# Returns a list of reactives that every tab reads, so plots and tables always
# show the same selection.

mod_controls_ui <- function(id) {
  ns <- shiny::NS(id)
  variance_label <- shiny::span(
    "Variance ",
    bslib::tooltip(
      shiny::span(class = "info-dot", tabindex = "0", `aria-label` = "What the variances mean", "?"),
      shiny::tags$dl(class = "tip-list",
        shiny::tags$dt("MoM"), shiny::tags$dd("Month against the month before."),
        shiny::tags$dt("QoQ"), shiny::tags$dd("Complete quarter against the quarter before. A quarter counts only when all three months are published."),
        shiny::tags$dt("YoY"), shiny::tags$dd("Month against the same month a year earlier."),
        shiny::tags$dt("YTD"), shiny::tags$dd("January to this month against the same months a year earlier. Stocks compare with the previous December.")),
      placement = "right"
    )
  )
  bslib::accordion(
    id = ns("panels"), multiple = TRUE, open = TRUE, class = "control-groups",
    bslib::accordion_panel(
      "Dataset", value = "dataset",
      shiny::selectInput(ns("dataset"), "File", choices = NULL),
      shiny::selectInput(ns("release"), "Release", choices = NULL),
      shiny::uiOutput(ns("unit_ui"))
    ),
    bslib::accordion_panel(
      "Filters", value = "filters",
      shiny::uiOutput(ns("dims_ui")),
      shiny::p(class = "control-note", "Leave a filter empty to include all its values.")
    ),
    bslib::accordion_panel(
      "Comparison", value = "comparison",
      shiny::radioButtons(ns("vtype"), variance_label, choices = c("MoM", "QoQ", "YoY", "YTD"),
                          selected = "MoM", inline = TRUE),
      shiny::uiOutput(ns("vtype_note")),
      shiny::sliderInput(ns("range"), "Period", min = as.Date("2022-01-01"), max = Sys.Date(),
                         value = c(as.Date("2022-01-01"), Sys.Date()), timeFormat = "%b %Y",
                         step = 31, ticks = FALSE)
    )
  )
}

mod_controls_server <- function(id, board) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    releases <- shiny::reactive({
      shiny::invalidateLater(15 * 60 * 1000)   # pick up a new release without a restart
      list_releases(board)
    })

    shiny::observe({
      r <- releases()
      shiny::req(r)
      sel <- shiny::isolate(input$release)
      if (is.null(sel) || !sel %in% r$version) sel <- r$version[1]
      shiny::updateSelectInput(session, "release", choices = stats::setNames(r$version, r$label),
                               selected = sel)
    })

    release_version <- shiny::reactive({
      shiny::req(input$release)
      input$release
    })
    release <- shiny::reactive(read_release(board, release_version()))

    shiny::observe({
      rel <- release()
      ids <- names(rel$datasets)
      sel <- shiny::isolate(input$dataset)
      if (is.null(sel) || !sel %in% ids) sel <- if ("mpcp" %in% ids) "mpcp" else ids[1]
      shiny::updateSelectInput(session, "dataset", choices = stats::setNames(ids, rel$titles[ids]),
                               selected = sel)
    })

    ds <- shiny::reactive({
      rel <- release()
      shiny::req(input$dataset, input$dataset %in% names(rel$datasets))
      read_dataset(board, input$dataset, rel$datasets[[input$dataset]]$version)
    })

    output$unit_ui <- shiny::renderUI({
      d <- ds()
      units <- sort(unique(d$series$unit))
      if (length(units) == 1) {
        return(shiny::div(class = "control-static",
                          shiny::tags$label("Measure"), shiny::div(unit_title(d, units))))
      }
      first <- units[grepl("euro", units, ignore.case = TRUE)][1]
      if (is.na(first)) first <- units[1]
      shiny::selectInput(ns("unit"), "Measure",
                         choices = stats::setNames(units, vapply(units, function(u) unit_title(d, u), "")),
                         selected = first)
    })
    unit <- shiny::reactive({
      d <- ds()
      units <- unique(d$series$unit)
      if (length(units) == 1) return(units)
      shiny::req(input$unit, input$unit %in% units)
      input$unit
    })

    output$dims_ui <- shiny::renderUI({
      d <- ds()
      s <- d$series
      shiny::tagList(lapply(d$dims, function(k) {
        ch <- sort(unique(s[[k]]))
        shiny::selectizeInput(
          ns(paste0("dim_", k)), d$dim_labels[[k]], choices = ch,
          selected = default_choice(ch, d$codes, k), multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "All values")
        )
      }))
    })

    # Filters must exist even while the sidebar or a panel is collapsed (phones start
    # collapsed); every tab waits on them.
    shiny::outputOptions(output, "dims_ui", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "unit_ui", suspendWhenHidden = FALSE)

    # Cascade: each filter offers only values that exist under the filters above it.
    offered <- shiny::reactiveValues()
    ready <- shiny::reactiveVal(NULL)    # dataset whose filters have all rendered
    shiny::observe({
      d <- ds()
      u <- unit()
      sel <- lapply(d$dims, function(k) input[[paste0("dim_", k)]])
      names(sel) <- d$dims
      if (!identical(shiny::isolate(ready()), d$id)) {
        # Before the new filters render, inputs are missing or left over from the
        # previous dataset; an empty filter afterwards means "All values".
        if (any(vapply(sel, is.null, logical(1)))) return()
        ready(d$id)
      }
      s <- d$series[d$series$unit == u, ]
      for (k in d$dims) {
        ch <- sort(unique(s[[k]]))
        cur <- sel[[k]] %||% character()
        keep <- intersect(cur, ch)
        if (length(cur) && !length(keep)) keep <- default_choice(ch, d$codes, k)
        key <- paste(d$id, k)
        if (!identical(shiny::isolate(offered[[key]]), ch) || !setequal(keep, cur)) {
          offered[[key]] <- ch
          shiny::updateSelectizeInput(session, paste0("dim_", k), choices = ch, selected = keep)
        }
        if (length(keep)) s <- s[s[[k]] %in% keep, ]
      }
    })

    filters <- shiny::reactive({
      d <- ds()
      shiny::req(identical(ready(), d$id) || !length(d$dims))
      stats::setNames(lapply(d$dims, function(k) input[[paste0("dim_", k)]] %||% character()), d$dims)
    })

    series <- shiny::reactive({
      d <- ds()
      s <- d$series[d$series$unit == unit(), ]
      f <- filters()
      for (k in names(f)) if (length(f[[k]])) s <- s[s[[k]] %in% f[[k]], ]
      s
    })

    # The slider is static (a slider made in renderUI loses its date formatter in Shiny 1.8).
    shiny::observe({
      d <- ds()
      shiny::updateSliderInput(session, "range", min = d$info$first_month, max = d$info$last_month,
                               value = c(d$info$first_month, d$info$last_month), timeFormat = "%b %Y")
    })
    range <- shiny::reactive({
      d <- ds()
      r <- input$range %||% c(d$info$first_month, d$info$last_month)
      r <- lubridate::floor_date(as.Date(r), "month")
      if (r[1] < d$info$first_month || r[2] > d$info$last_month) r <- c(d$info$first_month, d$info$last_month)
      r
    })

    vtype <- shiny::reactive(input$vtype %||% "MoM")

    output$vtype_note <- shiny::renderUI({
      d <- ds()
      s <- series()
      v <- d$variances[d$variances$series_key %in% s$series_key & d$variances$status == "ok", ]
      none <- setdiff(c("MoM", "QoQ", "YoY", "YTD"), unique(v$variance_type))
      notes <- character()
      if (length(none)) notes <- c(notes, paste(paste(none, collapse = ", "), "has no valid periods for this selection."))
      if (vtype() == "QoQ") notes <- c(notes, "QoQ uses complete quarters only (all 3 months published).")
      if (!length(notes)) return(NULL)
      shiny::div(class = "control-note", lapply(notes, shiny::p))
    })

    list(release_version = release_version, release = release, ds = ds, unit = unit,
         filters = filters, series = series, range = range, vtype = vtype,
         dataset_id = shiny::reactive(input$dataset))
  })
}
