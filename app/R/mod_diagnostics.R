# Diagnostics: timetk plots for one series -----------------------------------------

mod_diagnostics_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "diag-head",
      shiny::selectInput(ns("series"), "Series", choices = NULL, width = "100%"),
      shiny::uiOutput(ns("note"))),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel("Time series", plotly::plotlyOutput(ns("ts"), height = "380px"),
        shiny::div(class = "chart-foot", "Did the level jump? Raw values, no smoothing.")),
      bslib::nav_panel("Seasonality", plotly::plotlyOutput(ns("seasonal"), height = "420px"),
        shiny::div(class = "chart-foot", "Is this month normal for its time of year? Each box is one calendar month across years.")),
      bslib::nav_panel("STL decomposition", plotly::plotlyOutput(ns("stl"), height = "560px"),
        shiny::div(class = "chart-foot", "Is a change trend, season, or noise? Remainder is what trend and season do not explain; the anomaly check fences it.")),
      bslib::nav_panel("Anomalies", plotly::plotlyOutput(ns("anom"), height = "380px"),
        shiny::uiOutput(ns("anom_foot")))
    )
  )
}

mod_diagnostics_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      s <- state$series()
      d <- state$ds()
      choices <- stats::setNames(s$series_key, series_full_names(s, d$series, d$dims))
      sel <- shiny::isolate(input$series)
      if (is.null(sel) || !sel %in% s$series_key) sel <- s$series_key[1]
      shiny::updateSelectInput(session, "series", choices = choices, selected = sel)
    })

    ts_data <- shiny::reactive({
      d <- state$ds()
      shiny::req(input$series)
      o <- d$obs[d$obs$series_key == input$series & !is.na(d$obs$value), c("date", "value")]
      o[order(o$date), ]
    })
    long_enough <- shiny::reactive({
      o <- ts_data()
      n <- nrow(o)
      n >= 24 && n == length(seq(min(o$date), max(o$date), by = "month"))
    })

    output$note <- shiny::renderUI({
      o <- ts_data()
      if (long_enough()) {
        return(shiny::div(class = "control-note", sprintf("%d months, %s to %s.", nrow(o),
                                                            month_label(min(o$date)), month_label(max(o$date)))))
      }
      empty_note("Seasonal, STL and anomaly plots need 24 or more unbroken months; this series has ",
                 nrow(o), ".")
    })

    style <- function(p) {
      plotly_base(p, legend = FALSE) |> plotly::layout(hovermode = "closest")
    }

    output$ts <- plotly::renderPlotly({
      o <- ts_data()
      shiny::validate(shiny::need(nrow(o) > 1, "Not enough data."))
      timetk::plot_time_series(o, date, value, .smooth = FALSE, .interactive = TRUE,
                               .line_color = SERIES_COLOURS[1], .line_size = 0.7, .title = NULL,
                               .y_lab = unit_title(state$ds(), state$unit())) |> style()
    })
    output$seasonal <- plotly::renderPlotly({
      shiny::validate(shiny::need(long_enough(), "Needs 24 or more unbroken months."))
      timetk::plot_seasonal_diagnostics(ts_data(), date, value, .feature_set = c("month.lbl", "quarter"),
                                        .interactive = TRUE, .title = NULL) |> style()
    })
    # The same fit and fences as the Validation tab's anomaly check.
    iqr_factor <- shiny::reactive(ds_thresholds(state$ds(), state$release())$anomaly_iqr_factor %||% 3)
    anomalies <- shiny::reactive({
      shiny::req(long_enough())
      o <- ts_data()
      stl_fences(o$date, o$value, iqr_factor())
    })
    output$stl <- plotly::renderPlotly({
      shiny::validate(shiny::need(long_enough(), "Needs 24 or more unbroken months."))
      timetk::plot_anomalies_decomp(anomalies(), date, .interactive = TRUE, .title = NULL,
                                    .line_color = SERIES_COLOURS[1]) |> style()
    })
    output$anom_foot <- shiny::renderUI({
      shiny::div(class = "chart-foot", sprintf(
        "Months outside the expected band: STL remainder beyond %s \u00d7 IQR, the rule the Validation tab's anomaly check applies to the latest month.",
        iqr_factor()))
    })
    output$anom <- plotly::renderPlotly({
      shiny::validate(shiny::need(long_enough(), "Needs 24 or more unbroken months."))
      timetk::plot_anomalies(anomalies(), date, .interactive = TRUE, .title = NULL,
                             .line_color = SERIES_COLOURS[1], .anom_color = STATUS_COLOURS[["fail"]]) |> style()
    })
  })
}
