# County choropleth with a sorted table beside it ------------------------------------

mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "caveat",
      "County is the location of the terminal that processed the payment. Online spend is booked to the ",
      "merchant's head office, which inflates Dublin."),
    shiny::div(class = "map-controls",
      shiny::selectInput(ns("metric"), "Show", choices = NULL),
      shiny::selectInput(ns("period"), "Period", choices = NULL)),
    shiny::uiOutput(ns("selection_note")),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(full_screen = TRUE, leaflet::leafletOutput(ns("map"), height = "560px")),
      bslib::card(
        bslib::card_header(shiny::div(class = "card-head", shiny::span(class = "card-title", "Counties, sorted"),
                                      download_buttons(ns))),
        reactable::reactableOutput(ns("table")),
        shiny::uiOutput(ns("excluded")))
    ),
    bslib::card(
      bslib::card_header(shiny::uiOutput(ns("county_title"))),
      plotly::plotlyOutput(ns("county_trend"), height = "260px"))
  )
}

mod_map_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    county_dim <- shiny::reactive({
      cd <- state$ds()$info$county_dim
      shiny::req(!is.null(cd), cd %in% state$ds()$dims)
      cd
    })
    geo <- shiny::reactive(state$release()$geo)
    pop <- shiny::reactive(state$release()$population)

    shiny::observe({
      county_dim()
      choices <- c("Value" = "value")
      if (!is.null(pop())) choices <- c(choices, "Per 1,000 residents" = "per_1000")
      choices <- c(choices, "Share of all counties" = "share", "% change (selected variance)" = "pct")
      sel <- shiny::isolate(input$metric)
      shiny::updateSelectInput(session, "metric", choices = choices,
                               selected = if (!is.null(sel) && sel %in% choices) sel else "value")
    })

    # One series per county: other filters take their first selected value.
    county_series <- shiny::reactive({
      d <- state$ds()
      cd <- county_dim()
      s <- d$series[d$series$unit == state$unit(), ]
      f <- state$filters()
      used <- list()
      for (k in setdiff(d$dims, cd)) {
        pick <- f[[k]]
        if (!length(pick)) pick <- default_choice(sort(unique(s[[k]])), d$codes, k)
        pick <- pick[1]
        used[[d$dim_labels[[k]]]] <- pick
        s <- s[s[[k]] == pick, ]
      }
      labs <- d$codes[d$codes$dim == cd, ]
      s$county_key <- normalise_county(s[[cd]])
      s$is_total <- s[[cd]] %in% labs$label[is_total_like(labs$code, labs$label)]
      attr(s, "used") <- used
      s
    })

    periods <- shiny::reactive({
      d <- state$ds()
      o <- d$obs[d$obs$series_key %in% county_series()$series_key, ]
      if (state$vtype() == "QoQ" && identical(input$metric, "pct")) {
        v <- d$variances[d$variances$series_key %in% county_series()$series_key &
                           d$variances$variance_type == "QoQ", ]
        dates <- sort(unique(v$date), decreasing = TRUE)
        return(stats::setNames(as.character(dates), quarter_label(dates)))
      }
      dates <- sort(unique(o$date), decreasing = TRUE)
      stats::setNames(as.character(dates), month_label(dates))
    })
    shiny::observe({
      p <- periods()
      sel <- shiny::isolate(input$period)
      shiny::updateSelectInput(session, "period", choices = p,
                               selected = if (!is.null(sel) && sel %in% p) sel else p[1])
    })

    values <- shiny::reactive({
      shiny::req(input$period, input$metric)
      d <- state$ds()
      s <- county_series()
      when <- as.Date(input$period)
      counties <- s[!s$is_total & s$county_key %in% geo()$county_key, ]
      o <- d$obs[d$obs$series_key %in% counties$series_key & d$obs$date == when, c("series_key", "value")]
      x <- dplyr::left_join(counties[c("series_key", "county_key")], o, by = "series_key")
      if (input$metric == "pct") {
        v <- d$variances[d$variances$series_key %in% counties$series_key &
                           d$variances$variance_type == state$vtype() & d$variances$date == when,
                         c("series_key", "pct_change", "status")]
        x <- dplyr::left_join(x, v, by = "series_key")
        x$metric <- x$pct_change
        all_v <- d$variances$pct_change[d$variances$series_key %in% counties$series_key &
                                          d$variances$variance_type == state$vtype()]
        lim <- stats::quantile(abs(all_v), 0.98, na.rm = TRUE, names = FALSE)
        attr(x, "limit") <- if (is.finite(lim) && lim > 0) lim else 0.1
      } else if (input$metric == "per_1000") {
        x <- dplyr::left_join(x, pop(), by = "county_key")
        x$metric <- 1000 * x$value / x$population
      } else if (input$metric == "share") {
        x$metric <- x$value / sum(x$value, na.rm = TRUE)
      } else {
        x$metric <- x$value
      }
      x$county <- geo()$county[match(x$county_key, geo()$county_key)]
      x
    })

    metric_label <- shiny::reactive({
      u <- unit_title(state$ds(), state$unit())
      switch(input$metric, value = u, per_1000 = paste(u, "per 1,000 residents"),
             share = "Share of all counties", pct = paste(state$vtype(), "% change"), u)
    })
    fmt_metric <- function(x) {
      if (input$metric %in% c("share", "pct")) {
        if (input$metric == "share") paste0(formatC(100 * x, format = "f", digits = 1), "%") else fmt_pct(x)
      } else fmt_num(x)
    }

    output$selection_note <- shiny::renderUI({
      used <- attr(county_series(), "used")
      if (!length(used)) return(NULL)
      shiny::div(class = "control-note",
                 "Map uses one series per county: ",
                 paste0(paste(paste0(names(used), " = ", unlist(used)), collapse = "; "), "."))
    })

    # Drawn in one render (not a proxy) so the map is complete whenever the tab opens.
    output$map <- leaflet::renderLeaflet({
      g <- geo()
      shiny::validate(shiny::need(!is.null(g) && nrow(g) > 0, "County boundaries are not in this release."))
      x <- values()
      g$metric <- x$metric[match(g$county_key, x$county_key)]
      pct <- input$metric == "pct"
      lim <- attr(x, "limit")
      pal <- if (pct) {
        leaflet::colorNumeric(DIV_RAMP, domain = c(-lim, lim), na.color = "#d9d8d4")
      } else {
        leaflet::colorNumeric(SEQ_RAMP, domain = range(g$metric, na.rm = TRUE), na.color = "#d9d8d4")
      }
      fill <- if (pct) pal(pmax(pmin(g$metric, lim), -lim)) else pal(g$metric)
      # Leaflet lists legend values low to high; reversing both puts high values at the top.
      legend_pal <- if (pct) {
        leaflet::colorNumeric(DIV_RAMP, domain = c(-lim, lim), na.color = "#d9d8d4", reverse = TRUE)
      } else {
        leaflet::colorNumeric(SEQ_RAMP, domain = range(g$metric, na.rm = TRUE), na.color = "#d9d8d4", reverse = TRUE)
      }
      labels <- sprintf("<strong>%s</strong><br>%s: %s", g$county, metric_label(), fmt_metric(g$metric))
      leaflet::leaflet(g, options = leaflet::leafletOptions(zoomSnap = 0.25, attributionControl = FALSE)) |>
        leaflet::addPolygons(
          layerId = ~county_key, fillColor = fill, fillOpacity = 0.9, color = "#ffffff", weight = 1.5,
          label = lapply(labels, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(weight = 2.5, color = INK, bringToFront = TRUE)
        ) |>
        leaflet::addLegend(
          "bottomright", pal = legend_pal, values = if (pct) c(-lim, lim) else g$metric,
          title = metric_label(), opacity = 0.9,
          labFormat = if (input$metric %in% c("pct", "share"))
            leaflet::labelFormat(suffix = "%", transform = function(v) round(100 * sort(v, decreasing = TRUE), 1))
          else leaflet::labelFormat(big.mark = ",", transform = function(v) sort(v, decreasing = TRUE))
        ) |>
        leaflet::fitBounds(-10.6, 51.4, -5.9, 55.4)
    })

    output$table <- reactable::renderReactable({
      x <- values()
      x <- x[order(x$metric, decreasing = TRUE), ]
      t <- tibble::tibble(rank = seq_len(nrow(x)), county = x$county, value = x$value,
                          metric = fmt_metric(x$metric))
      cols <- list(
        rank = reactable::colDef(name = "#", width = 44),
        county = reactable::colDef(name = "County"),
        value = reactable::colDef(name = unit_title(state$ds(), state$unit()), align = "right",
                                  format = reactable::colFormat(separators = TRUE, digits = 2)),
        metric = reactable::colDef(name = metric_label(), align = "right")
      )
      if (input$metric == "value") cols$metric <- reactable::colDef(show = FALSE)
      reactable::reactable(t, columns = cols, compact = TRUE, pagination = FALSE, height = 470,
                           highlight = TRUE)
    })

    output$excluded <- shiny::renderUI({
      s <- county_series()
      out <- unique(s[[county_dim()]][s$is_total | !s$county_key %in% geo()$county_key])
      if (!length(out)) return(NULL)
      shiny::div(class = "chart-foot", "Not mapped: ", paste(out, collapse = ", "),
                 " (totals and values without a boundary).")
    })

    clicked <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$map_shape_click, clicked(input$map_shape_click$id))
    output$county_title <- shiny::renderUI({
      k <- clicked()
      shiny::span(class = "card-title",
                  if (is.null(k)) "Click a county to see its trend" else
                    paste0(geo()$county[geo()$county_key == k], ", ", unit_title(state$ds(), state$unit())))
    })
    output$county_trend <- plotly::renderPlotly({
      k <- clicked()
      shiny::validate(shiny::need(!is.null(k), "Click a county on the map."))
      s <- county_series()
      key <- s$series_key[s$county_key == k & !s$is_total][1]
      o <- state$ds()$obs[state$ds()$obs$series_key == key, ]
      p <- plotly::plot_ly(o, x = ~date, y = ~value, type = "scatter", mode = "lines",
                           line = list(color = SERIES_COLOURS[1], width = 2),
                           hovertemplate = "%{x|%b %Y}: %{y:,.2f}<extra></extra>")
      plotly_base(p, unit_title(state$ds(), state$unit()), legend = FALSE)
    })

    download_handlers(output, state, function() {
      x <- values()
      tibble::tibble(county = x$county, series_key = x$series_key, period = input$period,
                     value = x$value, metric = x$metric, metric_name = metric_label())
    }, function() paste0(state$ds()$id, "_map"))
  })
}
