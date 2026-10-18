# Trend: level lines with the chosen variance beneath ----------------------------

mod_trend_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("ledger")),
    shiny::uiOutput(ns("note")),
    bslib::card(
      full_screen = TRUE, height = "420px",
      bslib::card_header(shiny::div(class = "card-head",
        shiny::uiOutput(ns("level_title"), inline = TRUE), download_buttons(ns, "dl_level"))),
      bslib::card_body(plotly::plotlyOutput(ns("level"), height = "100%"), padding = c(8, 12))
    ),
    bslib::card(
      full_screen = TRUE, height = "400px",
      bslib::card_header(shiny::div(class = "card-head",
        shiny::uiOutput(ns("var_title"), inline = TRUE),
        shiny::div(class = "head-controls",
          shiny::radioButtons(ns("as"), NULL, c("% change" = "pct", "Change" = "abs"), inline = TRUE),
          download_buttons(ns, "dl_var")))),
      bslib::card_body(plotly::plotlyOutput(ns("variance"), height = "100%"), padding = c(8, 12)),
      bslib::card_footer(shiny::uiOutput(ns("excluded")), class = "card-foot-quiet")
    )
  )
}

mod_trend_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    plotted <- shiny::reactive({
      s <- state$series()
      utils::head(s, MAX_SERIES)
    })
    colour_slots <- shiny::reactiveVal(stats::setNames(integer(), character()))
    colours <- shiny::reactive({
      keys <- plotted()$series_key
      slots <- assign_colours(keys, shiny::isolate(colour_slots()))
      colour_slots(slots)
      stats::setNames(SERIES_COLOURS[slots], names(slots))
    })
    xrange <- shiny::reactive({
      r <- state$range()
      c(r[1] - 15, r[2] + 15)
    })
    names_for <- shiny::reactive({
      s <- plotted()
      stats::setNames(series_names(s, state$ds()$dims), s$series_key)
    })
    obs <- shiny::reactive({
      d <- state$ds()
      r <- state$range()
      o <- d$obs[d$obs$series_key %in% plotted()$series_key & d$obs$date >= r[1] & d$obs$date <= r[2], ]
      o$series <- names_for()[o$series_key]
      o[order(o$series_key, o$date), ]
    })
    vrows <- shiny::reactive(variance_rows(state$ds(), plotted(), state$vtype(), state$range()))

    output$note <- shiny::renderUI({
      n <- nrow(state$series())
      if (n == 0) return(empty_note("No series match these filters. Widen a filter in the sidebar."))
      if (n > MAX_SERIES) {
        return(empty_note(sprintf("%d series match; the charts show the first %d. The variance table lists all of them.",
                                  n, MAX_SERIES)))
      }
      NULL
    })

    output$level_title <- shiny::renderUI({
      shiny::span(class = "card-title", "Level",
                  shiny::span(class = "card-sub", unit_title(state$ds(), state$unit())))
    })

    # Headline figures for the first plotted series, as one ruled line.
    output$ledger <- shiny::renderUI({
      s <- plotted()
      shiny::req(nrow(s) > 0)
      d <- state$ds()
      h <- headline_figures(d, s$series_key[1])
      shiny::req(h)
      who <- series_full_names(s[1, ], d$series, d$dims)
      caption <- if (nrow(state$series()) > 1) paste0("Headline figures for the first series: ", who, ".")
                 else paste0("Headline figures for ", who, ".")
      cells <- c(
        list(ledger_cell("Latest month", fmt_num(h$latest$value), month_label(h$latest$date),
                         unit_title(d, s$unit[1]), lead = TRUE)),
        lapply(h$variances, function(v) {
          if (is.na(v$pct) && is.na(v$abs)) return(ledger_cell(v$type, "\u2013", "No valid period yet"))
          ledger_cell(v$type, fmt_pct(v$pct), paste(v$period, "against", v$comparison),
                      paste("Change", fmt_num(v$abs)), active = identical(v$type, state$vtype()))
        })
      )
      shiny::div(class = "ledger",
        shiny::div(class = "ledger-caption", caption),
        shiny::div(class = "ledger-row", cells))
    })
    output$var_title <- shiny::renderUI({
      d <- state$ds()
      def <- d$definitions$definition[d$definitions$variance_type == state$vtype()]
      shiny::span(class = "card-title", paste(state$vtype(), "change"), shiny::span(class = "card-sub", def))
    })

    output$level <- plotly::renderPlotly({
      o <- obs()
      shiny::validate(shiny::need(nrow(o) > 0, "No data for this selection."))
      keys <- unique(o$series_key)
      col <- colours()
      p <- plotly::plot_ly()
      for (i in seq_along(keys)) {
        x <- o[o$series_key == keys[i], ]
        p <- plotly::add_trace(
          p, x = x$date, y = x$value, type = "scatter", mode = "lines", name = x$series[1],
          line = list(color = col[[keys[i]]], width = 2),
          hovertemplate = paste0(x$series[1], ": %{y:,.2f}<extra></extra>")
        )
      }
      ann <- annotation_lines(state)
      plotly_base(p, unit_title(state$ds(), state$unit()), legend = length(keys) > 1) |>
        plotly::layout(xaxis = list(type = "date", hoverformat = "%b %Y", range = xrange()), shapes = ann$shapes,
                       annotations = ann$labels)
    })

    output$variance <- plotly::renderPlotly({
      v <- vrows()
      shiny::validate(shiny::need(nrow(v) > 0 && any(v$status == "ok"),
                                  paste("No valid", state$vtype(), "periods for this selection.")))
      keys <- unique(plotted()$series_key)
      col <- colours()
      pct <- identical(input$as, "pct")
      p <- plotly::plot_ly()
      for (i in seq_along(keys)) {
        x <- v[v$series_key == keys[i], ]
        if (!nrow(x)) next
        y <- if (pct) 100 * x$pct_change else x$abs_change
        y[x$status != "ok"] <- NA
        hover <- sprintf("%s<br>%s vs %s<br>%s vs %s<br>Change %s (%s)",
                         x$series, x$period, x$compared_with, fmt_num(x$value), fmt_num(x$comparison_value),
                         fmt_num(x$abs_change), fmt_pct(x$pct_change))
        p <- plotly::add_trace(
          p, x = x$date, y = y, type = "scatter", mode = "lines+markers", name = x$series[1],
          line = list(color = col[[keys[i]]], width = 2),
          marker = list(color = col[[keys[i]]], size = 5),
          text = hover, hoverinfo = "text", connectgaps = FALSE
        )
      }
      zero <- list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                   line = list(color = INK_2, width = 1))
      ytitle <- if (pct) "% change" else paste("Change,", unit_title(state$ds(), state$unit()))
      plotly_base(p, ytitle, legend = length(keys) > 1) |>
        plotly::layout(hovermode = "closest", shapes = list(zero),
                       xaxis = list(type = "date", hoverformat = "%b %Y", range = xrange()))
    })

    output$excluded <- shiny::renderUI({
      if (state$vtype() != "QoQ") return(NULL)
      d <- state$ds()
      o <- d$obs[d$obs$series_key %in% plotted()$series_key, ]
      q <- lubridate::floor_date(o$date, "quarter")
      n <- tapply(o$date, list(o$series_key, q), function(x) length(unique(x)))
      partial <- colnames(n)[apply(n, 2, function(col) any(!is.na(col) & col < 3))]
      if (!length(partial)) return(NULL)
      labs <- vapply(partial, function(x) {
        qd <- as.Date(x)
        sprintf("%s (%d of 3 months)", quarter_label(qd), max(n[, x], na.rm = TRUE))
      }, "")
      shiny::div(class = "chart-foot", paste("Excluded as incomplete:", paste(labs, collapse = ", ")))
    })

    download_handlers(output, state, function() {
      d <- state$ds()
      out <- dplyr::left_join(obs()[c("series_key", "date", "value", "flag")], plotted(), by = "series_key")
      out$series <- series_full_names(out, d$series, d$dims)
      out <- out[c("series", "series_key", "unit", d$dims, "date", "value", "flag")]
      names(out)[match(d$dims, names(out))] <- unname(d$dim_labels[d$dims])
      out
    }, function() paste0(state$ds()$id, "_levels"), "dl_level")
    download_handlers(output, state, function() {
      vrows() |> dplyr::select(series, series_key, variance_type, period, compared_with, value,
                               comparison_value, abs_change, pct_change, status)
    }, function() paste0(state$ds()$id, "_", tolower(state$vtype())), "dl_var")
  })
}

# Known events from config/annotations.csv as dashed markers on the level chart.
annotation_lines <- function(state) {
  a <- state$release()$annotations
  d <- state$ds()
  if (is.null(a) || !nrow(a)) return(list(shapes = list(), labels = list()))
  a <- a[a$dataset %in% c(d$id, "*"), ]
  keys <- state$series()$series_key
  a <- a[vapply(seq_len(nrow(a)), function(i) any(grepl(a$series_pattern[i] %||% ".", keys)), logical(1)), ]
  if (!nrow(a)) return(list(shapes = list(), labels = list()))
  dates <- as.Date(paste0(a$month, "-01"))
  list(
    shapes = lapply(dates, function(x) list(type = "line", xref = "x", yref = "paper", x0 = x, x1 = x,
                                            y0 = 0, y1 = 1, line = list(color = INK_2, width = 1, dash = "dot"))),
    labels = lapply(seq_along(dates), function(i) list(x = dates[i], y = 1, xref = "x", yref = "paper",
                                                       text = a$note[i], showarrow = FALSE, yanchor = "bottom",
                                                       font = list(color = INK_2, size = 11)))
  )
}
