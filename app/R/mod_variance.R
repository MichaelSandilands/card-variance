# Variance table: every selected series and period, both source values shown ----

mod_variance_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(shiny::div(class = "card-head",
      shiny::uiOutput(ns("title"), inline = TRUE),
      shiny::div(class = "head-controls",
        shiny::checkboxInput(ns("attention"), "Only rows needing attention", FALSE),
        shiny::checkboxInput(ns("latest"), "Latest period only", FALSE),
        download_buttons(ns)))),
    reactable::reactableOutput(ns("table")),
    shiny::div(class = "chart-foot",
      "Attention: no valid comparison, a zero base, or a move beyond the review thresholds (MoM or YoY).")
  )
}

mod_variance_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    rows <- shiny::reactive({
      v <- variance_rows(state$ds(), state$series(), state$vtype(), state$range())
      if (!nrow(v)) return(v)
      th <- ds_thresholds(state$ds(), state$release())
      limit <- switch(state$vtype(), MoM = th$mom_pct %||% 0.2, YoY = th$yoy_pct %||% 0.3, Inf)
      v$attention <- v$status != "ok" | (!is.na(v$pct_change) & abs(v$pct_change) > limit)
      if (isTRUE(input$latest)) v <- v[v$date == max(v$date), ]
      if (isTRUE(input$attention)) v <- v[v$attention, ]
      v[order(v$date, decreasing = TRUE), ]
    })

    output$title <- shiny::renderUI({
      shiny::span(class = "card-title", paste(state$vtype(), "variances"),
                  shiny::span(class = "card-sub", paste0(format(nrow(rows()), big.mark = ","), " rows, ",
                                                       unit_title(state$ds(), state$unit()))))
    })

    output$table <- reactable::renderReactable({
      v <- rows()
      shiny::validate(shiny::need(nrow(v) > 0, "No rows for this selection."))
      t <- v[, c("series", "series_key", "period", "compared_with", "value", "comparison_value",
                 "abs_change", "pct_change", "status", "attention")]
      num <- reactable::colFormat(separators = TRUE, digits = state$digits())
      reactable::reactable(
        t, searchable = TRUE, striped = FALSE, highlight = TRUE, compact = TRUE,
        defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 100),
        columns = list(
          series = reactable::colDef(name = "Series", minWidth = 220),
          series_key = reactable::colDef(name = "Series key", minWidth = 160,
                                         style = list(fontFamily = "ui-monospace, monospace", fontSize = "0.8em")),
          period = reactable::colDef(name = "Period", minWidth = 110),
          compared_with = reactable::colDef(name = "Compared with", minWidth = 110),
          value = reactable::colDef(name = "Value", format = num, align = "right"),
          comparison_value = reactable::colDef(name = "Comparison value", format = num, align = "right"),
          abs_change = reactable::colDef(name = "Change", format = num, align = "right"),
          pct_change = reactable::colDef(name = "% change", align = "right",
                                         cell = function(x) fmt_pct(x)),
          status = reactable::colDef(name = "Status", minWidth = 120, cell = function(x, i) {
            if (x == "ok" && !t$attention[i]) return("OK")
            if (x == "ok") return(shiny::span(class = "status-chip status-warn", "Large move"))
            shiny::span(class = "status-chip status-info", tools::toTitleCase(x))
          }),
          attention = reactable::colDef(show = FALSE)
        )
      )
    })

    download_handlers(output, state, function() {
      rows() |> dplyr::select(series, series_key, variance_type, period, compared_with, value,
                              comparison_value, abs_change, pct_change, status, attention)
    }, function() paste0(state$ds()$id, "_", tolower(state$vtype()), "_table"))
  })
}
