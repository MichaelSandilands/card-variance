# Revisions: what changed since the previous release ---------------------------------

mod_revisions_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("summary")),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::div(class = "card-head",
        shiny::span(class = "card-title", "Changed values"),
        shiny::div(class = "head-controls",
          shiny::checkboxInput(ns("selected_only"), "Selected series only", FALSE),
          download_buttons(ns)))),
      reactable::reactableOutput(ns("table"))
    )
  )
}

mod_revisions_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    rev <- shiny::reactive(state$ds()$revisions)
    rows <- shiny::reactive({
      r <- rev()
      x <- r$changes
      if (is.null(x) || !nrow(x)) return(x)
      d <- state$ds()
      if (isTRUE(input$selected_only)) x <- x[x$series_key %in% state$series()$series_key, ]
      s <- d$series[match(x$series_key, d$series$series_key), ]
      x$series <- series_full_names(s, d$series, d$dims)
      x$unit <- s$unit
      x
    })

    output$summary <- shiny::renderUI({
      r <- rev()
      if (!isTRUE(r$available)) {
        return(empty_note("No previous release of this file is on the board yet, so there is nothing to compare."))
      }
      s <- r$summary
      shiny::div(class = "ledger",
        shiny::div(class = "ledger-caption",
          sprintf("Compared with the previous raw release (%s), which had data to %s. This release runs to %s.",
                  r$previous_version, format(r$previous_latest, "%B %Y"), format(r$current_latest, "%B %Y"))),
        shiny::div(class = "ledger-row",
          ledger_cell("New months", s$months_added, "Added at the end of the series"),
          ledger_cell("Revised", format(s$revised, big.mark = ","), "Earlier values that changed"),
          ledger_cell("Removed", s$removed, "Published values no longer in the file"),
          ledger_cell("New history", s$new_history, "Earlier months that appeared")))
    })

    output$table <- reactable::renderReactable({
      x <- rows()
      shiny::validate(shiny::need(!is.null(x) && nrow(x) > 0, "No earlier values changed."))
      t <- tibble::tibble(series = x$series, series_key = x$series_key, unit = x$unit,
                          month = month_label(x$date), change = tools::toTitleCase(x$change),
                          old = x$old, new = x$new, difference = x$difference, pct_change = x$pct_change)
      num <- reactable::colFormat(separators = TRUE, digits = 2)
      reactable::reactable(
        t, compact = TRUE, searchable = TRUE, highlight = TRUE, defaultPageSize = 25,
        columns = list(
          series = reactable::colDef(name = "Series", minWidth = 180),
          series_key = reactable::colDef(name = "Series key", minWidth = 200,
                                         style = list(fontFamily = "ui-monospace, monospace", fontSize = "0.8em")),
          unit = reactable::colDef(name = "Unit"),
          month = reactable::colDef(name = "Month"),
          change = reactable::colDef(name = "Change"),
          old = reactable::colDef(name = "Previous", format = num, align = "right"),
          new = reactable::colDef(name = "Now", format = num, align = "right"),
          difference = reactable::colDef(name = "Difference", format = num, align = "right"),
          pct_change = reactable::colDef(name = "% change", align = "right", minWidth = 90,
                                         cell = function(v) fmt_pct(v, 2))
        )
      )
    })

    download_handlers(output, state, function() {
      x <- rows()
      if (is.null(x)) return(tibble::tibble())
      x |> dplyr::select(series, series_key, unit, date, change, old, new, difference, pct_change)
    }, function() paste0(state$ds()$id, "_revisions"))
  })
}
