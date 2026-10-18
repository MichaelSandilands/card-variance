# Validation: every check, its result, and the rows behind it --------------------

mod_validation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("tiles")),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::div(class = "card-head",
        shiny::span(class = "card-title", "Checks"),
        shiny::div(class = "head-controls",
          shiny::radioButtons(ns("scope"), NULL, c("This dataset" = "dataset", "Across files" = "cross"),
                              inline = TRUE),
          shiny::checkboxInput(ns("hide_pass"), "Hide passed", TRUE),
          download_buttons(ns)))),
      reactable::reactableOutput(ns("checks")),
      shiny::div(class = "chart-foot",
        "Stop: the release is not published over the previous one. Fail: published, must be resolved before ",
        "submission. Review: published, explain before sign-off. Expand a row to see the items behind it.")
    )
  )
}

mod_validation_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    checks <- shiny::reactive({
      v <- if (identical(input$scope, "cross")) {
        x <- state$release()$cross_checks
        x[x$dataset %in% c("all", state$ds()$id), ]
      } else signoff_checks(state$ds(), state$release())
      v$status <- as.character(v$status)
      v[order(match(v$status, status_levels), v$category), ]
    })
    shown <- shiny::reactive({
      v <- checks()
      if (isTRUE(input$hide_pass)) v <- v[v$status != "pass", ]
      v
    })

    output$tiles <- shiny::renderUI({
      v <- checks()
      n <- function(s) sum(v$status == s)
      shiny::div(class = "ledger",
        shiny::div(class = "ledger-row ledger-status",
          ledger_cell("Blocked", n("stop"), "Not published over the previous release"),
          ledger_cell("Failed", n("fail"), "Resolve before submission"),
          ledger_cell("To review", n("warn"), "Explain before sign-off"),
          ledger_cell("Passed", n("pass"), paste("of", nrow(v), "checks"))))
    })

    output$checks <- reactable::renderReactable({
      v <- shown()
      shiny::validate(shiny::need(nrow(v) > 0, "Every check passed."))
      t <- v[, c("status", "category", "title", "n_tested", "n_failed", "rule_source", "note")]
      details <- v$details
      reactable::reactable(
        t, compact = TRUE, highlight = TRUE, pagination = FALSE, onClick = "expand",
        rowStyle = list(cursor = "pointer"),
        columns = list(
          status = reactable::colDef(name = "Result", width = 96, cell = function(x) status_chip(x)),
          category = reactable::colDef(name = "Category", width = 120),
          title = reactable::colDef(name = "Check", minWidth = 260),
          n_tested = reactable::colDef(name = "Tested", width = 84, align = "right",
                                       format = reactable::colFormat(separators = TRUE)),
          n_failed = reactable::colDef(name = "Items", width = 76, align = "right"),
          rule_source = reactable::colDef(name = "Rule", width = 150),
          note = reactable::colDef(name = "Note", minWidth = 260)
        ),
        details = function(i) {
          d <- details[[i]]
          if (is.null(d) || !nrow(d)) return(NULL)
          d <- as.data.frame(d)
          d <- d[, setdiff(names(d), c("n_total", "largest_part_name")), drop = FALSE]
          for (k in names(d)) if (inherits(d[[k]], "Date")) d[[k]] <- format(d[[k]], "%b %Y")
          labs <- state$ds()$dim_labels
          nice <- c(series_key = "Series key", date = "Month", missing_month = "Missing month",
                    unit = "Unit", total = "Total", sum_of_parts = "Sum of parts", n_parts = "Parts",
                    largest_part = "Largest part", difference = "Difference", tolerance = "Tolerance",
                    value = "Value", comparison_value = "Comparison value", pct_change = "% change",
                    variance_type = "Variance", explanation = "Explained by", remainder = "STL remainder",
                    lower = "Lower fence", upper = "Upper fence", old = "Previous", new = "Now",
                    change = "Change", flag = "Flag", rows = "Rows", last = "Last month",
                    atv = "Avg transaction value", atv_previous = "Previous ATV",
                    material = "Material series")
          names(d) <- ifelse(names(d) %in% names(labs), unname(labs[names(d)]),
                             ifelse(names(d) %in% names(nice), unname(nice[names(d)]), names(d)))
          shiny::div(class = "detail-table",
            reactable::reactable(d, compact = TRUE, defaultPageSize = 10, bordered = TRUE,
                                 defaultColDef = reactable::colDef(
                                   format = reactable::colFormat(separators = TRUE, digits = 2))))
        }
      )
    })

    download_handlers(output, state, function() {
      v <- checks()
      summary <- v[, c("dataset", "check", "title", "category", "severity", "status", "n_tested",
                       "n_failed", "rule_source", "note")]
      items <- dplyr::bind_rows(lapply(seq_len(nrow(v)), function(i) {
        d <- v$details[[i]]
        if (is.null(d) || !nrow(d)) return(NULL)
        d <- as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE)
        cbind(check = v$check[i], d)
      }))
      list(checks = summary, failing_items = if (is.null(items)) tibble::tibble() else items)
    }, function() paste0(state$ds()$id, "_validation"))
  })
}
