# About this dataset: source, lineage, caveats, definitions, fields ----------------

mod_about_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header(shiny::span(class = "card-title", "Source")), shiny::uiOutput(ns("source"))),
      bslib::card(bslib::card_header(shiny::span(class = "card-title", "This release")), shiny::uiOutput(ns("lineage")))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header(shiny::span(class = "card-title", "Caveats")), shiny::uiOutput(ns("caveats"))),
      bslib::card(bslib::card_header(shiny::span(class = "card-title", "Variance definitions")),
                  shiny::tableOutput(ns("definitions")))
    ),
    bslib::card(
      bslib::card_header(shiny::span(class = "card-title", "Fields kept and hidden"),
                         shiny::span(class = "card-sub", "Codes, constants and repeats are hidden from the filters; this is why.")),
      reactable::reactableOutput(ns("fields"))
    )
  )
}

mod_about_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    kv <- function(...) {
      x <- list(...)
      shiny::tags$dl(class = "kv", lapply(names(x), function(k) {
        if (is.null(x[[k]]) || identical(x[[k]], "") || (length(x[[k]]) == 1 && is.na(x[[k]]))) return(NULL)
        shiny::tagList(shiny::tags$dt(k), shiny::tags$dd(x[[k]]))
      }))
    }

    output$source <- shiny::renderUI({
      i <- state$ds()$info
      res <- i$resource
      shiny::tagList(
        shiny::h5(i$title),
        shiny::p(class = "muted", res$description %||% ""),
        kv(
          "Publisher dataset" = i$dataset_title,
          "Description" = i$dataset_description,
          "File" = res$name %||% i$file,
          "Download" = if (!is.null(res$url)) shiny::tags$a(href = res$url, target = "_blank", res$url),
          "Dataset page" = shiny::tags$a(href = i$dataset_url, target = "_blank", i$dataset_url),
          "Licence" = i$license,
          "Contact" = i$contact,
          "Source last modified" = res$last_modified
        )
      )
    })

    output$lineage <- shiny::renderUI({
      i <- state$ds()$info
      rel <- state$release()
      e <- rel$datasets[[i$id]]
      kv(
        "Data covers" = paste(month_label(i$first_month), "to", month_label(i$last_month)),
        "Measure type" = if (identical(i$measure_type, "stock")) "Stock (period-end value)" else "Flow (summed over periods)",
        "Rows" = format(i$rows, big.mark = ","),
        "Series" = format(i$n_series, big.mark = ","),
        "Source decimals" = i$decimals,
        "Raw file SHA-256" = shiny::tags$code(i$sha256),
        "Raw file version" = i$raw_version,
        "Retrieved" = i$retrieved_at,
        "Release created" = rel$created,
        "Published in this release" = if (isTRUE(e$published)) "Yes" else paste("No.", e$note),
        "Pipeline commit" = rel$git_sha,
        "Data source" = if (identical(rel$source, "fixtures")) "Synthetic fixtures (not real statistics)" else "Central Bank of Ireland open data"
      )
    })

    output$caveats <- shiny::renderUI({
      i <- state$ds()$info
      const <- i$constants
      shiny::tagList(
        if (length(i$caveats)) shiny::tags$ul(lapply(i$caveats, shiny::tags$li)),
        if (length(const)) shiny::tagList(
          shiny::h6("Same on every row"),
          shiny::tags$ul(lapply(names(const), function(k)
            shiny::tags$li(shiny::strong(paste0(dim_label(k, state$release()$labels %||% list()), ":")), " ", const[[k]]))))
      )
    })

    output$definitions <- shiny::renderTable({
      d <- state$ds()$definitions
      stats::setNames(d, c("Variance", "Definition"))
    }, striped = FALSE, bordered = FALSE, spacing = "s", width = "100%")

    output$fields <- reactable::renderReactable({
      r <- state$ds()$roles
      r$kept <- ifelse(r$kept, "Kept", "Hidden")
      reactable::reactable(
        r[, c("column", "role", "kept", "reason", "n_distinct", "examples")], compact = TRUE,
        pagination = FALSE, highlight = TRUE,
        columns = list(
          column = reactable::colDef(name = "Column", style = list(fontFamily = "ui-monospace, monospace")),
          role = reactable::colDef(name = "Role", width = 100),
          kept = reactable::colDef(name = "In app", width = 90),
          reason = reactable::colDef(name = "Why", minWidth = 240),
          n_distinct = reactable::colDef(name = "Distinct values", width = 120, align = "right"),
          examples = reactable::colDef(name = "Examples", minWidth = 200)
        )
      )
    })
  })
}
