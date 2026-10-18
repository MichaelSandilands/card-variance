# Card payments variance - Shiny app
#
# Reads published releases from the board (Cloudflare R2 in production; see
# R/storage.R) and never recalculates anything, so every number on screen is
# the one the pipeline validated. Files in R/ are loaded automatically.

library(shiny)
library(bslib)

# pins contacts R2 as soon as the board is made, so bad credentials fail here; the
# server shows the reason instead of the app failing to start.
board <- tryCatch(card_board(), error = function(e) e)

# IBM Plex is linked from Google Fonts in the viewer's browser (nothing downloads on the
# server); where that is blocked, the system fonts below take over.
app_theme <- bs_theme(
  version = 5,
  bg = "#f5f7f8", fg = "#17212b",
  primary = "#1c4f8a", secondary = "#5b6670",
  success = "#0ca30c", warning = "#fab219", danger = "#d03b3b", info = "#1c4f8a",
  base_font = font_collection(font_google("IBM Plex Sans", local = FALSE, wght = c(400, 500, 600)),
                              "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
  code_font = font_collection(font_google("IBM Plex Mono", local = FALSE, wght = c(400, 500)),
                              "ui-monospace", "SFMono-Regular", "Menlo", "Consolas", "monospace"),
  "font-size-base" = "0.9rem",
  "border-radius" = "0.375rem",
  "border-color" = "#dde2e6",
  "card-cap-bg" = "#ffffff",
  "card-bg" = "#ffffff",
  "accordion-bg" = "transparent",
  "navbar-light-brand-color" = "#17212b"
)

ui <- page_navbar(
  id = "tabs",
  title = tags$span(class = "brand", "Card payments variance"),
  window_title = "Card payments variance",
  theme = app_theme,
  fillable = FALSE,
  navbar_options = navbar_options(bg = "#ffffff"),
  sidebar = sidebar(width = 300, mod_controls_ui("controls")),
  header = tagList(
    tags$head(tags$link(rel = "stylesheet", href = "app.css"),
              tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg")),
    uiOutput("release_header")
  ),
  nav_panel("About", value = "about", mod_about_ui("about")),
  nav_panel("Trend", value = "trend", mod_trend_ui("trend")),
  nav_panel("Variance table", value = "variance", mod_variance_ui("variance")),
  nav_panel("Diagnostics", value = "diagnostics", mod_diagnostics_ui("diagnostics")),
  nav_panel("Map", value = "map", mod_map_ui("map")),
  nav_panel("Validation", value = "validation", mod_validation_ui("validation")),
  nav_panel("Revisions", value = "revisions", mod_revisions_ui("revisions")),
  nav_spacer(),
  nav_item(tags$a(href = "https://www.centralbank.ie/statistics/data-and-analysis/monthly-card-payment-statistics",
                  target = "_blank", rel = "noopener", class = "nav-link", "Source statistics")),
  selected = "trend"
)

server <- function(input, output, session) {
  if (inherits(board, "error") || is.null(list_releases(board))) {
    problem <- if (inherits(board, "error")) paste("Connecting to the board failed:", conditionMessage(board))
               else board_error(board)
    showModal(modalDialog(
      title = "No release found",
      p(board_description()),
      p(problem %||%
          "The board has no published release yet. Publish one with CARDVAR_BOARD=r2 Rscript run_pipeline.R and reload."),
      footer = NULL, easyClose = FALSE
    ))
    return(invisible())
  }

  state <- mod_controls_server("controls", board)

  # The release header: what this is, how current it is, and the result in words.
  output$release_header <- renderUI({
    rel <- state$release()
    ds <- state$ds()
    log <- read_runlog(board)
    status <- as.character(signoff_checks(ds, rel)$status)
    checked <- if (!is.null(log) && nrow(log)) log$checked_at[1] else rel$created
    when <- function(x) format(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "%d %b %Y, %H:%M UTC")
    worst <- intersect(c("stop", "fail", "warn"), status)[1]
    div(
      class = "release-header",
      if (identical(rel$source, "fixtures")) {
        div(class = "fixture-banner", "Synthetic test data, not real statistics.")
      },
      div(class = "release-main",
        div(
          h1(class = "release-title", paste0(ds$info$title, ", ", format(ds$info$last_month, "%B %Y"))),
          p(class = "release-meta",
            sprintf("Monthly data from %s. Published %s. Source last checked %s.",
                    format(ds$info$first_month, "%B %Y"), when(rel$created), when(checked)))
        ),
        div(class = paste("release-verdict", paste0("verdict-", if (is.na(worst)) "pass" else worst)),
          span(class = "verdict-text", check_sentence(status)),
          tags$button(class = "btn btn-link verdict-link", type = "button",
                      onclick = "document.querySelector(\"a[data-value='validation']\").click()",
                      "Open the checks"))
      )
    )
  })

  observe({
    cd <- state$ds()$info$county_dim
    if (!is.null(cd) && cd %in% state$ds()$dims) nav_show("tabs", "map") else nav_hide("tabs", "map")
  })

  mod_about_server("about", state)
  mod_trend_server("trend", state)
  mod_variance_server("variance", state)
  mod_diagnostics_server("diagnostics", state)
  mod_map_server("map", state)
  mod_validation_server("validation", state)
  mod_revisions_server("revisions", state)
}

shinyApp(ui, server)
