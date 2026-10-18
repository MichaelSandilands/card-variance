# Deploy the Shiny app to Posit Connect Cloud.
#
# One-time setup on your machine:
#   rsconnect::connectCloudUser()    # opens a browser to log in and choose an account
#
# Then, with the R2 variables in .Renviron, run from the project root:
#   Rscript deploy/deploy_connect_cloud.R            # keeps the app's current visibility
#   Rscript deploy/deploy_connect_cloud.R public     # or private (needs a plan with private content)
#
# Every deploy replaces the app's variables on Connect Cloud with the values sent here
# (stored there as secrets), so set them in .Renviron, not on the Connect Cloud page:
# anything added there is removed by the next deploy. rsconnect's help says envVars
# works on Posit Connect only; rsconnect 1.11 sends them to Connect Cloud too.
#
# New releases need no redeploy: the app checks R2 every 15 minutes. Redeploy after
# changing the app's code.

server <- "connect.posit.cloud"
if (!nrow(rsconnect::accounts(server = server))) {
  stop("No Connect Cloud account is registered. Run rsconnect::connectCloudUser() first.",
       call. = FALSE)
}

# .Renviron holds the pipeline's read-write token; the app only reads, so it gets the
# read-only pair when R2_READ_ACCESS_KEY_ID and R2_READ_SECRET_ACCESS_KEY are set.
if (nzchar(Sys.getenv("R2_READ_ACCESS_KEY_ID"))) {
  Sys.setenv(R2_ACCESS_KEY_ID = Sys.getenv("R2_READ_ACCESS_KEY_ID"),
             R2_SECRET_ACCESS_KEY = Sys.getenv("R2_READ_SECRET_ACCESS_KEY"))
} else {
  message("Deploying with the token in R2_ACCESS_KEY_ID. Set R2_READ_ACCESS_KEY_ID and ",
          "R2_READ_SECRET_ACCESS_KEY to give the app a read-only token instead.")
}

needed <- c("R2_ACCOUNT_ID", "R2_ACCESS_KEY_ID", "R2_SECRET_ACCESS_KEY", "R2_BUCKET")
missing <- needed[!nzchar(Sys.getenv(needed))]
if (length(missing)) stop("Set these before deploying: ", paste(missing, collapse = ", "))

Sys.setenv(CARDVAR_BOARD = "r2")
if (!nzchar(Sys.getenv("R2_PREFIX"))) Sys.setenv(R2_PREFIX = "card-variance/")

visibility <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(visibility)) visibility <- NULL   # first deploy: private if the plan allows, else public

rsconnect::deployApp(
  appDir = "app",
  appName = "card-payments-variance",
  appTitle = "Card payments variance",
  server = server,
  envVars = c("CARDVAR_BOARD", needed, "R2_PREFIX"),
  appVisibility = visibility,
  forceUpdate = TRUE
)
