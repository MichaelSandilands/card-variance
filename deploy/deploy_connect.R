# Deploy the Shiny app to Posit Connect.
#
# One-time setup on your machine:
#   rsconnect::addServer("https://connect.example.com", name = "connect")
#   rsconnect::connectApiUser(server = "connect", account = "<you>", apiKey = "<Connect API key>")
#
# Then, with the R2 variables in .Renviron (see .Renviron.example, including the optional
# read-only pair for the app), run from the project root:
#   Rscript deploy/deploy_connect.R
#
# envVars copies each variable's current value to the content's Vars panel on
# Connect; the values are stored encrypted and are not part of the bundle.

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

rsconnect::deployApp(
  appDir = "app",
  appName = "card-payments-variance",
  appTitle = "Card payments variance",
  envVars = c("CARDVAR_BOARD", needed, "R2_PREFIX"),
  forceUpdate = TRUE
)
