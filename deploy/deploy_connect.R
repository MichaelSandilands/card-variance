# Deploy the Shiny app to Posit Connect.
#
# One-time setup on your machine:
#   rsconnect::addServer("https://connect.example.com", name = "connect")
#   rsconnect::connectApiUser(server = "connect", account = "<you>", apiKey = "<Connect API key>")
#
# Then set the R2 variables for a READ-ONLY R2 token in your session or .Renviron
# (see .Renviron.example) and run, from the project root:
#   Rscript deploy/deploy_connect.R
#
# envVars copies each variable's current value to the content's Vars panel on
# Connect; the values are stored encrypted and are not part of the bundle.

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
