# Deploy the Shiny app to Posit Connect Cloud.
#
# One-time setup on your machine:
#   rsconnect::connectCloudUser()    # opens a browser to log in and choose an account
#
# Then run, from the project root:
#   Rscript deploy/deploy_connect_cloud.R            # keeps the app's current visibility
#   Rscript deploy/deploy_connect_cloud.R public     # or private (needs a plan with private content)
#
# Connect Cloud does not take environment variables from rsconnect (envVars works on
# Posit Connect only), so nothing in .Renviron is sent. Set these once in the content's
# settings on Connect Cloud, with the two keys marked secret, then restart the app:
#   CARDVAR_BOARD         r2
#   R2_ACCOUNT_ID         your Cloudflare account ID
#   R2_BUCKET             card-variance
#   R2_ACCESS_KEY_ID      } from an R2 token with Object Read only;
#   R2_SECRET_ACCESS_KEY  } the app never writes
# Until they are set, the app shows "No release found".
#
# New releases need no redeploy: the app checks R2 every 15 minutes. Redeploy after
# changing the app's code.

server <- "connect.posit.cloud"
if (!nrow(rsconnect::accounts(server = server))) {
  stop("No Connect Cloud account is registered. Run rsconnect::connectCloudUser() first.",
       call. = FALSE)
}

visibility <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(visibility)) visibility <- NULL   # first deploy: private if the plan allows, else public

rsconnect::deployApp(
  appDir = "app",
  appName = "card-payments-variance",
  appTitle = "Card payments variance",
  server = server,
  appVisibility = visibility,
  forceUpdate = TRUE
)
