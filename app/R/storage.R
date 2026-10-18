# Where releases live. Shared by the pipeline (_targets.R sources this file)
# and the Shiny app.
#
# CARDVAR_BOARD=r2     Cloudflare R2 (production). Needs R2_ACCOUNT_ID,
#                      R2_ACCESS_KEY_ID, R2_SECRET_ACCESS_KEY and R2_BUCKET;
#                      R2_PREFIX is optional (default "card-variance/").
# CARDVAR_BOARD=local  A folder of pins (development). CARDVAR_LOCAL_BOARD
#                      sets the folder; default "pins-local" at the project root.

card_board <- function(mode = Sys.getenv("CARDVAR_BOARD", "local")) {
  if (identical(mode, "r2")) return(r2_board())
  pins::board_folder(local_board_path(), versioned = TRUE)
}

local_board_path <- function() {
  path <- Sys.getenv("CARDVAR_LOCAL_BOARD", "")
  if (nzchar(path)) return(path)
  if (dir.exists("pins-local") || !dir.exists("../pins-local")) "pins-local" else "../pins-local"
}

# A blank R2_PREFIX= line would otherwise put every pin at the bucket root.
r2_prefix <- function() {
  prefix <- Sys.getenv("R2_PREFIX")
  if (nzchar(prefix)) prefix else "card-variance/"
}

# Which board the pipeline writes to. A target depends on it, so switching board
# (local to R2, say) re-uploads the raw files instead of skipping them as unchanged.
board_identity <- function(mode = Sys.getenv("CARDVAR_BOARD", "local")) {
  if (identical(mode, "r2")) {
    return(paste("r2", Sys.getenv("R2_ACCOUNT_ID"), Sys.getenv("R2_BUCKET"), r2_prefix()))
  }
  paste("local", normalizePath(local_board_path(), mustWork = FALSE))
}

r2_board <- function() {
  needed <- c("R2_ACCOUNT_ID", "R2_ACCESS_KEY_ID", "R2_SECRET_ACCESS_KEY", "R2_BUCKET")
  missing <- needed[!nzchar(Sys.getenv(needed))]
  if (length(missing)) {
    stop("Cloudflare R2 is selected but these environment variables are empty: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!requireNamespace("paws.storage", quietly = TRUE)) {
    stop("Package 'paws.storage' is needed to read or write Cloudflare R2.", call. = FALSE)
  }
  endpoint <- Sys.getenv("R2_ENDPOINT", "")
  if (!nzchar(endpoint)) {
    endpoint <- paste0("https://", Sys.getenv("R2_ACCOUNT_ID"), ".r2.cloudflarestorage.com")
  }
  pins::board_s3(
    bucket = Sys.getenv("R2_BUCKET"),
    prefix = r2_prefix(),
    region = "auto",
    endpoint = endpoint,
    access_key = Sys.getenv("R2_ACCESS_KEY_ID"),
    secret_access_key = Sys.getenv("R2_SECRET_ACCESS_KEY"),
    versioned = TRUE
  )
}

pin_names <- list(
  release = "cardvar_release",
  runlog = "cardvar_runlog",
  dataset = function(id) paste0("cardvar_", id),
  raw = function(id) paste0("cardvar_raw_", id)
)

pin_exists_safe <- function(board, name) {
  tryCatch(pins::pin_exists(board, name), error = function(e) FALSE)
}

# Versions newest first.
pin_versions_desc <- function(board, name) {
  if (!pin_exists_safe(board, name)) return(NULL)
  v <- pins::pin_versions(board, name)
  v[order(v$created, decreasing = TRUE), , drop = FALSE]
}
