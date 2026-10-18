# Source metadata, download, snapshot ------------------------------------------

# CKAN package_show: dataset description, licence and per-resource metadata.
# Kept free of timestamps so the target only changes when the Bank changes something.
get_source_meta <- function(config) {
  if (cardvar_source() == "fixtures") return(fixture_source_meta(config))
  src <- config$source
  resp <- httr2::request(src$ckan_base) |>
    httr2::req_url_path_append("api", "3", "action", "package_show") |>
    httr2::req_url_query(id = src$package_id) |>
    httr2::req_user_agent(src$user_agent) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  body <- httr2::resp_body_json(resp)
  if (!isTRUE(body$success)) stop("CKAN package_show did not succeed for ", src$package_id)
  r <- body$result
  list(
    title = r$title %||% "",
    description = r$notes %||% "",
    license = r$license_title %||% "",
    metadata_modified = r$metadata_modified %||% NA_character_,
    contact = r$maintainer_email %||% r$author_email %||% "",
    url = paste0(src$ckan_base, "/dataset/", r$name %||% src$package_id),
    resources = lapply(r$resources, function(x) list(
      id = x$id, name = x$name %||% "", description = x$description %||% "",
      url = x$url, format = x$format %||% "",
      last_modified = x$last_modified %||% x$metadata_modified %||% NA_character_,
      size = x$size %||% NA
    ))
  )
}

fixture_source_meta <- function(config) {
  list(
    title = "Monthly Card Payment Statistics (synthetic fixture data)",
    description = paste(
      "Synthetic data shaped like the Central Bank of Ireland files, used for",
      "development and tests. Not real statistics."
    ),
    license = "Test data",
    metadata_modified = "fixture",
    contact = "",
    url = "https://opendata.centralbank.ie/dataset/monthly-card-payment-statistics",
    resources = lapply(names(config$datasets), function(id) {
      d <- config$datasets[[id]]
      list(id = d$resource_id, name = d$title, description = "Synthetic fixture",
           url = file.path(fixture_dir(), paste0(id, ".csv")), format = "CSV",
           last_modified = "fixture", size = NA)
    })
  )
}

resource_meta <- function(meta, resource_id) {
  hit <- Filter(function(x) identical(x$id, resource_id), meta$resources)
  if (!length(hit)) stop("Resource ", resource_id, " is not listed in the CKAN package.")
  hit[[1]]
}

# Download one resource to data/incoming/<id>.csv. targets hashes the file, so
# everything downstream is skipped when the content has not changed.
download_resource <- function(meta, resource_id, id, config, dest_dir = file.path("data", "incoming")) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, paste0(id, ".csv"))
  if (cardvar_source() == "fixtures") {
    ok <- file.copy(file.path(fixture_dir(), paste0(id, ".csv")), dest, overwrite = TRUE)
    if (!ok) stop("Fixture file missing for ", id)
    return(dest)
  }
  url <- resource_meta(meta, resource_id)$url
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_user_agent(config$source$user_agent) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = tmp)
  if (file.size(tmp) == 0) stop("Downloaded file for ", id, " is empty: ", url)
  file.copy(tmp, dest, overwrite = TRUE)
  dest
}

file_sha256 <- function(path) digest::digest(file = path, algo = "sha256")

# Keep every distinct release of the raw file on the board (R2 in production).
# pins skips the upload when the hash matches the latest version.
keep_snapshot <- function(path, id, resource_id, meta, board = card_board()) {
  name <- pin_names$raw(id)
  sha <- file_sha256(path)
  versions <- pin_versions_desc(board, name)
  latest_sha <- NULL
  if (!is.null(versions) && nrow(versions)) {
    latest_sha <- tryCatch(pins::pin_meta(board, name)$user$sha256, error = function(e) NULL)
  }
  uploaded <- FALSE
  if (!identical(latest_sha, sha)) {
    res <- resource_meta(meta, resource_id)
    pins::pin_upload(
      board, paths = path, name = name,
      title = paste("Raw", id, "release"),
      description = paste("Raw CSV as downloaded from", res$url),
      metadata = list(sha256 = sha, retrieved_at = utc_now(), source_url = res$url,
                      source_last_modified = res$last_modified)
    )
    uploaded <- TRUE
    versions <- pin_versions_desc(board, name)
  }
  list(
    id = id, pin = name, sha256 = sha, uploaded = uploaded,
    version = versions$version[1],
    previous_version = if (nrow(versions) > 1) versions$version[2] else NA_character_,
    retrieved_at = utc_now(),
    rows = nrow(read_snapshot(path))
  )
}

read_snapshot <- function(path) {
  out <- readr::read_csv(
    path, col_types = readr::cols(.default = readr::col_character()),
    na = character(), trim_ws = TRUE, progress = FALSE, show_col_types = FALSE
  )
  names(out) <- toupper(trimws(names(out)))
  out
}

# The previous distinct release of the raw file, tidied the same way, for revisions.
load_previous_release <- function(snapshot, config_ds, board = card_board()) {
  if (is.na(snapshot$previous_version)) return(NULL)
  path <- pins::pin_download(board, snapshot$pin, version = snapshot$previous_version)
  raw <- read_snapshot(path[1])
  prof <- profile_dataset(raw, config_ds)
  tidy <- tidy_dataset(raw, prof, config_ds)
  list(version = snapshot$previous_version, data = tidy$data)
}
