# Assemble each dataset and publish the release to the board (R2 in production) --

assemble_dataset <- function(id, config, meta, snapshot, schema, profile, tidy,
                             variances, validation, revisions) {
  cfg <- config$datasets[[id]]
  res <- tryCatch(resource_meta(meta, cfg$resource_id), error = function(e) list())
  data <- tidy$data
  obs <- data[!is.na(data$date), c("series_key", "date", "value", "flag")]
  series <- series_table(data, profile$dims)
  dim_labels <- stats::setNames(
    vapply(profile$dims, dim_label, character(1), labels = config$labels %||% list()),
    profile$dims
  )
  list(
    id = id,
    info = list(
      id = id, title = cfg$title, measure_type = cfg$measure_type %||% "flow",
      county_dim = cfg$county_dim, caveats = unlist(cfg$caveats),
      file = cfg$file, resource = res,
      dataset_title = meta$title, dataset_description = meta$description,
      license = meta$license, contact = meta$contact, dataset_url = meta$url,
      sha256 = snapshot$sha256, raw_pin = snapshot$pin, raw_version = snapshot$version,
      retrieved_at = snapshot$retrieved_at, rows = nrow(data), n_series = nrow(series),
      first_month = min(obs$date), last_month = max(obs$date), decimals = tidy$decimals,
      unit_notes = tidy$unit_notes, thresholds = dataset_thresholds(config, id),
      constants = profile$constants, columns = profile$columns
    ),
    roles = profile$roles,
    dims = profile$dims,
    dim_labels = dim_labels,
    codes = tidy$codes,
    series = series,
    obs = obs,
    variances = variances,
    definitions = variance_definitions(cfg$measure_type %||% "flow"),
    validation = validation,
    revisions = revisions,
    schema = schema
  )
}

status_counts <- function(validation) {
  s <- as.character(validation$status)
  stats::setNames(lapply(status_levels, function(l) sum(s == l)), status_levels)
}

publish_release <- function(results, cross_checks, geo, population, meta, config,
                            board = card_board()) {
  results <- stats::setNames(results, vapply(results, function(r) r$id, ""))
  prev <- if (pin_exists_safe(board, pin_names$release)) pins::pin_read(board, pin_names$release) else NULL
  entries <- list()
  for (id in names(results)) {
    r <- results[[id]]
    name <- pin_names$dataset(id)
    counts <- status_counts(r$validation)
    exists <- pin_exists_safe(board, name)
    if (counts$stop > 0 && exists) {
      v <- prev$datasets[[id]]$version %||% pin_versions_desc(board, name)$version[1]
      entries[[id]] <- list(version = v, published = FALSE, counts = counts,
                            last_month = prev$datasets[[id]]$last_month %||% NA,
                            note = "A stop-level check failed, so the previous release was kept.")
      next
    }
    # Everything the app shows (caveats, titles and definitions too), but not the fetch time.
    hashed <- r
    hashed$info$retrieved_at <- NULL
    obj_hash <- digest::digest(hashed)
    old_hash <- if (exists) tryCatch(pins::pin_meta(board, name)$user$obj_hash, error = function(e) NULL)
    if (!identical(old_hash, obj_hash)) {
      pins::pin_write(
        board, r, name = name, type = "rds", title = r$info$title,
        description = paste("Card variance dataset", id),
        metadata = list(obj_hash = obj_hash, sha256 = r$info$sha256,
                        last_month = format(r$info$last_month), counts = counts),
        versioned = TRUE
      )
    }
    entries[[id]] <- list(
      version = pin_versions_desc(board, name)$version[1], published = TRUE, counts = counts,
      last_month = format(r$info$last_month),
      note = if (counts$stop > 0) "Published for review: a stop-level check failed and there is no earlier release." else ""
    )
  }

  data_to <- max(as.Date(vapply(entries, function(e) as.character(e$last_month), "")), na.rm = TRUE)
  release <- list(
    created = utc_now(),
    source = cardvar_source(),
    git_sha = Sys.getenv("GITHUB_SHA", NA_character_),
    data_to = data_to,
    datasets = entries,
    titles = vapply(results, function(r) r$info$title, ""),
    cross_checks = cross_checks,
    geo = geo,
    population = population,
    annotations = read_annotations(),
    thresholds = config$thresholds,
    labels = config$labels,
    meta = meta[c("title", "description", "license", "metadata_modified", "contact", "url")]
  )
  # A new release only when its content changed (the timestamp does not count).
  content_hash <- digest::digest(release[setdiff(names(release), "created")])
  prev_hash <- if (!is.null(prev)) {
    tryCatch(pins::pin_meta(board, pin_names$release)$user$content_hash, error = function(e) NULL)
  }
  same <- identical(prev_hash, content_hash)
  if (!same) {
    pins::pin_write(board, release, name = pin_names$release, type = "rds",
                    title = "Card variance release",
                    metadata = list(data_to = format(data_to), created = release$created,
                                    source = release$source, content_hash = content_hash),
                    versioned = TRUE)
  }
  release_version <- pin_versions_desc(board, pin_names$release)$version[1]

  all_checks <- dplyr::bind_rows(c(unname(lapply(results, `[[`, "validation")), list(cross_checks)))
  tally <- table(factor(as.character(all_checks$status), levels = status_levels))
  run <- tibble::tibble(
    checked_at = utc_now(), release_changed = !same, release_version = release_version,
    data_to = format(data_to), source = cardvar_source(),
    stop = tally[["stop"]], fail = tally[["fail"]], warn = tally[["warn"]], pass = tally[["pass"]]
  )
  log <- if (pin_exists_safe(board, pin_names$runlog)) pins::pin_read(board, pin_names$runlog) else NULL
  pins::pin_write(board, utils::head(dplyr::bind_rows(run, log), 500), name = pin_names$runlog,
                  type = "rds", title = "Card variance run log", versioned = FALSE)

  list(release_version = release_version, changed = !same, run = run,
       datasets = entries, summary_md = release_summary_md(run, entries, results, all_checks))
}

release_summary_md <- function(run, entries, results, checks) {
  lines <- c(
    "## Card payments variance refresh", "",
    sprintf("- Checked: %s", run$checked_at),
    sprintf("- Data to: %s", format(as.Date(run$data_to), "%B %Y")),
    sprintf("- New release written: %s", if (run$release_changed) "yes" else "no (nothing changed)"),
    sprintf("- Checks: %d stop, %d fail, %d warn, %d pass", run$stop, run$fail, run$warn, run$pass), "",
    "| Dataset | Published | Stop | Fail | Warn | Note |", "| --- | --- | --- | --- | --- | --- |"
  )
  for (id in names(entries)) {
    e <- entries[[id]]
    lines <- c(lines, sprintf("| %s | %s | %d | %d | %d | %s |", results[[id]]$info$title,
                              if (e$published) "yes" else "no", e$counts$stop, e$counts$fail,
                              e$counts$warn, e$note))
  }
  bad <- checks[as.character(checks$status) %in% c("stop", "fail"), ]
  if (nrow(bad)) {
    lines <- c(lines, "", "### Failing checks", "")
    lines <- c(lines, sprintf("- **%s** %s: %d item(s)", bad$dataset, bad$title, bad$n_failed))
  }
  paste(lines, collapse = "\n")
}
