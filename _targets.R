# Card payments variance pipeline
#
# Run with targets::tar_make() or Rscript run_pipeline.R.
# Environment:
#   CARDVAR_SOURCE  live (default) | fixtures   where the CSVs come from
#   CARDVAR_BOARD   local (default) | r2        where releases are written

library(targets)
library(tarchetypes)

tar_source("R")
tar_source(file.path("app", "R", c("storage.R", "shared.R")))

tar_option_set(
  packages = c("dplyr", "tidyr", "tibble"),
  format = "rds",
  memory = "transient",
  garbage_collection = TRUE
)

datasets <- tibble::tibble(
  id = c("mpci", "mpcp", "mpcy", "mpmc"),
  resource_id = c(
    "09fb7335-f89f-4c47-89be-5e4156a61a37",
    "48aca6f9-bfd2-40fd-b746-e93c0ee32e41",
    "5503f6fb-b83b-472d-b755-694f4daf4a58",
    "6ecf9975-8bc9-4872-a62c-7916a7f8988f"
  )
)

per_dataset <- tar_map(
  values = datasets,
  names = id,
  # Fixed path data/incoming/<id>.csv, re-downloaded every run and hashed by targets.
  tar_target(incoming, download_resource(source_meta, resource_id, id, config),
             format = "file", cue = tar_cue(mode = "always")),
  # Runs only when the file changed: keeps the raw release on the board.
  tar_target(snapshot, keep_snapshot(incoming, id, resource_id, source_meta)),
  tar_target(raw, read_snapshot(incoming)),
  tar_target(schema, check_schema(raw, id)),
  tar_target(profile, profile_dataset(raw, config$datasets[[id]], config$labels)),
  tar_target(tidy, tidy_dataset(raw, profile, config$datasets[[id]])),
  tar_target(previous, load_previous_release(snapshot, config$datasets[[id]])),
  tar_target(variances, compute_variances(tidy$data, config$datasets[[id]]$measure_type)),
  tar_target(revisions, compare_releases(tidy, previous)),
  tar_target(validation, validate_dataset(id, raw, profile, tidy, variances, revisions, schema, config)),
  tar_target(result, assemble_dataset(id, config, source_meta, snapshot, schema, profile, tidy,
                                      variances, validation, revisions))
)

list(
  tar_target(config_file, file.path("config", "datasets.yml"), format = "file"),
  tar_target(annotations_file, file.path("config", "annotations.csv"), format = "file"),
  tar_target(config, {
    annotations_file
    read_config(config_file)
  }),
  tar_target(source_meta, get_source_meta(config), cue = tar_cue(mode = "always")),
  per_dataset,
  tar_target(geo, prepare_counties(config)),
  tar_target(population, read_population(config), cue = tar_cue(mode = "always")),
  tar_combine(results, per_dataset[["result"]], command = list(!!!.x)),
  tar_target(cross_checks, cross_file_checks(results, geo, config)),
  tar_target(release, publish_release(results, cross_checks, geo, population, source_meta, config),
             cue = tar_cue(mode = "always"))
)
