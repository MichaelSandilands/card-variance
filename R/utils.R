# Small shared helpers ---------------------------------------------------------


read_config <- function(path = "config/datasets.yml") {
  cfg <- yaml::read_yaml(path)
  cfg$rules <- cfg$rules %||% list()
  cfg$cross_checks <- cfg$cross_checks %||% list()
  cfg
}

# "fixtures" reads the synthetic files in tests/fixtures; "live" calls the Central Bank.
cardvar_source <- function() Sys.getenv("CARDVAR_SOURCE", "live")

fixture_dir <- function() {
  Sys.getenv("CARDVAR_FIXTURE_DIR", file.path("tests", "fixtures", "release_2026-08"))
}





# Number of decimal places used in the source values (drives rounding tolerance).
source_decimals <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(0L)
  parts <- regmatches(x, regexpr("\\.[0-9]+$", x))
  if (!length(parts)) return(0L)
  min(max(nchar(parts) - 1L), 6L)
}


