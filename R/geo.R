# County boundaries and population -------------------------------------------------

detect_county_col <- function(g) {
  cand <- names(g)[vapply(sf::st_drop_geometry(g), is.character, logical(1))]
  if (!length(cand)) stop("Boundary file has no text columns to match county names.")
  score <- vapply(cand, function(cl) sum(unique(normalise_county(g[[cl]])) %in% COUNTIES_26), numeric(1))
  if (max(score) < 20) stop("Could not find a county name column in the boundary file.")
  cand[which.max(score)]
}

# 26 counties, dissolved from whatever units the source uses, in WGS84 for leaflet.
prepare_counties <- function(config) {
  b <- config$boundaries
  if (cardvar_source() == "fixtures") {
    path <- file.path("tests", "fixtures", "counties_naturalearth.geojson")
  } else {
    path <- b$cache
    if (!file.exists(path)) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      httr2::request(b$url) |>
        httr2::req_user_agent(config$source$user_agent) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform(path = path)
    }
  }
  g <- sf::st_read(path, quiet = TRUE)
  col <- if (is.null(b$name_col) || identical(b$name_col, "auto")) detect_county_col(g) else b$name_col
  g$county_key <- normalise_county(g[[col]])
  g <- g[g$county_key %in% COUNTIES_26, ]
  g <- sf::st_make_valid(sf::st_transform(g, 2157))
  parts <- split(sf::st_geometry(g), g$county_key)
  geom <- do.call(c, lapply(parts, sf::st_union))
  out <- sf::st_sf(county_key = names(parts), geometry = sf::st_sfc(geom, crs = 2157))
  out <- sf::st_simplify(out, preserveTopology = TRUE, dTolerance = 150)
  out <- sf::st_transform(out, 4326)
  out$county <- tools::toTitleCase(tolower(out$county_key))
  out
}

read_population <- function(config) {
  path <- if (cardvar_source() == "fixtures") {
    file.path("tests", "fixtures", "county_population_synthetic.csv")
  } else {
    config$population$file
  }
  if (is.null(path) || !file.exists(path)) return(NULL)
  p <- readr::read_csv(path, show_col_types = FALSE)
  names(p) <- tolower(names(p))
  if (!all(c("county", "population") %in% names(p))) {
    stop("Population file needs columns county and population: ", path)
  }
  p$county_key <- normalise_county(p$county)
  p[p$county_key %in% COUNTIES_26, c("county_key", "population")]
}
