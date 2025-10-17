#' Evoland Configuration Handling
#'
#' Provides functionality for reading JSON-based configuration files and converting them
#' into structured R objects for use in the evoland package.
#'
#' @name evoland_config
NULL

#' @describeIn evoland_config Read an Evoland Configuration File
#' @param config_path character, Path to the JSON configuration file.
#' @return An object of class "evoland_config" containing the parsed configuration.
#' @export

read_evoland_config <- function(config_path) {
  # Read and parse YAML file
  config_data <- yaml::read_yaml(config_path)

  validate(
    structure(
      config_data,
      class = "evoland_config"
    )
  )
}

#' @export
validate.evoland_config <- function(x, ...) {
  # Required names for each section
  check_missing_names(
    x,
    c("reporting", "coords", "lulc_data", "lulc_classes", "periods")
  )
  check_missing_names(
    x[["reporting"]],
    c("scenario_name", "shortname")
  )
  check_missing_names(
    x[["coords"]],
    c("type", "epsg", "extent", "resolution")
  )
  check_missing_names(
    x[["periods"]],
    c("period_length", "start_observed", "end_observed")
  )

  src_classes <-
    pluck_wildcard(x[["lulc_classes"]], NA, "src_classes") |>
    unlist()

  stopifnot(
    is.list(x[["reporting"]]),
    is.list(x[["lulc_classes"]]),
    length(x[["lulc_classes"]]) > 1,
    all(purrr::map_lgl(x[["lulc_classes"]], is.list)),
    is.list(x[["periods"]]),
    !anyDuplicated(src_classes)
  )

  x
}

#' @export
print.evoland_config <- function(x, ...) {
  cat("Evoland Configuration\n\n")
  cat(yaml::as.yaml(x))

  invisible(x)
}
