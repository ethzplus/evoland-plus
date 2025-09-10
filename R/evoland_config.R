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
  # Required top-level sections
  check_missing_names(x, c("reporting", "coords", "lulc_data", "lulc_classes", "periods"))

  # Validate reporting section
  if (!is.list(x[["reporting"]])) {
    stop("'reporting' section must be a list")
  }
  check_missing_names(x[["reporting"]], c("scenario_name", "shortname"))

  # Validate coords section
  check_missing_names(x[["coords"]], c("type", "epsg", "extent", "resolution"))

  # Validate lulc_classes section
  if (!is.list(x[["lulc_classes"]])) {
    stop("'lulc_classes' section must be a list")
  }
  if (length(x[["lulc_classes"]]) < 2) {
    stop("At least two LULC classes must be defined")
  }
  # Validate each LULC class
  for (class_name in names(x[["lulc_classes"]])) {
    class_def <- x[["lulc_classes"]][[class_name]]
    if (!is.list(class_def)) {
      stop(glue::glue("LULC class '{class_name}' must be a list"))
    }
    if (!"pretty_name" %in% names(class_def)) {
      stop(glue::glue("LULC class '{class_name}' missing required 'pretty_name' field"))
    }
  }

  # Validate periods section
  if (!is.list(x[["periods"]])) {
    stop("'periods' section must be a list")
  }
  check_missing_names(x[["periods"]], c("period_length", "start_observed", "end_observed"))

  x
}

#' @export
print.evoland_config <- function(x, ...) {
  cat("Evoland Configuration\n\n")
  cat(yaml::as.yaml(x))

  invisible(x)
}
