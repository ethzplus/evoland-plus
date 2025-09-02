#' Evoland Configuration Handling
#'
#' Provides functionality for reading YAML-based configuration files and converting them
#' into structured R objects for use in the evoland package.
#'
#' @name evoland_config
NULL

#' @describeIn evoland_config Read an Evoland Configuration File
#' @param path character, Path to the YAML configuration file.
#'
#' @return An object of class "evoland_config" containing the parsed configuration.
#' @export

read_evoland_config <- function(path) {
  # Read and parse YAML file
  config_data <- yaml::read_yaml(path)

  validate(
    structure(
      config_data,
      class = "evoland_config"
    )
  )
}

#' @export
validate.evoland_config <- function(config) {
  # Required top-level sections
  required_sections <- c("reporting", "coords", "lulc_data", "lulc_classes", "periods")

  missing_sections <- setdiff(required_sections, names(config))
  if (length(missing_sections) > 0) {
    stop(glue::glue(
      "Missing required configuration sections: {paste(missing_sections, collapse = ', ')}"
    ))
  }

  # Validate reporting section
  if (!is.list(config[["reporting"]])) {
    stop("'reporting' section must be a list")
  }
  required_reporting <- c("scenario_name", "shortname")
  missing_reporting <- setdiff(required_reporting, names(config[["reporting"]]))
  if (length(missing_reporting) > 0) {
    stop(glue::glue(
      "Missing required reporting fields: {paste(missing_reporting, collapse = ', ')}"
    ))
  }

  # Validate coords section
  required_coords <- c("type", "epsg", "extent", "resolution")
  missing_coords <- setdiff(required_coords, names(config[["coords"]]))
  if (length(missing_coords) > 0) {
    stop(glue::glue("Missing required coords fields: {paste(missing_coords, collapse = ', ')}"))
  }

  # Validate lulc_classes section
  if (!is.list(config[["lulc_classes"]])) {
    stop("'lulc_classes' section must be a list")
  }
  if (length(config[["lulc_classes"]]) < 2) {
    stop("At least two LULC classes must be defined")
  }
  # Validate each LULC class
  for (class_name in names(config[["lulc_classes"]])) {
    class_def <- config[["lulc_classes"]][[class_name]]
    if (!is.list(class_def)) {
      stop(glue::glue("LULC class '{class_name}' must be a list"))
    }
    if (!"pretty_name" %in% names(class_def)) {
      stop(glue::glue("LULC class '{class_name}' missing required 'pretty_name' field"))
    }
  }

  # Validate periods section
  if (!is.list(config[["periods"]])) {
    stop("'periods' section must be a list")
  }

  required_periods <- c("period_length", "start_observed", "end_observed")
  missing_periods <- setdiff(required_periods, names(config[["periods"]]))
  if (length(missing_periods) > 0) {
    stop(glue::glue("Missing required periods fields: {paste(missing_periods, collapse = ', ')}"))
  }

  config
}

#' @export
print.evoland_config <- function(x, ...) {
  cat("Evoland Configuration\n\n")
  cat(yaml::as.yaml(unclass(x)))

  invisible(x)
}

#' Configuration Table Class
#'
#' A data.table-based class for storing multiple evoland configurations.
#' Inherits from data.table and provides methods for managing collections
#' of configuration objects.
#'
#' @param x named list of evoland_config objects
#'
#' @export
config_t <- function(x) {
  # eval sequentially
  stopifnot(
    is.list(x),
    !is.null(names(x)), # has to have names
    all(names(x) != ""), # no name can be empty
    all(purrr::map_lgl(x, inherits, what = "evoland_config"))
  )

  dt <- data.table::data.table(
    name = names(x),
    config = x
  )

  class(dt) <- c("config_t", "evoland_table", class(dt))
  dt
}
