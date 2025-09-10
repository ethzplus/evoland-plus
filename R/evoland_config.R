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
  cat(yaml::as.yaml(x))

  invisible(x)
}

# This is the active binding function for config handling in the evoland_db class
utils::globalVariables("self")
active_binding_config <- function(config_data) {
  if (missing(config_data)) {
    config_data <- DBI::dbGetQuery(
      self$connection,
      "select r_obj from config_t limit 1"
    )
    if (nrow(config_data) == 0L) {
      stop("No config ingested yet", call. = FALSE)
    }

    config_data <- qs::qdeserialize(
      config_data[["r_obj"]][[1]]
    )

    out <- validate(structure(config_data, class = "evoland_config"))
    return(out)
  }
  if (self$row_count("config_t") > 0L) {
    stop("DB already has a config! Use db$delete_from('config_t') to delete it")
  }
  config_json <- "{}" # empty until we can reliably (de)serialize JSON

  df <- data.table::data.table(
    config = config_json,
    r_obj = list(qs::qserialize(config_data))
  )
  self$commit(df, "config_t", mode = "overwrite")
}
