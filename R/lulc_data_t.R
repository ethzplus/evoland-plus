#' Create LULC Data Table from Configuration
#'
#' Creates an lulc_data_t table based on the LULC data specification in an
#' evoland_config object. This function processes LULC data files and creates
#' data entries linking coordinates, LULC classes, and time periods.
#'
#' @name lulc_data_t
#'
#' @param config An [evoland_config] instance
#'
#' @return A data.table of class "lulc_data_t" with columns:
#'   - `id_coord`: Foreign key to coords_t
#'   - `id_lulc`: Foreign key to lulc_meta_t
#'   - `id_period`: Foreign key to periods_t
#'   - `date`: Exact date if available (nullable)
#' @export
create_lulc_data_t <- function() {
  # For now, create an empty table with proper structure
  # In a full implementation, this would process the actual LULC data files
  x <- data.table::data.table(
    id_coord = integer(0),
    id_lulc = integer(0),
    id_period = integer(0),
    date = as.Date(character(0))
  )

  data.table::setkeyv(x, c("id_coord", "id_lulc", "id_period"))

  new_evoland_table(x, "lulc_data_t")
}

#' @export
validate.lulc_data_t <- function(x, ...) {
  # Check that it's a data.table
  if (!inherits(x, "data.table")) {
    stop("lulc_data_t must inherit from data.table")
  }

  # Check required columns
  check_missing_names(x, c("id_coord", "id_lulc", "id_period", "date"))

  # Check column types
  if (!is.integer(x[["id_coord"]])) {
    id_coords <- as.integer(x[["id_coord"]])
    if (anyNA(id_coords)) {
      stop("id_coord must be int or coercible to it")
    }
    data.table::set(x, j = "id_coord", value = id_coords)
  }

  if (!is.integer(x[["id_lulc"]])) {
    id_lulcs <- as.integer(x[["id_lulc"]])
    if (anyNA(id_lulcs)) {
      stop("id_lulc must be int or coercible to it")
    }
    data.table::set(x, j = "id_lulc", value = id_lulcs)
  }

  if (!is.integer(x[["id_period"]])) {
    id_periods <- as.integer(x[["id_period"]])
    if (anyNA(id_periods)) {
      stop("id_period must be int or coercible to it")
    }
    data.table::set(x, j = "id_period", value = id_periods)
  }

  if (!inherits(x[["date"]], "Date")) {
    stop("date must be of class Date")
  }

  # if empty, don't run soft checks
  if (nrow(x) == 0L) {
    return(x)
  }

  # Check for unique combinations of id_coord, id_lulc, id_period
  if (anyDuplicated(x, by = c("id_coord", "id_lulc", "id_period"))) {
    stop("combinations of id_coord, id_lulc, id_period must be unique")
  }

  # Check for positive IDs
  if (any(x[["id_coord"]] <= 0) || any(x[["id_lulc"]] <= 0) || any(x[["id_period"]] <= 0)) {
    stop("id_coord, id_lulc, and id_period must be positive integers")
  }

  return(x)
}

#' @export
#' @describeIn lulc_data_t Print an lulc_data_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.lulc_data_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_coords <- data.table::uniqueN(x[["id_coord"]])
    n_lulc <- data.table::uniqueN(x[["id_lulc"]])
    n_periods <- data.table::uniqueN(x[["id_period"]])
    date_range <- if (any(!is.na(x[["date"]]))) {
      paste0("[", min(x[["date"]], na.rm = TRUE), ", ", max(x[["date"]], na.rm = TRUE), "]")
    } else {
      "no dates available"
    }

    cat(glue::glue(
      "LULC Data Table\n",
      "Observations: {nrow(x)}\n",
      "Coordinates: {n_coords}, LULC classes: {n_lulc}, Periods: {n_periods}\n",
      "Date range: {date_range}\n\n"
    ))
  } else {
    cat("LULC Data Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
