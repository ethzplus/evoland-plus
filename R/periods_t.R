#' Create Period Table from Configuration
#'
#' Creates a period table, i.e. a description of discrete periods during which land use
#' can transition, based on the specification in an [evoland_config] object.
#'
#' @name periods_t
#'
#' @param config An [evoland_config] instance
#'
#' @return A data.table of class "periods_t" with columns:
#'   - `id_period`: Unique ID for each tperiod
#'   - `start_date`: Start date for period
#'   - `end_date`: End date for period
#'   - `is_extrapolated`: bool, are observations matched to this period, or is it used
#'     for extrapolation?
#' @export
create_periods_t <- function(config) {
  periods_spec <- config[["periods"]]

  # Parse the period length (ISO 8601 duration)
  period_length_str <- periods_spec[["period_length"]]
  if (!grepl("^P\\d+Y$", period_length_str)) {
    stop("Only yearly period lengths are currently supported (e.g., P5Y)")
  }
  period_length_years <- as.numeric(gsub("P(\\d+)Y", "\\1", period_length_str))

  # Parse dates
  start_observed <- as.Date(periods_spec[["start_observed"]])
  end_observed <- as.Date(periods_spec[["end_observed"]])
  end_extrapolated <- as.Date(periods_spec[["end_extrapolated"]])

  # Generate sequence of start dates from start_observed to end_extrapolated
  start_dates <- seq(
    from = start_observed,
    to = end_extrapolated,
    by = paste(period_length_years, "years")
  )

  # Calculate end dates (start of next period minus 1 day, except for the last period)
  end_dates <- c(
    start_dates[-1] - 1, # End dates are one day before the next period starts
    end_extrapolated # Last period ends at the extrapolated end date
  )

  # Determine which periods are observed vs extrapolated
  is_extrapolated <- start_dates > end_observed

  # Create the data.table
  x <- data.table::data.table(
    id_period = seq_along(start_dates),
    start_date = start_dates,
    end_date = end_dates,
    is_extrapolated = is_extrapolated
  )

  data.table::setkey(x, "id_period")

  new_evoland_table(x, "periods_t")
}

#' @export
validate.periods_t <- function(x, ...) {
  # Check that it's a data.table
  if (!inherits(x, "data.table")) {
    stop("periods_t must inherit from data.table")
  }

  # Check required columns
  check_missing_names(x, c("id_period", "start_date", "end_date", "is_extrapolated"))

  # Check column types
  if (!is.integer(x[["id_period"]])) {
    id_ints <- as.integer(x[["id_period"]])
    if (anyNA(id_ints)) {
      stop("id_period must be int or coercible to it")
    }
    data.table::set(x, j = "id_period", value = id_ints)
  }

  if (!inherits(x[["start_date"]], "Date") || !inherits(x[["end_date"]], "Date")) {
    stop("start_date and end_date must be of class Date")
  }

  if (!is.logical(x[["is_extrapolated"]])) {
    stop("is_extrapolated must be logical (bool)")
  }

  return(x)
}

#' @export
#' @describeIn periods_t Print a periods_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.periods_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 1) {
    n_observed <- sum(!x[["is_extrapolated"]])
    n_extrapolated <- sum(x[["is_extrapolated"]])
    cat(glue::glue(
      "Periods Table\n",
      "Date range: [{min(x[['start_date']])}, {max(x[['end_date']])}]\n",
      "Observed periods: {n_observed}, Extrapolated periods: {n_extrapolated}\n\n"
    ))
  } else {
    cat("Periods Table\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
