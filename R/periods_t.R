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
as_periods_t <- function(x) {
  new_evoland_table(
    x,
    "periods_t",
    "id_period"
  )
}

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

  as_periods_t(x)
}

#' @export
validate.periods_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_period",
      "start_date",
      "end_date",
      "is_extrapolated"
    )
  )

  stopifnot(
    is.integer(x[["id_period"]]),
    inherits(x[["start_date"]], "Date"),
    inherits(x[["end_date"]], "Date"),
    is.logical(x[["is_extrapolated"]]),
    !anyDuplicated(x[["id_period"]])
  )

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
