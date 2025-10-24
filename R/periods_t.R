#' Create Period Table
#'
#' Creates a period table, i.e. a description of discrete periods during which land use can
#' transition. This is necessary because a) land use data may not be available as regular time
#' series and need to be assigned to such a form, and b) because this normalization helps
#' consistency.
#'
#' @name periods_t
#'
#' @param x A list or data.frame coercible to a data.table
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

#' @describeIn periods_t Creates a periods_t table from specifications; periods that start after
#' `end_observed` are marked as extrapolated
#' @param period_length_str ISO 8601 duration string specifying the length of each period (currently
#' only accepting years, e.g., "P5Y" for 5 years)
#' @param start_observed Start date of the observed data (YYYY-MM-DD)
#' @param end_observed End date of the observed data (YYYY-MM-DD)
#' @param end_extrapolated End date for extrapolation time range (YYYY-MM-DD)
#' @export
create_periods_t <- function(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2020-01-01",
  end_extrapolated = "2060-01-01"
) {
  # Parse the period length (ISO 8601 duration)
  if (!stringi::stri_detect_regex(period_length_str, "^P\\d+Y$")) {
    stop("Only yearly period lengths are currently supported (e.g., P5Y)")
  }
  period_length_years <-
    stringi::stri_match_first_regex(period_length_str, "P(\\d+)Y")[2] |>
    as.integer()

  # Parse dates
  start_observed <- as.Date(start_observed)
  end_observed <- as.Date(end_observed)
  end_extrapolated <- as.Date(end_extrapolated)

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
