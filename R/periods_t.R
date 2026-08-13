#' Create Period Table
#'
#' Creates a `periods_t` table, i.e. a description of discrete, regular periods during
#' which land use can transition. This is a precondition for pattern based land use
#' change models. Periods outside the observed range are designated `is_extrapolated`.
#' The special period with ID 0 is used for static phenomena, coded as instantaneous at
#' the end of the observed period.
#'
#' @name periods_t
#'
#' @param x A list or data.frame coercible to a data.table
#'
#' @return A data.table of class "periods_t" with columns:
#'   - `id_period`: Unique ID for each tperiod
#'   - `start_date`: Start date for period
#'   - `end_date`: End date for period
#'   - `period_length_d`: Days between this and the preceding period's
#'     midpoints, derived from start and end date.
#'   - `is_extrapolated`: bool, are observations matched to this period, or is it used
#'     for extrapolation?
#' @export
as_periods_t <- function(x) {
  data.table::setDT(x) |>
    cast_dt_col("id_period", "int") |>
    cast_dt_col("start_date", "date") |>
    cast_dt_col("end_date", "date") |>
    cast_dt_col("is_extrapolated", "bool")

  data.table::setorder(x, id_period)
  x[, mean_date := start_date + difftime(end_date, start_date) / 2]
  x[
    id_period > 0,
    period_length_d := as.integer(difftime(
      mean_date,
      data.table::shift(mean_date, n = 1),
      units = "days"
    ))
  ]

  as_parquet_db_t(
    x,
    class_name = "periods_t",
    key_cols = c("start_date", "end_date"),
    alternate_key_cols = "id_period",
  )
}

#' @describeIn periods_t Creates a regular `periods_t` table from specifications.
#' @param period_length_str ISO 8601 duration string specifying the length of each
#' period (currently only accepting years, e.g., "P5Y" for 5 years)
#' @param start_observed Start date of the observed data (YYYY-MM-DD)
#' @param end_observed End date of the observed data (YYYY-MM-DD); periods that start
#' after `end_observed` are marked `is_extrapolated`.
#' @param end_extrapolated End date for extrapolation time range (YYYY-MM-DD); only full
#' periods _before_ this date are taken into account.
#' @export
create_periods_t <- function(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2020-01-01",
  end_extrapolated = "2059-12-31"
) {
  # Parse the period length (ISO 8601 duration)
  if (!grepl("^P\\d+Y$", period_length_str)) {
    stop("Only yearly period lengths are currently supported (e.g., P5Y)")
  }
  period_length_years <-
    sub("^P(\\d+)Y$", "\\1", period_length_str) |>
    as.integer()

  # Parse dates
  start_observed <- as.Date(start_observed)
  end_observed <- as.Date(end_observed)
  end_extrapolated <- as.Date(end_extrapolated)

  # Sequence of boundaries: every start date before end_extrapolated
  boundaries <-
    seq(start_observed, end_extrapolated, by = paste(period_length_years, "years")) |>
    length() |>
    (\(x) x + 1)() |> # one period longer
    seq(start_observed, by = paste(period_length_years, "years"), length.out = _)

  # lead, drop last date
  start_dates <- utils::head(boundaries, -1L)
  # lag, drop first date: start of next period minus 1 day
  end_dates <- utils::tail(boundaries, -1L) - 1

  # Determine which periods are observed vs extrapolated
  is_extrapolated <- start_dates > end_observed

  # Create the data.table
  x <- rbind(
    data.table::data.table(
      id_period = 0L,
      start_date = end_observed,
      end_date = end_observed,
      is_extrapolated = FALSE
    ),
    data.table::data.table(
      id_period = seq_along(start_dates),
      start_date = start_dates,
      end_date = end_dates,
      is_extrapolated = is_extrapolated
    )
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
      "mean_date",
      "period_length_d",
      "is_extrapolated"
    )
  )

  overlapping <-
    x[
      id_period > 1 # not meaningfully defined for periods 0 and 1
    ][
      order(id_period) &
        (data.table::shift(end_date, 1L, 0) - start_date) >= 0
    ]

  # leap years may not deviate by more than 2 days
  # exclude first extrapolated period because the last observed period may be irregular
  # nonproblematic if only one period
  extrap_lengths <- x[is_extrapolated == TRUE, period_length_d]
  equal_lengths <- if (length(extrap_lengths) > 1) {
    diff(range(tail(extrap_lengths, -1))) <= 2 # can only
  } else {
    TRUE
  }

  stopifnot(
    "id_period should be an integer" = is.integer(x[["id_period"]]),
    "start_date should be a Date" = inherits(x[["start_date"]], "Date"),
    "end_date should be a Date" = inherits(x[["end_date"]], "Date"),
    "is_extrapolated should be bool" = is.logical(x[["is_extrapolated"]]),
    "extrapolated periods must be regular (within leap-year tolerance)" = equal_lengths,
    "periods must not overlap" = nrow(overlapping) == 0
  )

  return(x)
}

#' @export
#' @describeIn periods_t Print a periods_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.periods_t <- function(x, nrow = 10, ...) {
  n_observed <- sum(!x[["is_extrapolated"]])
  n_extrapolated <- sum(x[["is_extrapolated"]])
  cat(glue::glue(
    "Periods Table\n",
    "Date range: [{min(x[['start_date']])}, {max(x[['end_date']])}]\n",
    "Observed periods: {n_observed}, Extrapolated periods: {n_extrapolated}\n\n"
  ))

  NextMethod(nrow = nrow, ...)
  invisible(x)
}
