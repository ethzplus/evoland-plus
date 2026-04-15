#' Create Transition Rates Table
#'
#' Creates a trans_rates_t table that stores transition rates (probabilities) for each
#' transition type in each time period. Historical rates are calculated from observed
#' transitions, and future rates are extrapolated using linear regression.
#'
#' @name trans_rates_t
#'
#' @param x A list or data.frame coercible to a data.table
#'
#' @return A data.table of class "trans_rates_t" with columns:
#'   - `id_run`: Foreign key to runs_t
#'   - `id_period`: Foreign key to periods_t
#'   - `id_trans`: Foreign key to trans_meta_t
#'   - `count`: Absolute count of transitions
#'   - `rate`: Transition rate: count of transitions in (id_trans, id_period) over (id_period)
#' @export
as_trans_rates_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_run = integer(0),
      id_period = integer(0),
      id_trans = integer(0),
      count = integer(0),
      rate = numeric(0)
    )
  }

  data.table::setDT(x) |>
    cast_dt_col("id_run", "int") |>
    cast_dt_col("id_period", "int") |>
    cast_dt_col("id_trans", "int") |>
    cast_dt_col("count", "int") |>
    cast_dt_col("rate", "float")

  as_parquet_db_t(
    x,
    class_name = "trans_rates_t",
    key_cols = c("id_run", "id_period", "id_trans")
  )
}

#' @describeIn trans_rates_t Calculate observed transition rates from historical data.
#' For each period and transition type, calculates the rate as the proportion of
#' id_lulc_anterior cells that transitioned to id_lulc_posterior.
#' @param self a DB instance
#' @keywords internal
get_obs_trans_rates <- function(self) {
  # subsets to active id_run, but could also just group by id_run
  stopifnot("id_run must be set" = !is.null(self$id_run))
  lulc_expr <- self$get_read_expr("lulc_data_t")
  meta_expr <- self$get_read_expr("trans_meta_t")

  result <- self$get_query(glue::glue(
    r"{
    with trans_v as (
      select
        curr.id_period,
        prev.id_lulc as id_lulc_anterior,
        curr.id_lulc as id_lulc_posterior
      from
        {lulc_expr} as curr
      inner join
        {lulc_expr} as prev
      on
        curr.id_coord = prev.id_coord
        and curr.id_period = prev.id_period + 1
    ),
    counts as (
      select
        id_period,
        id_lulc_anterior,
        id_lulc_posterior,
        count(*) as n
      from
        trans_v
      group by
        id_period, id_lulc_anterior, id_lulc_posterior
    ),
    totals as (
      select
        id_period,
        id_lulc_anterior,
        sum(n) as total
      from
        counts
      group by
        id_period, id_lulc_anterior
    )
    select
      {self$id_run} as id_run,
      c.id_period,
      m.id_trans,
      c.n as count,
      cast(c.n as double) / t.total as rate
    from
      counts c,
      totals t,
      {meta_expr} m
    where
      c.id_period = t.id_period
      and c.id_lulc_anterior = t.id_lulc_anterior
      and c.id_lulc_anterior = m.id_lulc_anterior
      and c.id_lulc_posterior = m.id_lulc_posterior
    }"
  ))

  as_trans_rates_t(result)
}

#' @describeIn trans_rates_t Return future transition rates using linear regression. For
#' each id_run + id_trans, fits a linear model of rate vs period number and extrapolates
#' to future periods. Negative predicted rates are set to 0.
#' @param obs_rates A trans_rates_t table of observed transition rates for historical periods
#' @param periods A periods_t table with is_extrapolated = TRUE for future periods
#' @param coord_count Optional integer specifying the number of coordinates
#' (cells) for normalization
#' @export
extrapolate_trans_rates <- function(obs_rates, periods, coord_count = NA_integer_) {
  stopifnot(
    inherits(obs_rates, "trans_rates_t"),
    inherits(periods, "periods_t")
  )

  future_periods <- periods[is_extrapolated == TRUE][["id_period"]]

  if (length(future_periods) == 0L) {
    # No future periods to extrapolate
    return(as_trans_rates_t())
  }

  # split into list of subtables
  # fit model for each (id_trans) combination
  # extrapolate
  obs_rates |>
    split(by = c("id_run", "id_trans")) |>
    lapply(FUN = \(subtable) {
      # TODO maybe fit on count instead of rate? easier to reason about
      mod <- lm(rate ~ id_period, data = subtable)

      predictions <-
        suppressWarnings(predict(
          # suppress warnings (e.g. if model is rank-deficient, i.e. fit using one observed rate)
          mod,
          newdata = data.table::data.table(id_period = future_periods)
        )) |>
        unname() |> # drop names
        c() # concatenation drops attrs (e.g. `non-estim` from predict)

      # no negative rates (can only positively describe a->b transition)
      # there is no evident reason for transferring negative rates to b->a
      predictions[predictions < 0] <- 0

      data.table::data.table(
        # run and trans are constants
        id_run = subtable$id_run[1],
        id_trans = subtable$id_trans[1],
        id_period = future_periods,
        count = as.integer(round(coord_count * predictions)), # convert back to counts for storage
        rate = predictions
      )
    }) |>
    data.table::rbindlist() |>
    as_trans_rates_t()
}

#' @export
validate.trans_rates_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_run",
      "id_period",
      "id_trans",
      "count",
      "rate"
    )
  )

  stopifnot(
    "id_run is not integer" = is.integer(x[["id_run"]]),
    "id_period is not integer" = is.integer(x[["id_period"]]),
    "id_trans is not integer" = is.integer(x[["id_trans"]]),
    "rate is not numeric" = is.numeric(x[["rate"]]),
    "rate is negative" = all(x[["rate"]] >= 0, na.rm = TRUE)
  )

  return(x)
}

#' @export
#' @describeIn trans_rates_t Print a trans_rates_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.trans_rates_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_periods <- length(unique(x[["id_period"]]))
    n_trans <- length(unique(x[["id_trans"]]))
    rate_range <- range(x[["rate"]], na.rm = TRUE)

    cat(glue::glue(
      "Transition Rates Table\n",
      "Periods covered: {n_periods}\n",
      "Transition types: {n_trans}\n",
      "Rate range: [{round(rate_range[1], 4)}, {round(rate_range[2], 4)}]\n\n"
    ))
  } else {
    cat("Transition Rates Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
