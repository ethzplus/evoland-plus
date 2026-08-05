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
#'   - `count`: Absolute number of transitioning cells for (id_trans, id_period)
#'   - `rate`: Transition rate: count of transitions in (id_trans, id_period)
#'     over count of cells of id_lulc_anterior in id_period
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
    inherits(periods, "periods_t"),
    "no extrapolation periods in periods_t" = {
      nrow(periods[is_extrapolated == TRUE]) > 0
    }
  )

  extrap_mean_dates <- periods[is_extrapolated == TRUE, mean_date]
  extrap_id_periods <- periods[is_extrapolated == TRUE, id_period]

  # split into list of subtables
  # fit model for each (id_trans) combination
  # extrapolate
  obs_rates |>
    merge(periods, by = "id_period") |>
    split(by = c("id_run", "id_trans")) |>
    lapply(FUN = \(subtable) {
      mod <- lm(rate ~ mean_date, data = subtable)

      predictions <-
        suppressWarnings(predict(
          # suppress warnings (e.g. if model is rank-deficient, i.e. fit using one observed rate)
          mod,
          newdata = data.table::data.table(mean_date = extrap_mean_dates)
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
        id_period = extrap_id_periods,
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

#' @describeIn trans_rates_t Replay a rate table forward from an observed state to recover
#' the class areas it implies. Transitions not recorded in `rates` are implied to be zero,
#' so the residual `1 - sum(rate)` of each class persists. This is what makes a solved
#' trajectory recoverable from a [trans_rates_t] alone, and therefore comparable against
#' the areas an allocation run actually realised.
#'
#' @param lulc_data A [lulc_data_t] for a single `id_run`; the areas of its last period are
#' the state the replay starts from.
#' @param rates A [trans_rates_t] table for a single `id_run`.
#' @param trans_meta A [trans_meta_t] table, resolving `id_trans` to a pair of classes.
#' @return `trans_rate_areas()` returns a data.table with `id_lulc`, `id_period` and
#' `area`; `id_period` is the period whose *state* the area describes, so the initial state
#' carries the last period of `lulc_data`.
#' @export
trans_rate_areas <- function(lulc_data, rates, trans_meta) {
  stopifnot(
    inherits(lulc_data, "lulc_data_t"),
    inherits(rates, "trans_rates_t"),
    inherits(trans_meta, "trans_meta_t"),
    "lulc_data must contain exactly one id_run" = {
      data.table::uniqueN(lulc_data[["id_run"]]) == 1L
    },
    "rates must contain exactly one id_run" = {
      data.table::uniqueN(rates[["id_run"]]) == 1L
    }
  )

  transitions <-
    rates[
      trans_meta,
      .(id_period, id_lulc_anterior, id_lulc_posterior, rate),
      on = "id_trans",
      nomatch = NULL
    ]
  stopifnot(
    "outflow rates sum above 1 for some class and period" = {
      transitions[, all(sum(rate) <= 1 + 1e-9), by = .(id_lulc_anterior, id_period)][, all(V1)]
    }
  )

  state <-
    lulc_data[
      id_period == max(id_period),
      .(area = as.numeric(.N)),
      by = .(id_lulc, id_period)
    ][order(id_lulc)]

  step_periods <- sort(unique(transitions[["id_period"]]))
  trajectory <- vector("list", length(step_periods) + 1L)
  trajectory[[1L]] <- state

  for (i in seq_along(step_periods)) {
    moved <-
      transitions[id_period == step_periods[i]][
        state,
        .(id_lulc_anterior, id_lulc_posterior, cells = rate * i.area),
        on = .(id_lulc_anterior = id_lulc),
        nomatch = NULL
      ]

    state <- data.table::copy(state)[, id_period := step_periods[i]]
    state[
      moved[, .(loss = sum(cells)), by = id_lulc_anterior],
      area := area - i.loss,
      on = .(id_lulc = id_lulc_anterior)
    ]
    state[
      moved[, .(gain = sum(cells)), by = id_lulc_posterior],
      area := area + i.gain,
      on = .(id_lulc = id_lulc_posterior)
    ]
    trajectory[[i + 1L]] <- state
  }

  data.table::rbindlist(trajectory)
}
