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
#'   - `id_period`: Foreign key to periods_t
#'   - `id_trans`: Foreign key to trans_meta_t
#'   - `rate`: Transition rate (0 to 1)
#' @export
as_trans_rates_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_period = integer(0),
      id_trans = integer(0),
      rate = numeric(0)
    )
  }
  new_evoland_table(
    x,
    "trans_rates_t",
    c("id_period", "id_trans")
  )
}

#' @describeIn trans_rates_t Calculate observed transition rates from historical data.
#' For each period and transition type, calculates the rate as the proportion of
#' id_lulc_anterior cells that transitioned to id_lulc_posterior.
#'
#' @param trans_v A transitions view table with columns: id_period, id_lulc_anterior,
#'   id_lulc_posterior, id_coord
#' @param trans_meta A trans_meta_t table with transition metadata including id_trans,
#'   id_lulc_anterior, and id_lulc_posterior
#'
#' @export
create_obs_trans_rates_t <- function(trans_v, trans_meta) {
  stopifnot(inherits(trans_meta, "trans_meta_t"))

  # Ensure trans_v is a data.table
  trans_v <- data.table::as.data.table(trans_v)
  trans_meta <- data.table::as.data.table(trans_meta)

  # For each period and id_lulc_anterior, count total cells
  total_anterior <- trans_v[,
    .(total = .N),
    by = .(id_period, id_lulc_anterior)
  ]

  # For each period and transition (anterior -> posterior), count occurrences
  trans_counts <- trans_v[,
    .(count = .N),
    by = .(id_period, id_lulc_anterior, id_lulc_posterior)
  ]

  # Join with totals to calculate rates
  trans_rates <- trans_counts[
    total_anterior,
    on = .(id_period, id_lulc_anterior)
  ][,
    .(
      id_period,
      id_lulc_anterior,
      id_lulc_posterior,
      rate = count / total
    )
  ]

  # Join with trans_meta to get id_trans
  # Only keep transitions that are in trans_meta
  result <- trans_meta[,
    .(id_trans, id_lulc_anterior, id_lulc_posterior)
  ][
    trans_rates,
    on = .(id_lulc_anterior, id_lulc_posterior),
    nomatch = NULL
  ][,
    .(id_period, id_trans, rate)
  ]

  as_trans_rates_t(result)
}

#' @describeIn trans_rates_t Extrapolate future transition rates using linear regression.
#' For each transition type (id_trans), fits a linear model of rate vs period number
#' and extrapolates to future periods. Negative predicted rates are set to 0.
#'
#' @param obs_rates A trans_rates_t table with observed historical rates
#' @param periods A periods_t table defining the time periods
#'
#' @export
create_extr_trans_rates_t <- function(obs_rates, periods) {
  stopifnot(
    inherits(obs_rates, "trans_rates_t"),
    inherits(periods, "periods_t")
  )

  # Ensure inputs are data.tables
  obs_rates <- data.table::as.data.table(obs_rates)
  periods <- data.table::as.data.table(periods)

  # Get future periods to extrapolate
  future_periods <- periods[is_extrapolated == TRUE & id_period > 0, .(id_period)]

  if (nrow(future_periods) == 0L) {
    # No future periods to extrapolate
    return(as_trans_rates_t())
  }

  # Get unique transition IDs from observed rates
  trans_ids <- unique(obs_rates$id_trans)

  # Initialize result list
  result_list <- list()

  # For each transition, fit linear model and extrapolate
  for (trans_id in trans_ids) {
    trans_data <- obs_rates[id_trans == trans_id]

    if (nrow(trans_data) < 2) {
      # If only one observation, use that rate for all future periods; else 0
      if (nrow(trans_data) == 1) {
        pred_rate <- trans_data$rate[1]
      } else {
        pred_rate <- 0
      }

      result_list[[trans_id]] <- data.table::data.table(
        id_period = future_periods$id_period,
        id_trans = trans_id,
        rate = pred_rate
      )
    } else {
      # TODO we should probably fit on absolute amounts, which is easier to reason about
      model <- lm(rate ~ id_period, data = trans_data)

      predictions <- predict(
        model,
        newdata = data.table::data.table(id_period = future_periods$id_period)
      )

      # no negative rates (can only positively describe a->b)
      # transferring negative rates to b->a does not make inherent sense
      predictions[predictions < 0] <- 0

      result_list[[trans_id]] <- data.table::data.table(
        id_period = future_periods$id_period,
        id_trans = trans_id,
        rate = predictions
      )
    }
  }

  if (length(result_list) == 0) {
    return(as_trans_rates_t())
  }

  result_list |>
    data.table::rbindlist() |>
    as_trans_rates_t()
}

#' @export
validate.trans_rates_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_period",
      "id_trans",
      "rate"
    )
  )

  stopifnot(
    "id_period is not integer" = is.integer(x[["id_period"]]),
    "id_trans is not integer" = is.integer(x[["id_trans"]]),
    "rate is not numeric" = is.numeric(x[["rate"]]),
    "rate is negative" = all(x[["rate"]] >= 0, na.rm = TRUE),
    "duplicated id_period, id_trans tuple" = !anyDuplicated(x, by = c("id_period", "id_trans"))
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
