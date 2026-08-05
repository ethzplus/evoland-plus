#' Demand-driven transition rate solver
#'
#' A coupled linear program that derives per-transition flows from per-class area targets,
#' as an alternative to [extrapolate_trans_rates()], which fits one independent univariate
#' regression per transition and has no input for a scenario target.
#'
#' The model works in three unit layers, each at the layer where it is correct:
#' scenario targets may be given as a share of the landscape (grid-independent), the LP
#' itself is solved in shares and reported in cells (readable diagnostics), and the result
#' is written to [trans_rates_t] as both a `rate` (what [evoland_db] `adjusted_trans_pot_v`
#' and the allocators consume) and a `count` of cells.
#'
#' The program is built and solved by the [trans_rate_lp] class; these functions are the
#' one-call form of it. Decision variables are the per-period flows between classes and the
#' per-period class areas. Constraints are the initial condition, row and column closure,
#' total-area conservation, softly penalised per-transition rate bounds derived from
#' observed history, an optional hard monotonic direction per class, and optional soft
#' trajectory-shape and smoothness terms. The objective is a weighted sum of slack
#' penalties, normalised per class so that small classes are not ignored.
#'
#' @section Reachability:
#' Elicited scenario targets are normative and routinely lie outside the envelope of
#' observed transition rates. [trans_rate_reachability()] quantifies this without needing a
#' target, and [solve_trans_rates()] runs it as a precheck: it *reports* how far outside
#' history each target is and only fails on gross violations (see `max_reachability_ratio`).
#' A solver that does not report how far outside history it went is actively misleading, so
#' the returned diagnostics are part of the result, not debug output.
#'
#' @section Solver dependency:
#' The formulation is deliberately kept inside what [lpSolve::lp()] can express. Terms that
#' would usually be written as quadratic penalties (terminal fit, historic preference,
#' fairness across classes) are implemented as their L1 or minimax equivalents, which are
#' linear.
#'
#' `lpSolve` is a *suggested* dependency, because most of evoland never solves a linear
#' program: [trans_rate_bounds()] and [trans_rate_areas()] work without it, while
#' [trans_rate_reachability()] and [solve_trans_rates()] need it installed and say so if it
#' is missing.
#'
#' @name trans_rates_solver
#' @seealso [trans_rate_lp], [trans_rates_t], [trans_meta_t], [periods_t]
NULL

#' @describeIn trans_rates_solver Derive per-transition minimum and maximum rate bounds
#' from observed history. Rates are annualised before the minimum and maximum are taken and
#' then re-inflated to the length of an extrapolated period, because observed periods are
#' typically irregular. Transitions absent from `obs_rates` in a given period count as a
#' rate of 0 for that period, not as missing -- otherwise `min_rate` is biased upward.
#'
#' @param obs_rates A [trans_rates_t] table of observed rates for a single `id_run`, as
#' returned by `evoland_db$get_obs_trans_rates()`.
#' @param periods A [periods_t] table, used for the true period lengths.
#' @param trans_meta A [trans_meta_t] table, defining the full set of transitions and their
#' viability.
#' @param include_persistence Whether to reconstruct the `i -> i` diagonal as
#' `1 - sum(rate)` over all transitions out of `i`. The diagonal is not part of
#' [trans_meta_t] but the LP needs it; without it, persistence is left unbounded.
#' @return `trans_rate_bounds()` returns a data.table with `id_trans` (`NA` on the
#' diagonal), `id_lulc_anterior`, `id_lulc_posterior`, `min_rate`, `max_rate`, `ref_rate`
#' (the historic mean, for the optional historic-preference term) and `is_viable`.
#' @export
trans_rate_bounds <- function(
  obs_rates,
  periods,
  trans_meta,
  include_persistence = TRUE
) {
  stopifnot(
    inherits(obs_rates, "trans_rates_t"),
    inherits(periods, "periods_t"),
    inherits(trans_meta, "trans_meta_t"),
    "must have at least 2 non-0 periods" = nrow(periods) > 2,
    "obs_rates must contain exactly one id_run" = {
      length(unique(obs_rates[["id_run"]])) == 1L
    },
    "obs_rates is empty" = nrow(obs_rates) > 0L
  )

  # may not be one unique period length because of leap years
  extrap_step_years <- mean(periods[is_extrapolated == TRUE, period_length_d / 365.25])

  # only interested in observed periods that carry a rate (i.e. cannot be the
  # first period)
  obs_periods <-
    data.table::as.data.table(
      periods[id_period > 1 & is_extrapolated == FALSE, ]
    )[,
      period_length_y := period_length_d / 365.25 # good enough for leap years
    ]

  rates <-
    # like tidyr::complete obs_rates: all id_trans x observed id_period
    obs_rates[
      data.table::CJ(
        id_trans = trans_meta[, id_trans],
        id_period = obs_periods[, id_period],
        unique = TRUE
      ),
      .(id_trans, id_period, rate),
      on = c("id_trans", "id_period")
    ][
      # if not observed: no transitions happened
      is.na(rate),
      rate := 0
    ][
      # there is an id_trans for non-persistence transitions
      trans_meta,
      on = "id_trans"
    ][
      obs_periods,
      .(
        id_trans,
        id_period,
        id_lulc_anterior,
        id_lulc_posterior,
        is_viable,
        rate,
        period_length_y,
        is_persistence = FALSE
      ),
      on = "id_period",
      nomatch = NULL
    ]

  if (include_persistence) {
    # all the cells that did _not_ transition
    persistence <- rates[,
      .(
        rate = 1 - sum(rate),
        id_trans = NA_integer_,
        is_viable = TRUE,
        is_persistence = TRUE,
        id_lulc_posterior = id_lulc_anterior
      ),
      by = .(id_lulc_anterior, id_period, period_length_y)
    ]
    rates <- rbind(rates, persistence, use.names = TRUE, fill = TRUE)
  }

  bounds <-
    rates[,
      rate_annual := rescale_trans_rate(rate, period_length_y, 1, is_persistence)
    ][,
      .(
        min_rate = rescale_trans_rate(min(rate_annual), 1, extrap_step_years, is_persistence[1L]),
        max_rate = rescale_trans_rate(max(rate_annual), 1, extrap_step_years, is_persistence[1L]),
        ref_rate = rescale_trans_rate(mean(rate_annual), 1, extrap_step_years, is_persistence[1L])
      ),
      by = .(id_trans, id_lulc_anterior, id_lulc_posterior, is_viable)
    ][
      order(id_lulc_anterior, id_lulc_posterior)
    ]

  bounds
}

#' @describeIn trans_rates_solver Reachability precheck: the areas each class can attain at
#' each extrapolated period under mass balance and *hard* historic maximum rates, ignoring
#' targets. Persistence is left free and minimum rates are not imposed, which makes this
#' the loosest honest question -- "can this class get there at all, given that no
#' transition has ever moved faster than it historically did?".
#'
#' @param lulc_data A [lulc_data_t] for a single `id_run`; the areas of its last observed
#' period are the initial state.
#' @param bounds Per-transition rate bounds, as returned by [trans_rate_bounds()].
#' @return `trans_rate_reachability()` returns a data.table with `id_lulc`, `id_period`,
#' `area_init`, `area_min` and `area_max`, in cells.
#' @export
trans_rate_reachability <- function(lulc_data, bounds, periods) {
  trans_rate_lp$new(lulc_data = lulc_data, bounds = bounds, periods = periods)$reachability
}

#' @describeIn trans_rates_solver Solve for the per-transition flows that take the
#' landscape from its observed state to `targets` over the extrapolated periods. A one-call
#' form of [trans_rate_lp]; use the class itself to inspect the program or to write the
#' same solution to several runs.
#'
#' @param targets A data.table with `id_lulc` and either `area` (cells on the same grid as
#' `lulc_data`) or `share` (of the landscape, rehydrated against `lulc_data`).
#' @param shapes A data.table with `id_lulc` and `shape`, or `NULL` for no shape
#' preference. See [trans_rate_lp].
#' @param ... Further arguments to `trans_rate_lp$new()`, such as the penalty weights.
#' @return `solve_trans_rates()` returns the solved [trans_rate_lp] object.
#' @export
solve_trans_rates <- function(lulc_data, targets, shapes, bounds, periods, ...) {
  solver <- trans_rate_lp$new(
    lulc_data = lulc_data,
    bounds = bounds,
    periods = periods,
    targets = targets,
    shapes = shapes,
    ...
  )
  solver$solve()
  solver
}

#' Preparing the inputs of the transition rate solver
#'
#' Unit conversion and label normalisation that happen before any program is built. The
#' program itself is in [trans_rate_lp].
#'
#' @name trans_rate_inputs
#' @keywords internal
NULL

#' @describeIn trans_rate_inputs Rescale a transition rate between period lengths.
#' Transition rates are survival-style quantities: the complement `1 - rate` compounds
#' multiplicatively, so a rate observed over 12 years is not comparable to one over 10.
#' Persistence (a class staying itself) is the complement of the total outflow and
#' therefore compounds directly.
#'
#' @param rate Numeric vector of rates in `[0, 1]`.
#' @param from_years,to_years Period lengths in years.
#' @param is_persistence Logical, `TRUE` for `i -> i` rates.
#' @return `rescale_trans_rate()` returns rates on the `to_years` scale.
#' @keywords internal
rescale_trans_rate <- function(rate, from_years, to_years, is_persistence = FALSE) {
  exponent <- to_years / from_years
  data.table::fifelse(
    is_persistence,
    rate^exponent,
    1 - (1 - rate)^exponent
  )
}

#' @describeIn trans_rate_inputs Normalise elicited trajectory shape labels.
#'
#' @param shapes A data.table with `id_lulc` and `shape`.
#' @return `canonical_shapes()` returns `shapes` with canonical labels, `NA` where unset.
#' @keywords internal
canonical_shapes <- function(shapes) {
  known <- c(
    "instant growth",
    "delayed growth",
    "constant change",
    "instant decline",
    "delayed decline"
  )
  stopifnot(
    "shapes needs columns id_lulc and shape" = all(c("id_lulc", "shape") %in% names(shapes))
  )

  canonical <- data.table::as.data.table(shapes)[,
    .(id_lulc, shape = tolower(trimws(as.character(shape))))
  ]
  canonical[shape %chin% c("", "na", "none"), shape := NA_character_]

  stopifnot(
    "unknown shape labels" = all(canonical[!is.na(shape), shape] %chin% known)
  )
  canonical
}
