#' Demand-driven transition rate solver
#'
#' A coupled linear program that derives per-transition flows from per-class area targets,
#' as an alternative to [extrapolate_trans_rates()], which fits one independent univariate
#' regression per transition and has no input for a scenario target.
#'
#' The model works in three unit layers, each at the layer where it is correct:
#' scenario targets may be given as a share of the landscape (grid-independent), the LP
#' itself is solved in absolute cells (readable diagnostics), and the result is written to
#' [trans_rates_t] as both a `rate` (what [evoland_db] `adjusted_trans_pot_v` and the
#' allocators consume) and a `count` of cells.
#'
#' Decision variables are the per-step flows `x[i, j, t]` between classes and the per-step
#' class areas `area[l, t]`; both are absolute. Constraints are the initial condition,
#' row (outflow) and column (inflow) closure, total-area conservation, softly penalised
#' per-transition rate bounds derived from observed history, an optional hard monotonic
#' direction per class, and optional soft trajectory-shape and smoothness terms. The
#' objective is a weighted sum of slack penalties, normalised per class so that small
#' classes are not ignored. The program itself is assembled block by block in
#' [trans_rate_lp], and the inputs are prepared by [trans_rate_inputs].
#'
#' @section Reachability:
#' Elicited scenario targets are normative and routinely lie outside the envelope of
#' observed transition rates. [trans_rate_reachability()] quantifies this without needing a
#' target, and [solve_trans_rates()] runs it as a precheck: it *reports* how far outside
#' history each target is and only fails on gross violations (see `max_reachability_ratio`).
#' A solver that does not report how far outside history it went is actively misleading, so
#' the returned `diagnostics` are part of the result, not debug output.
#'
#' @section Solver dependency:
#' The formulation is deliberately kept inside what [lpSolve::lp()] can express. Terms that
#' would usually be written as quadratic penalties (terminal fit, historic preference,
#' fairness across classes) are implemented as their L1 or minimax equivalents, which are
#' linear.
#'
#' @name trans_rates_solver
#' @seealso [trans_rates_t], [trans_meta_t], [periods_t]
NULL

#' @describeIn trans_rates_solver Derive per-transition minimum and maximum rate bounds
#' from observed history. Rates are annualised before the minimum and maximum are taken and
#' then re-inflated to the step length of the extrapolated periods, because observed
#' intervals are typically irregular. Transitions absent from `obs_rates` in a given period
#' count as a rate of 0 for that period, not as missing -- otherwise `min_rate` is biased
#' upward.
#'
#' @param obs_rates A [trans_rates_t] table of observed rates for a single `id_run`, as
#' returned by `evoland_db$get_obs_trans_rates()`.
#' @param periods A [periods_t] table, used for the true interval lengths.
#' @param trans_meta A [trans_meta_t] table, defining the full edge set and its viability.
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

#' @describeIn trans_rates_solver Reachability precheck: the maximum and minimum area each
#' class can attain at the horizon under mass balance and *hard* historic maximum rates,
#' ignoring targets. Persistence is left free and minimum rates are not imposed, which
#' makes this the loosest honest question -- "can this class get there at all, given that
#' no edge has ever moved faster than it historically did?". It is the same program
#' [solve_trans_rates()] builds, with the penalty blocks switched off and the bounds made
#' hard; see [trans_rate_lp].
#'
#' @param init_area A data.table with `id_lulc` and `area` (cells at the anchor period).
#' @param bounds Edge bounds as returned by [trans_rate_bounds()].
#' @param n_steps Number of steps to the horizon.
#' @param monotone_sign Optional named or `id_lulc`-ordered vector of `-1`, `0`, `1`
#' constraining the direction of change per class. `NULL` leaves the trajectory free.
#' @return `trans_rate_reachability()` returns a data.table with `id_lulc`, `area_init`,
#' `area_min` and `area_max`, in cells.
#' @export
trans_rate_reachability <- function(init_area, bounds, n_steps, monotone_sign = NULL) {
  stopifnot(
    "n_steps must be a single positive integer" = length(n_steps) == 1L && n_steps >= 1L,
    "bounds needs a max_rate column" = "max_rate" %in% names(bounds)
  )

  ids <- sort(unique(init_area[["id_lulc"]]))
  n_lulc <- length(ids)
  n_step <- as.integer(n_steps)
  init <- as_lulc_area_vector(init_area, ids, NA_real_, "init_area")
  total <- sum(init)
  stopifnot("init_area must contain at least one cell" = total > 0)

  model <- c(
    trans_rate_lp_layout(
      n_lulc,
      n_step,
      blocks = c(
        slack = FALSE,
        shape = FALSE,
        smooth = FALSE,
        target = FALSE,
        historic = FALSE,
        fairness = FALSE
      )
    ),
    list(
      n_lulc = n_lulc,
      n_step = n_step,
      # solved in shares to keep the constraint matrix well conditioned; reported in cells
      init_share = init / total,
      rate = list(max = lulc_pair_matrix(bounds, ids, "max_rate", default = 0, diag_default = 1)),
      forbidden = !lulc_viable_matrix(bounds, ids),
      monotone_sign = align_monotone_sign(monotone_sign, ids)
    )
  )

  problem <- new_lp_problem(model[["n_var"]])
  add_balance_rows(problem, model)
  add_forbidden_rows(problem, model)
  add_rate_bound_rows(problem, model)
  add_monotonicity_rows(problem, model)

  extreme_area <- function(l, direction) {
    objective <- numeric(model[["n_var"]])
    objective[model[["ix"]][["area"]](l, n_step)] <- 1
    solution <- solve_lp_problem(problem, objective, direction)
    stopifnot(
      "the reachability LP is infeasible; check bounds and init_area" = solution[["status"]] == 0L
    )
    solution[["objval"]] * total
  }

  data.table::data.table(
    id_lulc = ids,
    area_init = init,
    area_min = vapply(seq_len(n_lulc), extreme_area, numeric(1L), direction = "min"),
    area_max = vapply(seq_len(n_lulc), extreme_area, numeric(1L), direction = "max")
  )
}

#' @describeIn trans_rates_solver Solve for the per-transition flows that take the
#' landscape from `init_area` to `targets` over the extrapolated periods. Rate bounds are
#' soft, because elicited targets routinely require flows outside the observed envelope;
#' how far outside is reported in the returned diagnostics rather than being suppressed.
#'
#' @param targets A data.table with `id_lulc` and either `area` (cells on the same grid as
#' `init_area`) or `share` (of the landscape, rehydrated against `init_area`).
#' @param shapes A data.table with `id_lulc` and `shape`, one of `"instant growth"`,
#' `"delayed growth"`, `"constant change"`, `"instant decline"`, `"delayed decline"`, or
#' `NULL` for no shape preference. Note that a straight line satisfies every one of these
#' one-sided curvature constraints, so shapes only bind when `shape_strictness > 0`.
#' @param n_steps Number of steps to the horizon; inferred from `periods` when `NULL`.
#' @param lambda_bounds Penalty weight on rate-bound violation.
#' @param mu_shape Penalty weight on trajectory-shape violation.
#' @param mu_smooth Penalty weight on the second difference of the trajectory; a
#' tie-breaker among otherwise equivalent trajectories.
#' @param mu_target Penalty weight on the L1 distance between the solved terminal area and
#' the target. Without it, a hard terminal band is treated as free real estate and every
#' class parks on a band edge.
#' @param mu_historic Penalty weight on the L1 distance between flows and the historic
#' outflow pattern (`ref_rate`). Zero by default; raising it keeps flows near the observed
#' pattern where the target does not force otherwise.
#' @param margin Slack around the rate bounds before a violation is penalised.
#' @param terminal_band Relative half-width of a *hard* band around the terminal target,
#' or `NA` (the default) to rely on `mu_target` alone. A hard band and `forbid_non_viable`
#' together turn an out-of-reach target into an infeasible program rather than a near miss:
#' on the SSP-CH demand that combination is infeasible for three of five scenarios, while
#' the L1 fit lands as close as the viable edge set allows and reports the shortfall.
#' @param shape_strictness Minimum curvature a shaped trajectory must exhibit, as a
#' fraction of the class's mean per-step change. At 0 (the default) the shape constraints
#' are one-sided and a straight line satisfies all of them.
#' @param monotone Whether to hard-constrain each class to move monotonically in the
#' direction of `sign(target - init)`.
#' @param fairness Minimax bound on the worst per-class rate-bound violation. `TRUE` uses
#' `lambda_bounds` as its weight; a number sets the weight explicitly.
#' @param forbid_non_viable Whether to hard-zero flows on transitions that
#' [trans_meta_t] marks as non-viable. Such flows have no `trans_pot_t` rows and would be
#' silently dropped at allocation time, so the trajectory would not materialise.
#' @param max_reachability_ratio Fail if any target asks for more than this multiple of the
#' historically achievable change. Everything below the threshold is reported, not gated.
#' @return `solve_trans_rates()` returns a list with `status`, `objective`, `areas`
#' (`id_lulc`, `step`, `id_period`, `area`), `flows` and `rates` (`id_trans`,
#' `id_lulc_anterior`, `id_lulc_posterior`, `step`, `id_period`, `count`, `rate`), and
#' `diagnostics`.
#' @examples
#' bounds <- data.table::data.table(
#'   id_trans = c(NA, 1L, NA, 2L),
#'   id_lulc_anterior = c(1L, 1L, 2L, 2L),
#'   id_lulc_posterior = c(1L, 2L, 2L, 1L),
#'   min_rate = c(0.9, 0, 0.95, 0),
#'   max_rate = c(1, 0.1, 1, 0.05),
#'   is_viable = TRUE
#' )
#' solution <- solve_trans_rates(
#'   init_area = data.table::data.table(id_lulc = 1:2, area = c(6000, 4000)),
#'   targets = data.table::data.table(id_lulc = 1:2, share = c(0.5, 0.5)),
#'   shapes = NULL,
#'   bounds = bounds,
#'   n_steps = 3L
#' )
#' solution[["areas"]]
#' solution[["diagnostics"]][["reachability"]]
#' @export
solve_trans_rates <- function(
  init_area,
  targets,
  shapes,
  bounds,
  periods = NULL,
  n_steps = NULL,
  lambda_bounds = 0.1,
  mu_shape = 15,
  mu_smooth = 1,
  mu_target = 1e3,
  mu_historic = 0,
  margin = 0.01,
  terminal_band = NA,
  shape_strictness = 0,
  monotone = TRUE,
  fairness = TRUE,
  forbid_non_viable = TRUE,
  max_reachability_ratio = 10
) {
  stopifnot(
    "bounds needs min_rate, max_rate and is_viable columns" = all(
      c("min_rate", "max_rate", "is_viable") %in% names(bounds)
    )
  )

  grid <- trans_rate_time_grid(periods, n_steps)
  scenario <- trans_rate_scenario(init_area, targets, shapes)
  if (!isTRUE(monotone)) {
    scenario[["monotone_sign"]] <- NULL
  }

  reachability <-
    trans_rate_reachability(init_area, bounds, grid[["n_step"]]) |>
    reachability_verdict(scenario[["target"]])
  assert_reachable_targets(reachability, max_reachability_ratio)

  model <- trans_rate_lp_model(
    scenario,
    grid,
    bounds,
    params = list(
      lambda_bounds = lambda_bounds,
      mu_shape = mu_shape,
      mu_smooth = mu_smooth,
      mu_target = mu_target,
      mu_historic = mu_historic,
      margin = margin,
      terminal_band = terminal_band,
      shape_strictness = shape_strictness,
      fairness = fairness,
      forbid_non_viable = forbid_non_viable
    )
  )

  solution <- solve_lp_problem(
    trans_rate_lp_problem(model),
    trans_rate_lp_objective(model),
    "min"
  )
  if (solution[["status"]] != 0L) {
    stop(glue::glue(
      "The transition rate LP has no solution (lpSolve status {solution[['status']]}). ",
      "A hard terminal_band on an out-of-reach target is the usual cause; ",
      "set terminal_band = NA to fall back on the mu_target penalty, ",
      "or inspect trans_rate_reachability()."
    ))
  }

  tables <- trans_rate_lp_tables(solution[["solution"]], model)
  list(
    status = solution[["status"]],
    objective = solution[["objval"]],
    areas = tables[["areas"]][, .(id_lulc, step, id_period, area)],
    flows = tables[["flows"]][,
      .(id_trans, id_lulc_anterior, id_lulc_posterior, step, id_period, flow, count, is_viable)
    ],
    rates = tables[["flows"]][,
      .(id_trans, id_lulc_anterior, id_lulc_posterior, step, id_period, count, rate, is_viable)
    ],
    diagnostics = trans_rate_lp_diagnostics(tables, solution[["solution"]], model, reachability)
  )
}

#' @describeIn trans_rates_solver Turn a solution into a [trans_rates_t] table. Flows on the
#' diagonal and on non-viable transitions are dropped, since neither has an `id_trans` or
#' any `trans_pot_t` rows to be allocated against; non-viable flow is asserted to be zero
#' rather than filtered away silently.
#'
#' @param solution A list as returned by [solve_trans_rates()].
#' @param id_run One or more `id_run` values to write the same solution to. Scenario demand
#' is usually shared by several runs, for instance across climate variants.
#' @param tolerance Cells of flow on non-viable transitions to tolerate before failing.
#' @return `trans_rates_from_solution()` returns a [trans_rates_t] table.
#' @export
trans_rates_from_solution <- function(solution, id_run, tolerance = 1) {
  rates <- data.table::as.data.table(solution[["rates"]])

  stopifnot(
    "solution rates lack id_period; solve with a periods_t" = !anyNA(rates[["id_period"]]),
    "flow was allocated to non-viable transitions; solve with forbid_non_viable = TRUE" = rates[
      is_viable == FALSE,
      sum(count)
    ] <=
      tolerance
  )

  viable <- rates[is_viable == TRUE & !is.na(id_trans)]
  as.integer(id_run) |>
    lapply(\(this_run) {
      data.table::data.table(
        id_run = this_run,
        id_period = viable[["id_period"]],
        id_trans = viable[["id_trans"]],
        count = viable[["count"]],
        rate = viable[["rate"]]
      )
    }) |>
    data.table::rbindlist() |>
    as_trans_rates_t()
}

#' @describeIn trans_rates_solver Replay a rate table forward from an initial state to
#' recover the class-area trajectory it implies. Transitions not present in `rates` are
#' taken to be zero, so the residual `1 - sum(rate)` of each class persists. This is what
#' makes a solved trajectory recoverable from [trans_rates_t] alone, and therefore
#' comparable against the areas an allocation run actually realised.
#'
#' @param rates A [trans_rates_t] table for a single `id_run`.
#' @return `trans_rate_areas()` returns a data.table with `id_lulc`, `id_period` and
#' `area`; `id_period` is the period whose *state* the area describes, so the initial state
#' carries the period preceding the first one in `rates`.
#' @export
trans_rate_areas <- function(init_area, rates, trans_meta) {
  stopifnot(
    inherits(trans_meta, "trans_meta_t"),
    "rates must contain exactly one id_run" = !("id_run" %in% names(rates)) ||
      length(unique(rates[["id_run"]])) == 1L
  )

  ids <- sort(unique(init_area[["id_lulc"]]))
  area <- as_lulc_area_vector(init_area, ids, NA_real_, "init_area")

  edges <- merge(
    data.table::as.data.table(rates)[, .(id_trans, id_period, rate)],
    data.table::as.data.table(trans_meta)[, .(id_trans, id_lulc_anterior, id_lulc_posterior)],
    by = "id_trans"
  )
  step_periods <- sort(unique(edges[["id_period"]]))

  trajectory <- vector("list", length(step_periods) + 1L)
  trajectory[[1L]] <- data.table::data.table(
    id_lulc = ids,
    id_period = min(step_periods) - 1L,
    area = area
  )

  for (i in seq_along(step_periods)) {
    step_rate <- lulc_pair_matrix(
      edges[id_period == step_periods[i]],
      ids,
      "rate",
      default = 0,
      diag_default = 0
    )
    outflow_rate <- rowSums(step_rate)
    stopifnot(
      "outflow rates sum above 1 for some class and period" = all(outflow_rate <= 1 + 1e-9)
    )
    diag(step_rate) <- 1 - outflow_rate
    area <- as.numeric(crossprod(step_rate, area))
    trajectory[[i + 1L]] <- data.table::data.table(
      id_lulc = ids,
      id_period = step_periods[i],
      area = area
    )
  }

  data.table::rbindlist(trajectory)
}

#' Preparing the inputs of the transition rate solver
#'
#' Unit conversions and table reshaping that happen before any program is built: rescaling
#' rates between interval lengths, resolving the time grid, aligning per-class vectors and
#' matrices to one class ordering, and comparing targets against the reachable band. The
#' program itself is in [trans_rate_lp].
#'
#' @name trans_rate_inputs
#' @keywords internal
NULL

#' @describeIn trans_rate_inputs Rescale a transition rate between interval lengths.
#' Transition rates are survival-style quantities: the complement `1 - rate` compounds
#' multiplicatively, so a rate observed over 12 years is not comparable to one over 10.
#' Persistence (a class staying itself) is the complement of the total outflow and
#' therefore compounds directly.
#'
#' @param rate Numeric vector of rates in `[0, 1]`.
#' @param from_years,to_years Interval lengths in years.
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

#' @describeIn trans_rate_inputs Interval length in years covered by each period's
#' transition. The rate at period `p` describes the transition from the state at `p - 1` to
#' the state at `p`, so the interval that matters is the spacing between consecutive period
#' anchors, not the length of a single period. Period 0 (static phenomena) is ignored.
#'
#' @return `period_interval_years()` returns a data.table with `id_period` and
#' `interval_years`, the latter `NA` for the first period.
#' @keywords internal
period_interval_years <- function(periods) {
  stopifnot(inherits(periods, "periods_t"))

  anchors <- data.table::as.data.table(periods)[id_period > 0L]
  data.table::setorder(anchors, id_period)
  stopifnot("periods_t needs at least two periods" = nrow(anchors) >= 2L)

  days <- as.numeric(difftime(
    anchors[["start_date"]][-1L],
    anchors[["start_date"]][-nrow(anchors)],
    units = "days"
  ))

  data.table::data.table(
    id_period = anchors[["id_period"]],
    interval_years = c(NA_real_, days / 365.25)
  )
}

#' @describeIn trans_rate_inputs Resolve how many steps to solve for, how long each is and
#' which `id_period` each belongs to. Either a [periods_t] or a plain step count is given;
#' without periods the steps are taken to be of equal length, which only matters for the
#' curvature of the trajectory.
#'
#' @return `trans_rate_time_grid()` returns a list of `n_step`, `step_years` and
#' `id_periods` (`NULL` when no [periods_t] was given).
#' @keywords internal
trans_rate_time_grid <- function(periods = NULL, n_steps = NULL) {
  stopifnot(
    "pass either periods or n_steps" = !is.null(periods) || !is.null(n_steps),
    "pass only one of periods and n_steps" = is.null(periods) || is.null(n_steps)
  )

  if (is.null(periods)) {
    n_step <- as.integer(n_steps)
    stopifnot("n_steps must be at least 1" = n_step >= 1L)
    return(list(n_step = n_step, step_years = rep(1, n_step), id_periods = NULL))
  }

  intervals <- period_interval_years(periods)
  future <- data.table::as.data.table(periods)[is_extrapolated == TRUE]
  data.table::setorder(future, id_period)
  id_periods <- future[["id_period"]]
  step_years <- intervals[match(id_periods, id_period)][["interval_years"]]

  stopifnot(
    "periods contains no extrapolated periods" = length(id_periods) >= 1L,
    "an extrapolated period has no predecessor to measure its length against" = !anyNA(step_years)
  )
  list(n_step = length(id_periods), step_years = step_years, id_periods = id_periods)
}

#' @describeIn trans_rate_inputs Align the initial state, the targets and the elicited
#' shapes to one class ordering, and check that the targets conserve area.
#'
#' @return `trans_rate_scenario()` returns a list of `ids`, `total`, `init`, `target`,
#' `init_share`, `target_share`, `shape` and `monotone_sign`.
#' @keywords internal
trans_rate_scenario <- function(init_area, targets, shapes) {
  ids <- sort(unique(init_area[["id_lulc"]]))
  init <- as_lulc_area_vector(init_area, ids, NA_real_, "init_area")
  total <- sum(init)
  stopifnot("init_area must contain at least one cell" = total > 0)

  target <- as_lulc_area_vector(targets, ids, total, "targets")
  stopifnot(
    "targets must sum to the total area of init_area; state them as shares to rehydrate" = abs(
      sum(target) - total
    ) <=
      1e-6 * total
  )

  list(
    ids = ids,
    total = total,
    init = init,
    target = target,
    # absolutes and shares are the same program up to a scalar, but the share version spans
    # six fewer orders of magnitude
    init_share = init / total,
    target_share = target / total,
    shape = canonical_shapes(shapes, ids),
    monotone_sign = sign(target - init)
  )
}

#' @describeIn trans_rate_inputs Coerce an area or share table to a vector aligned with
#' `ids`. Targets stated as a share of the landscape are grid-independent and therefore
#' portable; targets stated in cells are only meaningful on the grid they were elicited on.
#' Shares are rehydrated against the total area of `init_area`.
#'
#' @param x A data.table with `id_lulc` and either `area` or `share`.
#' @param ids Sorted vector of class ids.
#' @param total Total landscape area in cells.
#' @param what Name of the argument, for error messages.
#' @return `as_lulc_area_vector()` returns areas in cells, aligned with `ids`.
#' @keywords internal
as_lulc_area_vector <- function(x, ids, total, what) {
  stopifnot(
    "table must have an id_lulc column" = "id_lulc" %in% names(x),
    "table needs either an area or a share column" = any(c("area", "share") %in% names(x)),
    "id_lulc values must be unique" = !anyDuplicated(x[["id_lulc"]]),
    "id_lulc values must match those of init_area" = setequal(x[["id_lulc"]], ids)
  )

  ordered <- x[match(ids, x[["id_lulc"]]), ]

  if ("share" %in% names(ordered)) {
    shares <- as.numeric(ordered[["share"]])
    stopifnot(
      "shares must be non-negative" = all(shares >= 0),
      "init_area must be stated in cells, not shares" = !is.na(total)
    )
    if (abs(sum(shares) - 1) > 1e-6) {
      warning(sprintf("%s shares sum to %.6f, renormalising to 1", what, sum(shares)))
    }
    return(shares / sum(shares) * total)
  }

  areas <- as.numeric(ordered[["area"]])
  stopifnot("areas must be non-negative" = all(areas >= 0))
  areas
}

#' @describeIn trans_rate_inputs Build a class-by-class matrix from an edge table.
#'
#' @param edges A data.table keyed by `id_lulc_anterior` and `id_lulc_posterior`.
#' @param value_col Column of `edges` to place in the matrix.
#' @param default Value for pairs absent from `edges`.
#' @param diag_default Value for absent diagonal entries; defaults to `default`.
#' @return `lulc_pair_matrix()` returns a `length(ids)` square matrix.
#' @keywords internal
lulc_pair_matrix <- function(edges, ids, value_col, default, diag_default = default) {
  out <- matrix(default, nrow = length(ids), ncol = length(ids))
  diag(out) <- diag_default

  anterior <- match(edges[["id_lulc_anterior"]], ids)
  posterior <- match(edges[["id_lulc_posterior"]], ids)
  known <- !is.na(anterior) & !is.na(posterior)
  out[cbind(anterior[known], posterior[known])] <- edges[[value_col]][known]
  out
}

#' @describeIn trans_rate_inputs Which transitions may carry flow at all. Edge tables
#' without an `is_viable` column place no restriction; persistence is always allowed,
#' because without it a class could not stay itself.
#'
#' @return `lulc_viable_matrix()` returns a logical square matrix.
#' @keywords internal
lulc_viable_matrix <- function(bounds, ids) {
  out <- if ("is_viable" %in% names(bounds)) {
    lulc_pair_matrix(bounds, ids, "is_viable", default = FALSE, diag_default = TRUE)
  } else {
    matrix(TRUE, nrow = length(ids), ncol = length(ids))
  }
  diag(out) <- TRUE
  out
}

#' @describeIn trans_rate_inputs Normalise elicited trajectory shape labels and align them
#' with `ids`.
#'
#' @return `canonical_shapes()` returns canonical shape labels, `NA` where unset.
#' @keywords internal
canonical_shapes <- function(shapes, ids) {
  known <- c(
    "instant growth",
    "delayed growth",
    "constant change",
    "instant decline",
    "delayed decline"
  )
  if (is.null(shapes)) {
    return(rep(NA_character_, length(ids)))
  }

  stopifnot(
    "shapes needs columns id_lulc and shape" = all(c("id_lulc", "shape") %in% names(shapes)),
    "shapes must not contain unknown id_lulc values" = all(shapes[["id_lulc"]] %in% ids)
  )

  labels <- tolower(trimws(as.character(shapes[["shape"]])))
  labels[labels %in% c("", "na", "none")] <- NA_character_
  unknown <- setdiff(labels[!is.na(labels)], known)
  stopifnot(
    "unknown shape labels" = length(unknown) == 0L
  )

  labels[match(ids, shapes[["id_lulc"]])]
}

#' @describeIn trans_rate_inputs Align a per-class direction of change with `ids`, whether
#' it is named or already in class order.
#'
#' @return `align_monotone_sign()` returns the signs in class order, or `NULL`.
#' @keywords internal
align_monotone_sign <- function(monotone_sign, ids) {
  if (is.null(monotone_sign)) {
    return(NULL)
  }
  if (!is.null(names(monotone_sign))) {
    monotone_sign <- monotone_sign[match(as.character(ids), names(monotone_sign))]
  }
  stopifnot(
    "monotone_sign must have one entry per class" = length(monotone_sign) == length(ids)
  )
  monotone_sign
}

#' @describeIn trans_rate_inputs Compare targets against the reachable band. A class that
#' cannot move at all in the direction asked is unreachable outright, not unreachable by a
#' very large multiple.
#'
#' @param reachability Output of [trans_rate_reachability()].
#' @param target Numeric vector of target areas, aligned with `reachability$id_lulc`.
#' @return `reachability_verdict()` returns `reachability` with `target`, `asked`,
#' `achievable`, `ratio` and `verdict`.
#' @keywords internal
reachability_verdict <- function(reachability, target) {
  out <- data.table::copy(reachability)
  out[, target := target]
  out[, asked := target - area_init]
  out[, achievable := data.table::fifelse(asked >= 0, area_max - area_init, area_min - area_init)]

  immovable <- 1e-6 * pmax(out[["area_init"]], 1)
  out[,
    ratio := data.table::fifelse(
      abs(asked) <= abs(achievable) + immovable,
      1,
      data.table::fifelse(abs(achievable) < immovable, Inf, abs(asked) / abs(achievable))
    )
  ]
  out[,
    verdict := cut(
      ratio,
      breaks = c(-Inf, 1 + 1e-9, 1.5, 3, Inf),
      labels = c("reachable", "near the edge", "outside history", "far outside history")
    )
  ]
  out[]
}

#' @describeIn trans_rate_inputs Fail on targets far beyond what history supports. The
#' precheck quantifies and reports; it gates only above `max_ratio`, because scenario
#' targets are normative and a precheck that failed on every departure from observed
#' dynamics would abort every scenario.
#'
#' @param max_ratio The `max_reachability_ratio` of [solve_trans_rates()].
#' @keywords internal
assert_reachable_targets <- function(reachability, max_ratio) {
  over_threshold <- reachability[ratio > max_ratio]
  if (nrow(over_threshold) == 0L) {
    return(invisible(reachability))
  }

  stop(glue::glue(
    "Targets for id_lulc {toString(over_threshold[['id_lulc']])} ",
    "ask for {toString(round(over_threshold[['ratio']], 2L))}",
    "times the historically achievable change. ",
    "This is above the current max_reachability_ratio = {max_ratio};",
    "see trans_rate_reachability()"
  ))
}
