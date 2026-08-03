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
#' classes are not ignored.
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

#' Rescale a transition rate between interval lengths.
#'
#' Transition rates are survival-style quantities: the complement `1 - rate` compounds
#' multiplicatively, so a rate observed over 12 years is not comparable to one over 10.
#' Persistence (a class staying itself) is the complement of the total outflow and
#' therefore compounds directly.
#'
#' @param rate Numeric vector of rates in `[0, 1]`.
#' @param from_years,to_years Interval lengths in years.
#' @param is_persistence Logical, `TRUE` for `i -> i` rates.
#' @return Numeric vector of rates on the `to_years` scale.
#' @keywords internal
rescale_trans_rate <- function(rate, from_years, to_years, is_persistence = FALSE) {
  exponent <- to_years / from_years
  data.table::fifelse(
    is_persistence,
    rate^exponent,
    1 - (1 - rate)^exponent
  )
}

#' Interval length in years covered by each period's transition.
#'
#' The rate at period `p` describes the transition from the state at `p - 1` to the state
#' at `p`, so the interval that matters is the spacing between consecutive period anchors,
#' not the length of a single period. Period 0 (static phenomena) is ignored.
#'
#' @param periods A [periods_t] table.
#' @return A data.table with `id_period` and `interval_years` (`NA` for the first period).
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
#' @param step_years Length of a future step in years. Inferred from the extrapolated
#' periods when `NULL`.
#' @return `trans_rate_bounds()` returns a data.table with `id_trans` (`NA` on the
#' diagonal), `id_lulc_anterior`, `id_lulc_posterior`, `min_rate`, `max_rate`, `ref_rate`
#' (the historic mean, for the optional historic-preference term) and `is_viable`. The
#' step length the bounds apply to is attached as the `step_years` attribute.
#' @export
trans_rate_bounds <- function(
  obs_rates,
  periods,
  trans_meta,
  include_persistence = TRUE,
  step_years = NULL
) {
  stopifnot(
    inherits(obs_rates, "trans_rates_t"),
    inherits(periods, "periods_t"),
    inherits(trans_meta, "trans_meta_t"),
    "obs_rates must contain exactly one id_run" = length(unique(obs_rates[["id_run"]])) == 1L,
    "obs_rates is empty" = nrow(obs_rates) > 0L
  )

  intervals <- period_interval_years(periods)

  if (is.null(step_years)) {
    future_ids <- periods[is_extrapolated == TRUE][["id_period"]]
    future_years <- intervals[id_period %in% future_ids][["interval_years"]]
    stopifnot(
      "cannot infer step_years: `periods` has no extrapolated periods" = length(future_years) >= 1L,
      "cannot infer step_years: the first extrapolated period has no predecessor" = !anyNA(
        future_years
      ),
      # leap years make nominally equal steps differ by a fraction of a percent
      "cannot infer step_years: extrapolated periods differ in length, pass step_years" = diff(range(
        future_years
      )) <=
        0.05 * mean(future_years)
    )
    step_years <- mean(future_years)
  }
  stopifnot(
    "step_years must be a single positive number" = length(step_years) == 1L &&
      is.finite(step_years) &&
      step_years > 0
  )

  obs_periods <- sort(unique(obs_rates[["id_period"]]))
  obs_periods <- obs_periods[obs_periods > 0L]

  # complete the grid: an edge absent in a period transitioned at rate 0, not at NA
  rates <- merge(
    data.table::CJ(id_trans = trans_meta[["id_trans"]], id_period = obs_periods),
    data.table::as.data.table(obs_rates)[, .(id_trans, id_period, rate)],
    by = c("id_trans", "id_period"),
    all.x = TRUE
  )
  rates[is.na(rate), rate := 0]
  rates <- merge(
    rates,
    data.table::as.data.table(trans_meta)[,
      .(id_trans, id_lulc_anterior, id_lulc_posterior, is_viable)
    ],
    by = "id_trans"
  )
  rates[intervals, interval_years := i.interval_years, on = "id_period"]
  rates[, is_persistence := FALSE]
  stopifnot(
    "observed periods without an interval length" = !anyNA(rates[["interval_years"]])
  )

  if (isTRUE(include_persistence)) {
    persistence <- rates[,
      .(rate = 1 - sum(rate), id_trans = NA_integer_, is_viable = TRUE, is_persistence = TRUE),
      by = .(id_lulc_anterior, id_period, interval_years)
    ]
    stopifnot(
      "outflow rates sum above 1 for some class and period" = all(persistence[["rate"]] > -1e-9)
    )
    persistence[, rate := pmax(rate, 0)]
    persistence[, id_lulc_posterior := id_lulc_anterior]
    rates <- rbind(rates, persistence, use.names = TRUE)
  }

  rates[,
    rate_annual := rescale_trans_rate(rate, interval_years, 1, is_persistence)
  ]

  bounds <- rates[,
    .(
      min_rate = rescale_trans_rate(min(rate_annual), 1, step_years, is_persistence[1L]),
      max_rate = rescale_trans_rate(max(rate_annual), 1, step_years, is_persistence[1L]),
      ref_rate = rescale_trans_rate(mean(rate_annual), 1, step_years, is_persistence[1L])
    ),
    by = .(id_trans, id_lulc_anterior, id_lulc_posterior, is_viable)
  ]
  data.table::setorder(bounds, id_lulc_anterior, id_lulc_posterior)
  data.table::setattr(bounds, "step_years", step_years)
  bounds[]
}

#' Build a class-by-class matrix from an edge table.
#'
#' @param edges A data.table keyed by `id_lulc_anterior` and `id_lulc_posterior`.
#' @param ids Sorted vector of class ids defining the matrix layout.
#' @param value_col Column of `edges` to place in the matrix.
#' @param default Value for pairs absent from `edges`.
#' @param diag_default Value for absent diagonal entries; defaults to `default`.
#' @return A `length(ids)` square matrix.
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

#' Sparse linear program builder.
#'
#' [lpSolve::lp()] accepts constraints in triplet form via `dense.const`, which keeps
#' memory linear in the number of non-zero coefficients. It matches constraint rows to
#' `const.dir`/`const.rhs` by the order of the row indices it sees, so every row must carry
#' at least one non-zero coefficient.
#'
#' @param n_var Number of decision variables.
#' @return An environment collecting rows, to be passed to `solve_lp_problem()`.
#' @keywords internal
new_lp_problem <- function(n_var) {
  problem <- new.env(parent = emptyenv())
  problem[["n_var"]] <- n_var
  problem[["cols"]] <- list()
  problem[["vals"]] <- list()
  problem[["dir"]] <- character(0L)
  problem[["rhs"]] <- numeric(0L)
  problem
}

#' @param problem An object from `new_lp_problem()`.
#' @param cols Integer vector of variable indices.
#' @param vals Numeric vector of coefficients, parallel to `cols`.
#' @param dir One of `"<="`, `">="`, `"="`.
#' @param rhs Right-hand side.
#' @rdname new_lp_problem
#' @keywords internal
add_lp_row <- function(problem, cols, vals, dir, rhs) {
  cols <- as.integer(cols)
  vals <- as.numeric(vals)

  if (anyDuplicated(cols)) {
    summed <- rowsum(vals, cols, reorder = TRUE)
    cols <- as.integer(rownames(summed))
    vals <- as.numeric(summed)
  }
  nonzero <- vals != 0
  cols <- cols[nonzero]
  vals <- vals[nonzero]

  stopifnot(
    "a constraint row has no non-zero coefficient" = length(cols) > 0L,
    "a constraint row references an unknown variable" = all(cols >= 1L & cols <= problem[["n_var"]])
  )

  n_row <- length(problem[["cols"]]) + 1L
  problem[["cols"]][[n_row]] <- cols
  problem[["vals"]][[n_row]] <- vals
  problem[["dir"]][n_row] <- dir
  problem[["rhs"]][n_row] <- rhs
  invisible(problem)
}

#' @param objective Numeric vector of objective coefficients, one per variable.
#' @param direction `"min"` or `"max"`.
#' @rdname new_lp_problem
#' @keywords internal
solve_lp_problem <- function(problem, objective, direction = "min") {
  stopifnot(length(objective) == problem[["n_var"]])

  cols <- problem[["cols"]]
  triplets <- cbind(
    rep.int(seq_along(cols), lengths(cols)),
    unlist(cols, use.names = FALSE),
    unlist(problem[["vals"]], use.names = FALSE)
  )

  lpSolve::lp(
    direction = direction,
    objective.in = objective,
    const.dir = problem[["dir"]],
    const.rhs = problem[["rhs"]],
    dense.const = triplets
  )
}

#' Coerce an area or share table to a vector aligned with `ids`.
#'
#' Targets stated as a share of the landscape are grid-independent and therefore portable;
#' targets stated in cells are only meaningful on the grid they were elicited on. Shares
#' are rehydrated against the total area of `init_area`.
#'
#' @param x A data.table with `id_lulc` and either `area` or `share`.
#' @param ids Sorted vector of class ids.
#' @param total Total landscape area in cells.
#' @param what Name of the argument, for error messages.
#' @return Numeric vector of areas in cells, aligned with `ids`.
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

#' @describeIn trans_rates_solver Reachability precheck: the maximum and minimum area each
#' class can attain at the horizon under mass balance and *hard* historic maximum rates,
#' ignoring targets. Persistence is left free and minimum rates are not imposed, which
#' makes this the loosest honest question -- "can this class get there at all, given that
#' no edge has ever moved faster than it historically did?".
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

  # solved in shares to keep the constraint matrix well conditioned; reported in cells
  init_share <- init / total
  max_rate <- lulc_pair_matrix(bounds, ids, "max_rate", default = 0, diag_default = 1)
  if ("is_viable" %in% names(bounds)) {
    viable <- lulc_pair_matrix(bounds, ids, "is_viable", default = FALSE, diag_default = TRUE)
    max_rate[!viable] <- 0
  }

  n_x <- n_lulc * n_lulc * n_step
  n_var <- n_x + n_lulc * (n_step + 1L)
  idx_x <- function(i, j, t) as.integer(t * n_lulc * n_lulc + (i - 1L) * n_lulc + j)
  idx_area <- function(l, t) as.integer(n_x + t * n_lulc + l)

  problem <- new_lp_problem(n_var)

  for (l in seq_len(n_lulc)) {
    add_lp_row(problem, idx_area(l, 0L), 1, "=", init_share[l])
  }
  for (t in seq.int(0L, n_step)) {
    add_lp_row(problem, idx_area(seq_len(n_lulc), t), rep(1, n_lulc), "=", 1)
  }
  for (t in seq.int(0L, n_step - 1L)) {
    for (i in seq_len(n_lulc)) {
      add_lp_row(
        problem,
        c(idx_x(i, seq_len(n_lulc), t), idx_area(i, t)),
        c(rep(1, n_lulc), -1),
        "=",
        0
      )
    }
    for (j in seq_len(n_lulc)) {
      add_lp_row(
        problem,
        c(idx_area(j, t + 1L), idx_x(seq_len(n_lulc), j, t)),
        c(1, rep(-1, n_lulc)),
        "=",
        0
      )
    }
    for (i in seq_len(n_lulc)) {
      for (j in seq_len(n_lulc)) {
        if (i == j) {
          next
        }
        add_lp_row(
          problem,
          c(idx_x(i, j, t), idx_area(i, t)),
          c(1, -max_rate[i, j]),
          "<=",
          0
        )
      }
    }
  }
  if (!is.null(monotone_sign)) {
    signs <- monotone_sign
    if (!is.null(names(signs))) {
      signs <- signs[match(as.character(ids), names(signs))]
    }
    stopifnot("monotone_sign must have one entry per class" = length(signs) == n_lulc)
    for (l in seq_len(n_lulc)) {
      if (is.na(signs[l]) || signs[l] == 0) {
        next
      }
      for (t in seq_len(n_step)) {
        add_lp_row(
          problem,
          c(idx_area(l, t), idx_area(l, t - 1L)),
          c(1, -1),
          if (signs[l] > 0) ">=" else "<=",
          0
        )
      }
    }
  }

  extreme_area <- function(l, direction) {
    objective <- numeric(n_var)
    objective[idx_area(l, n_step)] <- 1
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

#' Compare targets against the reachable band.
#'
#' @param reachability Output of [trans_rate_reachability()].
#' @param target Numeric vector of target areas, aligned with `reachability$id_lulc`.
#' @return `reachability` with `target`, `asked`, `achievable`, `ratio` and `verdict`.
#' @keywords internal
reachability_verdict <- function(reachability, target) {
  out <- data.table::copy(reachability)
  out[, target := target]
  out[, asked := target - area_init]
  out[, achievable := data.table::fifelse(asked >= 0, area_max - area_init, area_min - area_init)]
  # a class that cannot move at all in the direction asked is unreachable outright, not
  # unreachable by a very large multiple
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

#' Normalise elicited trajectory shape labels.
#'
#' @param shapes A data.table with `id_lulc` and `shape`, or `NULL`.
#' @param ids Sorted vector of class ids.
#' @return Character vector of canonical shapes aligned with `ids`, `NA` where unset.
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
    "pass either periods or n_steps" = !is.null(periods) || !is.null(n_steps),
    "pass only one of periods and n_steps" = is.null(periods) || is.null(n_steps),
    "bounds needs min_rate, max_rate and is_viable columns" = all(
      c("min_rate", "max_rate", "is_viable") %in% names(bounds)
    )
  )

  id_periods <- NULL
  step_years <- rep(1, max(n_steps %||% 0L, 1L))
  if (!is.null(periods)) {
    intervals <- period_interval_years(periods)
    future <- data.table::as.data.table(periods)[is_extrapolated == TRUE]
    data.table::setorder(future, id_period)
    id_periods <- future[["id_period"]]
    step_years <- intervals[match(id_periods, id_period)][["interval_years"]]
    stopifnot(
      "periods contains no extrapolated periods" = length(id_periods) >= 1L,
      "an extrapolated period has no predecessor to measure its length against" = !anyNA(step_years)
    )
    n_steps <- length(id_periods)
  }
  n_step <- as.integer(n_steps)
  stopifnot("n_steps must be at least 1" = n_step >= 1L)
  if (length(step_years) != n_step) {
    step_years <- rep(step_years[1L], n_step)
  }

  ids <- sort(unique(init_area[["id_lulc"]]))
  n_lulc <- length(ids)
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

  shape <- canonical_shapes(shapes, ids)
  monotone_sign <- sign(target - init)

  reachability <-
    trans_rate_reachability(init_area, bounds, n_step) |>
    reachability_verdict(target)

  over_threshold <- reachability[ratio > max_reachability_ratio]
  if (nrow(over_threshold) > 0L) {
    stop(glue::glue(
      "Targets for id_lulc {toString(over_threshold[['id_lulc']])} ",
      "ask for {toString(round(over_threshold[['ratio']], 2L))}",
      "times the historically achievable change. ",
      "This is above the current max_reachability_ratio = {max_reachability_ratio};",
      "see trans_rate_reachability()"
    ))
  }

  # the LP is solved in shares of the landscape: absolutes and shares are the same program
  # up to a scalar, but the share version spans six fewer orders of magnitude
  init_share <- init / total
  target_share <- target / total
  weight <- 1 / pmax(init_share, 1e-9)

  min_rate <- lulc_pair_matrix(bounds, ids, "min_rate", default = 0, diag_default = 0)
  max_rate <- lulc_pair_matrix(bounds, ids, "max_rate", default = 0, diag_default = 1)
  ref_rate <- if ("ref_rate" %in% names(bounds)) {
    lulc_pair_matrix(bounds, ids, "ref_rate", default = 0, diag_default = 0)
  } else {
    NULL
  }
  viable <- lulc_pair_matrix(bounds, ids, "is_viable", default = FALSE, diag_default = TRUE)
  diag(viable) <- TRUE
  forbidden <- isTRUE(forbid_non_viable) & !viable

  use_shape <- any(!is.na(shape)) && n_step >= 2L
  use_smooth <- mu_smooth > 0 && n_step >= 2L
  use_target <- mu_target > 0
  use_historic <- mu_historic > 0 && !is.null(ref_rate)
  fair_weight <- if (isTRUE(fairness)) {
    lambda_bounds
  } else if (is.numeric(fairness)) {
    fairness
  } else {
    0
  }
  use_fairness <- fair_weight > 0

  n_x <- n_lulc * n_lulc * n_step
  n_area <- n_lulc * (n_step + 1L)
  n_curve <- if (n_step >= 2L) n_lulc * (n_step - 1L) else 0L

  off_x <- 0L
  off_lower <- off_x + n_x
  off_upper <- off_lower + n_x
  off_area <- off_upper + n_x
  off_shape <- off_area + n_area
  off_smooth <- off_shape + if (use_shape) n_curve else 0L
  off_target <- off_smooth + if (use_smooth) n_curve else 0L
  off_historic <- off_target + if (use_target) 2L * n_lulc else 0L
  off_fair <- off_historic + if (use_historic) n_x else 0L
  n_var <- off_fair + if (use_fairness) 1L else 0L

  flow_offset <- function(i, j, t) as.integer(t * n_lulc * n_lulc + (i - 1L) * n_lulc + j)
  idx_x <- function(i, j, t) as.integer(off_x + flow_offset(i, j, t))
  idx_lower <- function(i, j, t) as.integer(off_lower + flow_offset(i, j, t))
  idx_upper <- function(i, j, t) as.integer(off_upper + flow_offset(i, j, t))
  idx_historic <- function(i, j, t) as.integer(off_historic + flow_offset(i, j, t))
  idx_area <- function(l, t) as.integer(off_area + t * n_lulc + l)
  idx_shape <- function(l, t) as.integer(off_shape + (l - 1L) * (n_step - 1L) + t)
  idx_smooth <- function(l, t) as.integer(off_smooth + (l - 1L) * (n_step - 1L) + t)
  idx_plus <- function(l) as.integer(off_target + l)
  idx_minus <- function(l) as.integer(off_target + n_lulc + l)

  problem <- new_lp_problem(n_var)

  # initial condition and total-area conservation; the latter is redundant given row and
  # column closure, but it is a cheap numerical anchor
  for (l in seq_len(n_lulc)) {
    add_lp_row(problem, idx_area(l, 0L), 1, "=", init_share[l])
  }
  for (t in seq.int(0L, n_step)) {
    add_lp_row(problem, idx_area(seq_len(n_lulc), t), rep(1, n_lulc), "=", 1)
  }

  for (t in seq.int(0L, n_step - 1L)) {
    for (i in seq_len(n_lulc)) {
      add_lp_row(
        problem,
        c(idx_x(i, seq_len(n_lulc), t), idx_area(i, t)),
        c(rep(1, n_lulc), -1),
        "=",
        0
      )
    }
    for (j in seq_len(n_lulc)) {
      add_lp_row(
        problem,
        c(idx_area(j, t + 1L), idx_x(seq_len(n_lulc), j, t)),
        c(1, rep(-1, n_lulc)),
        "=",
        0
      )
    }
  }

  # a forbidden edge is zero, not merely expensive: non-negative flows summing to zero are
  # each zero, so one row per edge suffices
  for (i in seq_len(n_lulc)) {
    for (j in seq_len(n_lulc)) {
      if (!forbidden[i, j]) {
        next
      }
      add_lp_row(
        problem,
        idx_x(i, j, seq.int(0L, n_step - 1L)),
        rep(1, n_step),
        "=",
        0
      )
    }
  }

  for (t in seq.int(0L, n_step - 1L)) {
    for (i in seq_len(n_lulc)) {
      for (j in seq_len(n_lulc)) {
        if (forbidden[i, j]) {
          next
        }

        upper <- max_rate[i, j] + margin
        if (upper < 1) {
          add_lp_row(
            problem,
            c(idx_x(i, j, t), idx_area(i, t), idx_upper(i, j, t)),
            c(1, -upper, -1),
            "<=",
            0
          )
        }
        lower <- min_rate[i, j] - margin
        if (lower > 0) {
          add_lp_row(
            problem,
            c(idx_x(i, j, t), idx_area(i, t), idx_lower(i, j, t)),
            c(-1, lower, -1),
            "<=",
            0
          )
        }
        if (use_historic && i != j) {
          add_lp_row(
            problem,
            c(idx_x(i, j, t), idx_area(i, t), idx_historic(i, j, t)),
            c(1, -ref_rate[i, j], -1),
            "<=",
            0
          )
          add_lp_row(
            problem,
            c(idx_x(i, j, t), idx_area(i, t), idx_historic(i, j, t)),
            c(-1, ref_rate[i, j], -1),
            "<=",
            0
          )
        }
      }
    }
  }

  if (!is.null(terminal_band) && !is.na(terminal_band)) {
    stopifnot("terminal_band must be non-negative" = terminal_band >= 0)
    for (l in seq_len(n_lulc)) {
      add_lp_row(problem, idx_area(l, n_step), 1, ">=", (1 - terminal_band) * target_share[l])
      add_lp_row(problem, idx_area(l, n_step), 1, "<=", (1 + terminal_band) * target_share[l])
    }
  }

  if (use_target) {
    for (l in seq_len(n_lulc)) {
      add_lp_row(
        problem,
        c(idx_area(l, n_step), idx_plus(l), idx_minus(l)),
        c(1, -1, 1),
        "=",
        target_share[l]
      )
    }
  }

  if (isTRUE(monotone)) {
    for (l in seq_len(n_lulc)) {
      if (monotone_sign[l] == 0) {
        next
      }
      for (t in seq_len(n_step)) {
        add_lp_row(
          problem,
          c(idx_area(l, t), idx_area(l, t - 1L)),
          c(1, -1),
          if (monotone_sign[l] > 0) ">=" else "<=",
          0
        )
      }
    }
  }

  # curvature of the trajectory: the per-year change over step t against that over t + 1,
  # scaled by the length of step t
  curvature_row <- function(l, t) {
    ratio <- step_years[t] / step_years[t + 1L]
    list(
      cols = c(idx_area(l, t - 1L), idx_area(l, t), idx_area(l, t + 1L)),
      vals = c(-1, 1 + ratio, -ratio)
    )
  }

  if (use_shape) {
    for (l in seq_len(n_lulc)) {
      if (is.na(shape[l])) {
        next
      }
      growing <- target_share[l] > init_share[l]
      declining <- target_share[l] < init_share[l]
      # a shape elicited against the opposite direction of change says nothing
      applies <- switch(
        shape[l],
        "instant growth" = growing,
        "delayed growth" = growing,
        "instant decline" = declining,
        "delayed decline" = declining,
        "constant change" = TRUE
      )
      if (!applies) {
        next
      }

      strict <- shape_strictness * abs(target_share[l] - init_share[l]) / n_step
      for (t in seq_len(n_step - 1L)) {
        row <- curvature_row(l, t)
        slack <- idx_shape(l, t)
        # "instant" front-loads the change, "delayed" back-loads it
        if (shape[l] %in% c("instant growth", "delayed decline")) {
          add_lp_row(problem, c(row[["cols"]], slack), c(row[["vals"]], 1), ">=", strict)
        }
        if (shape[l] %in% c("delayed growth", "instant decline")) {
          add_lp_row(problem, c(row[["cols"]], slack), c(row[["vals"]], -1), "<=", -strict)
        }
        if (shape[l] == "constant change") {
          add_lp_row(problem, c(row[["cols"]], slack), c(row[["vals"]], 1), ">=", 0)
          add_lp_row(problem, c(row[["cols"]], slack), c(row[["vals"]], -1), "<=", 0)
        }
      }
    }
  }

  if (use_smooth) {
    for (l in seq_len(n_lulc)) {
      for (t in seq_len(n_step - 1L)) {
        cols <- c(idx_area(l, t + 1L), idx_area(l, t), idx_area(l, t - 1L), idx_smooth(l, t))
        add_lp_row(problem, cols, c(1, -2, 1, -1), "<=", 0)
        add_lp_row(problem, cols, c(-1, 2, -1, -1), "<=", 0)
      }
    }
  }

  if (use_fairness) {
    for (i in seq_len(n_lulc)) {
      pairs <- expand.grid(j = seq_len(n_lulc), t = seq.int(0L, n_step - 1L))
      cols <- c(
        idx_lower(i, pairs[["j"]], pairs[["t"]]),
        idx_upper(i, pairs[["j"]], pairs[["t"]]),
        n_var
      )
      add_lp_row(problem, cols, c(rep(weight[i], 2L * nrow(pairs)), -1), "<=", 0)
    }
  }

  objective <- numeric(n_var)
  for (i in seq_len(n_lulc)) {
    pairs <- expand.grid(j = seq_len(n_lulc), t = seq.int(0L, n_step - 1L))
    objective[idx_lower(i, pairs[["j"]], pairs[["t"]])] <- lambda_bounds * weight[i]
    objective[idx_upper(i, pairs[["j"]], pairs[["t"]])] <- lambda_bounds * weight[i]
    if (use_historic) {
      objective[idx_historic(i, pairs[["j"]], pairs[["t"]])] <- mu_historic * weight[i]
    }
  }
  for (l in seq_len(n_lulc)) {
    if (use_shape) {
      objective[idx_shape(l, seq_len(n_step - 1L))] <- mu_shape * weight[l]
    }
    if (use_smooth) {
      objective[idx_smooth(l, seq_len(n_step - 1L))] <- mu_smooth * weight[l]
    }
    if (use_target) {
      objective[c(idx_plus(l), idx_minus(l))] <- mu_target * weight[l]
    }
  }
  if (use_fairness) {
    objective[n_var] <- fair_weight
  }

  solution <- solve_lp_problem(problem, objective, "min")
  if (solution[["status"]] != 0L) {
    stop(
      "the transition rate LP has no solution (lpSolve status ",
      solution[["status"]],
      "). A hard terminal_band on an out-of-reach target is the usual cause; ",
      "set terminal_band = NA to fall back on the mu_target penalty, ",
      "or inspect trans_rate_reachability()."
    )
  }
  values <- solution[["solution"]]

  areas <- data.table::CJ(step = seq.int(0L, n_step), id_lulc = ids, sorted = FALSE)
  areas[, area := values[idx_area(match(id_lulc, ids), step)] * total]

  flows <- data.table::CJ(
    step = seq_len(n_step),
    id_lulc_anterior = ids,
    id_lulc_posterior = ids,
    sorted = FALSE
  )
  flows[,
    c("row", "col") := .(match(id_lulc_anterior, ids), match(id_lulc_posterior, ids))
  ]
  flows[, flow := values[idx_x(row, col, step - 1L)] * total]
  flows[, dev_lower := values[idx_lower(row, col, step - 1L)] * total]
  flows[, dev_upper := values[idx_upper(row, col, step - 1L)] * total]
  flows[, area_anterior := values[idx_area(row, step - 1L)] * total]
  flows[, rate := data.table::fifelse(area_anterior > 1e-9, flow / area_anterior, 0)]
  flows[, count := round(flow)]

  if (!is.null(id_periods)) {
    areas[, id_period := c(NA_integer_, id_periods)[step + 1L]]
    flows[, id_period := id_periods[step]]
  } else {
    areas[, id_period := NA_integer_]
    flows[, id_period := NA_integer_]
  }

  id_trans <- if ("id_trans" %in% names(bounds)) {
    lulc_pair_matrix(bounds, ids, "id_trans", default = NA_integer_, diag_default = NA_integer_)
  } else {
    matrix(NA_integer_, n_lulc, n_lulc)
  }
  flows[, id_trans := id_trans[cbind(row, col)]]
  flows[, is_viable := viable[cbind(row, col)]]
  # measured against the historic envelope itself, not against the margin around it: the
  # margin is a modelling convenience, and on a large class it is a lot of unremarked flow
  flows[, max_flow := max_rate[cbind(row, col)] * area_anterior]
  flows[, min_flow := min_rate[cbind(row, col)] * area_anterior]
  flows[, above_max_rate := flow > max_flow + 1e-6]
  flows[, below_min_rate := flow < min_flow - 1e-6]

  final_area <- areas[step == n_step][match(ids, id_lulc)][["area"]]

  diagnostics <- list(
    reachability = reachability,
    target_error = data.table::data.table(
      id_lulc = ids,
      area_init = init,
      area_final = final_area,
      target = target,
      error = final_area - target,
      pct_error = 100 * (final_area - target) / pmax(target, 1)
    ),
    bound_violation = flows[
      dev_lower > 1e-6 | dev_upper > 1e-6,
      .(id_trans, id_lulc_anterior, id_lulc_posterior, step, id_period, dev_lower, dev_upper)
    ],
    flow_summary = data.table::data.table(
      total_flow = flows[id_lulc_anterior != id_lulc_posterior, sum(flow)],
      # flow the historic envelope does not support, and by how much it overshoots
      flow_above_max_rate = flows[
        id_lulc_anterior != id_lulc_posterior & above_max_rate == TRUE,
        sum(flow)
      ],
      excess_above_max_rate = flows[
        id_lulc_anterior != id_lulc_posterior,
        sum(pmax(flow - max_flow, 0))
      ],
      shortfall_below_min_rate = flows[
        id_lulc_anterior != id_lulc_posterior,
        sum(pmax(min_flow - flow, 0))
      ],
      flow_non_viable = flows[is_viable == FALSE, sum(flow)]
    ),
    shape = data.table::data.table(
      id_lulc = ids,
      shape = shape,
      curvature_slack = if (use_shape) {
        vapply(
          seq_len(n_lulc),
          \(l) sum(values[idx_shape(l, seq_len(n_step - 1L))]) * total,
          numeric(1L)
        )
      } else {
        rep(NA_real_, n_lulc)
      }
    )
  )

  list(
    status = solution[["status"]],
    objective = solution[["objval"]],
    areas = areas[, .(id_lulc, step, id_period, area)],
    flows = flows[,
      .(id_trans, id_lulc_anterior, id_lulc_posterior, step, id_period, flow, count, is_viable)
    ],
    rates = flows[,
      .(id_trans, id_lulc_anterior, id_lulc_posterior, step, id_period, count, rate, is_viable)
    ],
    diagnostics = diagnostics
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
  n_lulc <- length(ids)
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
