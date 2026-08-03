#' Building and solving the transition rate linear program
#'
#' Internals of [solve_trans_rates()], separated so that each block of the program can be
#' read on its own. The pieces are, in the order they are used: a sparse problem builder
#' ([new_lp_problem()]), a variable layout ([trans_rate_lp_layout()]), a model object
#' gathering everything the constraint blocks need ([trans_rate_lp_model()]), one function
#' per block of constraint rows, the objective, and the extraction of the solved vector
#' back into tables.
#'
#' @section Variable layout:
#' All variables are non-negative, and all areas and flows are shares of the landscape.
#' Optional blocks are only allocated when the corresponding term is switched on.
#'
#' | block | count | meaning |
#' | --- | --- | --- |
#' | `x[i, j, t]` | `L^2 * n_step` | flow from class `i` to class `j` during step `t` |
#' | `lower[i, j, t]`, `upper[i, j, t]` | `L^2 * n_step` each | rate-bound violation |
#' | `area[l, t]` | `L * (n_step + 1)` | area of class `l` at time point `t` |
#' | `shape[l, t]`, `smooth[l, t]` | `L * (n_step - 1)` each | curvature violation |
#' | `plus[l]`, `minus[l]` | `L` each | terminal fit, above and below the target |
#' | `historic[i, j, t]` | `L^2 * n_step` | distance from the historic outflow pattern |
#' | `fair` | 1 | minimax bound on the worst per-class violation |
#'
#' @section The model object:
#' A plain list, so that the constraint blocks take one argument rather than fifteen. It
#' carries the scenario (`ids`, `init_share`, `target_share`, `shape`, `monotone_sign`,
#' `total`), the time grid (`n_step`, `step_years`, `id_periods`), the rate matrices
#' (`rate`, `viable`, `forbidden`, `id_trans`), the layout (`n_var`, `ix`, `blocks`) and the
#' tuning parameters (`params`, `weight`, `fair_weight`). [trans_rate_reachability()]
#' assembles a reduced model of the same shape, which is why the balance, bound and
#' monotonicity blocks are shared between the precheck and the solver.
#'
#' @name trans_rate_lp
#' @keywords internal
NULL

#' @describeIn trans_rate_lp Start an empty sparse program. [lpSolve::lp()] accepts
#' constraints in triplet form via `dense.const`, which keeps memory linear in the number of
#' non-zero coefficients. It matches constraint rows to `const.dir`/`const.rhs` by the order
#' of the row indices it sees, so every row must carry at least one non-zero coefficient.
#' The returned object is an environment, so the constraint blocks can add to it in place.
#'
#' @param n_var Number of decision variables.
#' @return `new_lp_problem()` returns an environment collecting constraint rows.
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

#' @describeIn trans_rate_lp Add one constraint row, summing duplicated and dropping zero
#' coefficients.
#'
#' @param problem An object from [new_lp_problem()].
#' @param cols Integer vector of variable indices.
#' @param vals Numeric vector of coefficients, parallel to `cols`.
#' @param dir One of `"<="`, `">="`, `"="`.
#' @param rhs Right-hand side.
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
    "a constraint row references an unknown variable" = all(
      cols >= 1L & cols <= problem[["n_var"]]
    )
  )

  n_row <- length(problem[["cols"]]) + 1L
  problem[["cols"]][[n_row]] <- cols
  problem[["vals"]][[n_row]] <- vals
  problem[["dir"]][n_row] <- dir
  problem[["rhs"]][n_row] <- rhs
  invisible(problem)
}

#' @describeIn trans_rate_lp Hand the collected rows to [lpSolve::lp()].
#'
#' @param objective Numeric vector of objective coefficients, one per variable.
#' @param direction `"min"` or `"max"`.
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

#' @describeIn trans_rate_lp Lay the decision variables out in one contiguous vector and
#' return the index function of each block. Blocks that are switched off are not allocated
#' and their index function is `NULL`, so reaching for one is an error rather than a silent
#' collision with the next block.
#'
#' @param n_lulc Number of land use classes.
#' @param n_step Number of steps to the horizon.
#' @param blocks Named logical vector switching the optional variable blocks on: `slack`,
#' `shape`, `smooth`, `target`, `historic`, `fairness`.
#' @return `trans_rate_lp_layout()` returns a list with `n_var`, the `blocks` it was given
#' and `ix`, a list of index functions.
#' @keywords internal
trans_rate_lp_layout <- function(n_lulc, n_step, blocks) {
  n_x <- n_lulc * n_lulc * n_step
  n_area <- n_lulc * (n_step + 1L)
  n_curve <- if (n_step >= 2L) n_lulc * (n_step - 1L) else 0L
  sized <- function(block, size) if (isTRUE(blocks[[block]])) as.integer(size) else 0L

  off_x <- 0L
  off_lower <- off_x + n_x
  off_upper <- off_lower + sized("slack", n_x)
  off_area <- off_upper + sized("slack", n_x)
  off_shape <- off_area + n_area
  off_smooth <- off_shape + sized("shape", n_curve)
  off_target <- off_smooth + sized("smooth", n_curve)
  off_historic <- off_target + sized("target", 2L * n_lulc)
  off_fair <- off_historic + sized("historic", n_x)
  n_var <- off_fair + sized("fairness", 1L)

  flow <- function(offset) {
    function(i, j, t) as.integer(offset + t * n_lulc * n_lulc + (i - 1L) * n_lulc + j)
  }
  curve <- function(offset) {
    function(l, t) as.integer(offset + (l - 1L) * (n_step - 1L) + t)
  }
  only_if <- function(block, index_fun) if (isTRUE(blocks[[block]])) index_fun else NULL

  list(
    n_var = n_var,
    blocks = blocks,
    ix = list(
      x = flow(off_x),
      lower = only_if("slack", flow(off_lower)),
      upper = only_if("slack", flow(off_upper)),
      area = function(l, t) as.integer(off_area + t * n_lulc + l),
      shape = only_if("shape", curve(off_shape)),
      smooth = only_if("smooth", curve(off_smooth)),
      plus = only_if("target", function(l) as.integer(off_target + l)),
      minus = only_if("target", function(l) as.integer(off_target + n_lulc + l)),
      historic = only_if("historic", flow(off_historic)),
      fair = only_if("fairness", function() as.integer(off_fair + 1L))
    )
  )
}

#' @describeIn trans_rate_lp Gather the scenario, the time grid, the rate matrices and the
#' tuning parameters into the single object the constraint blocks read from.
#'
#' @param scenario A list from `trans_rate_scenario()`.
#' @param grid A list from `trans_rate_time_grid()`.
#' @param bounds Edge bounds as returned by [trans_rate_bounds()].
#' @param params A list of the tuning arguments of [solve_trans_rates()].
#' @keywords internal
trans_rate_lp_model <- function(scenario, grid, bounds, params) {
  ids <- scenario[["ids"]]
  n_lulc <- length(ids)
  n_step <- grid[["n_step"]]

  rate <- list(
    min = lulc_pair_matrix(bounds, ids, "min_rate", default = 0, diag_default = 0),
    max = lulc_pair_matrix(bounds, ids, "max_rate", default = 0, diag_default = 1),
    ref = if ("ref_rate" %in% names(bounds)) {
      lulc_pair_matrix(bounds, ids, "ref_rate", default = 0, diag_default = 0)
    }
  )
  viable <- lulc_viable_matrix(bounds, ids)

  fair_weight <- if (isTRUE(params[["fairness"]])) {
    params[["lambda_bounds"]]
  } else if (is.numeric(params[["fairness"]])) {
    params[["fairness"]]
  } else {
    0
  }

  blocks <- c(
    slack = TRUE,
    shape = any(!is.na(scenario[["shape"]])) && n_step >= 2L,
    smooth = params[["mu_smooth"]] > 0 && n_step >= 2L,
    target = params[["mu_target"]] > 0,
    historic = params[["mu_historic"]] > 0 && !is.null(rate[["ref"]]),
    fairness = fair_weight > 0
  )

  c(
    scenario,
    grid,
    trans_rate_lp_layout(n_lulc, n_step, blocks),
    list(
      n_lulc = n_lulc,
      rate = rate,
      viable = viable,
      forbidden = isTRUE(params[["forbid_non_viable"]]) & !viable,
      id_trans = if ("id_trans" %in% names(bounds)) {
        lulc_pair_matrix(bounds, ids, "id_trans", default = NA_integer_, diag_default = NA_integer_)
      } else {
        matrix(NA_integer_, n_lulc, n_lulc)
      },
      params = params,
      fair_weight = fair_weight,
      weight = 1 / pmax(scenario[["init_share"]], 1e-9)
    )
  )
}

#' @describeIn trans_rate_lp Add the initial condition, total-area conservation and the row
#' and column closure of the flows. Total-area conservation is redundant given closure in
#' both directions, but it is a cheap numerical anchor.
#'
#' @param model A list from [trans_rate_lp_model()], or the reduced equivalent that
#' [trans_rate_reachability()] builds.
#' @keywords internal
add_balance_rows <- function(problem, model) {
  ix <- model[["ix"]]
  n_lulc <- model[["n_lulc"]]
  n_step <- model[["n_step"]]
  classes <- seq_len(n_lulc)

  for (l in classes) {
    add_lp_row(problem, ix[["area"]](l, 0L), 1, "=", model[["init_share"]][l])
  }
  for (t in seq.int(0L, n_step)) {
    add_lp_row(problem, ix[["area"]](classes, t), rep(1, n_lulc), "=", 1)
  }
  for (t in seq.int(0L, n_step - 1L)) {
    for (i in classes) {
      add_lp_row(
        problem,
        c(ix[["x"]](i, classes, t), ix[["area"]](i, t)),
        c(rep(1, n_lulc), -1),
        "=",
        0
      )
    }
    for (j in classes) {
      add_lp_row(
        problem,
        c(ix[["area"]](j, t + 1L), ix[["x"]](classes, j, t)),
        c(1, rep(-1, n_lulc)),
        "=",
        0
      )
    }
  }
  invisible(problem)
}

#' @describeIn trans_rate_lp Hard-zero the forbidden edges. A forbidden edge is zero, not
#' merely expensive -- non-viable transitions have no `trans_pot_t` rows and would be
#' dropped at allocation time. One row per edge and step: a single row summing the steps
#' says the same thing about non-negative flows, but it makes the program dense enough in
#' that one row for `lpSolve`'s default scaling to fail on it numerically.
#' @keywords internal
add_forbidden_rows <- function(problem, model) {
  ix <- model[["ix"]]

  for (i in seq_len(model[["n_lulc"]])) {
    for (j in seq_len(model[["n_lulc"]])) {
      if (!model[["forbidden"]][i, j]) {
        next
      }
      for (t in seq.int(0L, model[["n_step"]] - 1L)) {
        add_lp_row(problem, ix[["x"]](i, j, t), 1, "=", 0)
      }
    }
  }
  invisible(problem)
}

#' @describeIn trans_rate_lp Bound each flow by the historic rates of its edge, times the
#' area of its source class at that step. With the `slack` block the bounds are soft and
#' widened by `margin`, which is what lets an elicited target that lies outside the observed
#' envelope still be met, at a price. Without it they are hard and the diagonal is left
#' free, which is the question [trans_rate_reachability()] asks.
#' @keywords internal
add_rate_bound_rows <- function(problem, model) {
  ix <- model[["ix"]]
  soft <- isTRUE(model[["blocks"]][["slack"]])
  margin <- if (soft) model[["params"]][["margin"]] else 0
  classes <- seq_len(model[["n_lulc"]])

  for (t in seq.int(0L, model[["n_step"]] - 1L)) {
    for (i in classes) {
      for (j in classes) {
        if (model[["forbidden"]][i, j] || (!soft && i == j)) {
          next
        }

        upper <- model[["rate"]][["max"]][i, j] + margin
        if (upper < 1) {
          cols <- c(ix[["x"]](i, j, t), ix[["area"]](i, t))
          vals <- c(1, -upper)
          if (soft) {
            cols <- c(cols, ix[["upper"]](i, j, t))
            vals <- c(vals, -1)
          }
          add_lp_row(problem, cols, vals, "<=", 0)
        }

        lower <- model[["rate"]][["min"]][i, j] - margin
        if (soft && lower > 0) {
          add_lp_row(
            problem,
            c(ix[["x"]](i, j, t), ix[["area"]](i, t), ix[["lower"]](i, j, t)),
            c(-1, lower, -1),
            "<=",
            0
          )
        }
      }
    }
  }
  invisible(problem)
}

#' @describeIn trans_rate_lp Pull each flow towards the historic outflow pattern, as an L1
#' penalty on `|x[i, j, t] - ref_rate[i, j] * area[i, t]|`. This is what stops the program
#' inventing edges that happen to be cheap.
#' @keywords internal
add_historic_rows <- function(problem, model) {
  if (!isTRUE(model[["blocks"]][["historic"]])) {
    return(invisible(problem))
  }
  ix <- model[["ix"]]
  classes <- seq_len(model[["n_lulc"]])

  for (t in seq.int(0L, model[["n_step"]] - 1L)) {
    for (i in classes) {
      for (j in classes) {
        if (i == j || model[["forbidden"]][i, j]) {
          next
        }
        cols <- c(ix[["x"]](i, j, t), ix[["area"]](i, t), ix[["historic"]](i, j, t))
        ref <- model[["rate"]][["ref"]][i, j]
        add_lp_row(problem, cols, c(1, -ref, -1), "<=", 0)
        add_lp_row(problem, cols, c(-1, ref, -1), "<=", 0)
      }
    }
  }
  invisible(problem)
}

#' @describeIn trans_rate_lp Tie the terminal areas to the targets: a hard band when
#' `terminal_band` is set, and the L1 fit `area[l, T] - target = plus - minus` when
#' `mu_target` is positive.
#' @keywords internal
add_target_rows <- function(problem, model) {
  ix <- model[["ix"]]
  n_step <- model[["n_step"]]
  band <- model[["params"]][["terminal_band"]]
  target_share <- model[["target_share"]]

  if (!is.null(band) && !is.na(band)) {
    stopifnot("terminal_band must be non-negative" = band >= 0)
    for (l in seq_len(model[["n_lulc"]])) {
      add_lp_row(problem, ix[["area"]](l, n_step), 1, ">=", (1 - band) * target_share[l])
      add_lp_row(problem, ix[["area"]](l, n_step), 1, "<=", (1 + band) * target_share[l])
    }
  }

  if (isTRUE(model[["blocks"]][["target"]])) {
    for (l in seq_len(model[["n_lulc"]])) {
      add_lp_row(
        problem,
        c(ix[["area"]](l, n_step), ix[["plus"]](l), ix[["minus"]](l)),
        c(1, -1, 1),
        "=",
        target_share[l]
      )
    }
  }
  invisible(problem)
}

#' @describeIn trans_rate_lp Constrain each class to move in the direction of
#' `sign(target - init)`, as a hard constraint with no tolerance. `monotone_sign` of `NULL`
#' or 0 leaves the trajectory free.
#' @keywords internal
add_monotonicity_rows <- function(problem, model) {
  signs <- model[["monotone_sign"]]
  if (is.null(signs)) {
    return(invisible(problem))
  }
  ix <- model[["ix"]]
  stopifnot("monotone_sign must have one entry per class" = length(signs) == model[["n_lulc"]])

  for (l in seq_len(model[["n_lulc"]])) {
    if (is.na(signs[l]) || signs[l] == 0) {
      next
    }
    for (t in seq_len(model[["n_step"]])) {
      add_lp_row(
        problem,
        c(ix[["area"]](l, t), ix[["area"]](l, t - 1L)),
        c(1, -1),
        if (signs[l] > 0) ">=" else "<=",
        0
      )
    }
  }
  invisible(problem)
}

#' @describeIn trans_rate_lp The curvature of a class trajectory: its per-year change over
#' step `t` against that over step `t + 1`, scaled by the length of step `t`.
#'
#' @param l Index of a land use class.
#' @param t Index of a step.
#' @return `curvature_row()` returns a list of `cols` and `vals`.
#' @keywords internal
curvature_row <- function(model, l, t) {
  ix <- model[["ix"]]
  ratio <- model[["step_years"]][t] / model[["step_years"]][t + 1L]
  list(
    cols = c(ix[["area"]](l, t - 1L), ix[["area"]](l, t), ix[["area"]](l, t + 1L)),
    vals = c(-1, 1 + ratio, -ratio)
  )
}

#' @describeIn trans_rate_lp Ask each class trajectory to curve the way its elicited shape
#' says, as a softly penalised one-sided constraint on curvature. Note that zero curvature
#' satisfies all five shapes at once, so the block only bites once `shape_strictness`
#' demands a minimum curvature.
#' @keywords internal
add_shape_rows <- function(problem, model) {
  if (!isTRUE(model[["blocks"]][["shape"]])) {
    return(invisible(problem))
  }
  ix <- model[["ix"]]
  shape <- model[["shape"]]
  n_step <- model[["n_step"]]
  init_share <- model[["init_share"]]
  target_share <- model[["target_share"]]

  for (l in seq_len(model[["n_lulc"]])) {
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

    strict <- model[["params"]][["shape_strictness"]] *
      abs(target_share[l] - init_share[l]) /
      n_step

    for (t in seq_len(n_step - 1L)) {
      row <- curvature_row(model, l, t)
      slack <- ix[["shape"]](l, t)
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
  invisible(problem)
}

#' @describeIn trans_rate_lp Penalise the second difference of each class trajectory. This
#' is a tie-breaker among the many trajectories that satisfy everything else, not a
#' modelling statement.
#' @keywords internal
add_smoothness_rows <- function(problem, model) {
  if (!isTRUE(model[["blocks"]][["smooth"]])) {
    return(invisible(problem))
  }
  ix <- model[["ix"]]

  for (l in seq_len(model[["n_lulc"]])) {
    for (t in seq_len(model[["n_step"]] - 1L)) {
      cols <- c(
        ix[["area"]](l, t + 1L),
        ix[["area"]](l, t),
        ix[["area"]](l, t - 1L),
        ix[["smooth"]](l, t)
      )
      add_lp_row(problem, cols, c(1, -2, 1, -1), "<=", 0)
      add_lp_row(problem, cols, c(-1, 2, -1, -1), "<=", 0)
    }
  }
  invisible(problem)
}

#' @describeIn trans_rate_lp Bound the worst per-class rate-bound violation from above by a
#' single variable. A minimax of linear expressions is itself linear, so this needs no
#' quadratic solver.
#' @keywords internal
add_fairness_rows <- function(problem, model) {
  if (!isTRUE(model[["blocks"]][["fairness"]])) {
    return(invisible(problem))
  }
  ix <- model[["ix"]]
  pairs <- expand.grid(j = seq_len(model[["n_lulc"]]), t = seq.int(0L, model[["n_step"]] - 1L))

  for (i in seq_len(model[["n_lulc"]])) {
    cols <- c(
      ix[["lower"]](i, pairs[["j"]], pairs[["t"]]),
      ix[["upper"]](i, pairs[["j"]], pairs[["t"]]),
      ix[["fair"]]()
    )
    vals <- c(rep(model[["weight"]][i], 2L * nrow(pairs)), -1)
    add_lp_row(problem, cols, vals, "<=", 0)
  }
  invisible(problem)
}

#' @describeIn trans_rate_lp Assemble every constraint block into one program.
#' @keywords internal
trans_rate_lp_problem <- function(model) {
  problem <- new_lp_problem(model[["n_var"]])

  add_balance_rows(problem, model)
  add_forbidden_rows(problem, model)
  add_rate_bound_rows(problem, model)
  add_historic_rows(problem, model)
  add_target_rows(problem, model)
  add_monotonicity_rows(problem, model)
  add_shape_rows(problem, model)
  add_smoothness_rows(problem, model)
  add_fairness_rows(problem, model)

  problem
}

#' @describeIn trans_rate_lp The objective: a weighted sum of the slack penalties. Weights
#' are `1 / init_share`, so that a violation counts relative to the size of the class it
#' happens in and small classes are not ignored.
#' @keywords internal
trans_rate_lp_objective <- function(model) {
  ix <- model[["ix"]]
  params <- model[["params"]]
  weight <- model[["weight"]]
  blocks <- model[["blocks"]]
  n_step <- model[["n_step"]]
  curve_steps <- seq_len(n_step - 1L)
  pairs <- expand.grid(j = seq_len(model[["n_lulc"]]), t = seq.int(0L, n_step - 1L))

  objective <- numeric(model[["n_var"]])
  for (i in seq_len(model[["n_lulc"]])) {
    objective[ix[["lower"]](i, pairs[["j"]], pairs[["t"]])] <- params[["lambda_bounds"]] * weight[i]
    objective[ix[["upper"]](i, pairs[["j"]], pairs[["t"]])] <- params[["lambda_bounds"]] * weight[i]
    if (isTRUE(blocks[["historic"]])) {
      objective[ix[["historic"]](i, pairs[["j"]], pairs[["t"]])] <- params[["mu_historic"]] *
        weight[i]
    }
  }
  for (l in seq_len(model[["n_lulc"]])) {
    if (isTRUE(blocks[["shape"]])) {
      objective[ix[["shape"]](l, curve_steps)] <- params[["mu_shape"]] * weight[l]
    }
    if (isTRUE(blocks[["smooth"]])) {
      objective[ix[["smooth"]](l, curve_steps)] <- params[["mu_smooth"]] * weight[l]
    }
    if (isTRUE(blocks[["target"]])) {
      objective[c(ix[["plus"]](l), ix[["minus"]](l))] <- params[["mu_target"]] * weight[l]
    }
  }
  if (isTRUE(blocks[["fairness"]])) {
    objective[ix[["fair"]]()] <- model[["fair_weight"]]
  }
  objective
}

#' @describeIn trans_rate_lp Turn the solved variable vector back into an `areas` and a
#' `flows` table, in cells, keyed by `id_lulc` and `id_trans` and labelled with the
#' `id_period` each step belongs to.
#'
#' @param values The `solution` element of an [lpSolve::lp()] result.
#' @return `trans_rate_lp_tables()` returns a list of `areas` and `flows`.
#' @keywords internal
trans_rate_lp_tables <- function(values, model) {
  ix <- model[["ix"]]
  ids <- model[["ids"]]
  total <- model[["total"]]
  n_step <- model[["n_step"]]
  id_periods <- model[["id_periods"]]

  areas <- data.table::CJ(step = seq.int(0L, n_step), id_lulc = ids, sorted = FALSE)
  areas[, area := values[ix[["area"]](match(id_lulc, ids), step)] * total]

  flows <- data.table::CJ(
    step = seq_len(n_step),
    id_lulc_anterior = ids,
    id_lulc_posterior = ids,
    sorted = FALSE
  )
  flows[, c("row", "col") := .(match(id_lulc_anterior, ids), match(id_lulc_posterior, ids))]
  flows[, flow := values[ix[["x"]](row, col, step - 1L)] * total]
  flows[, dev_lower := values[ix[["lower"]](row, col, step - 1L)] * total]
  flows[, dev_upper := values[ix[["upper"]](row, col, step - 1L)] * total]
  flows[, area_anterior := values[ix[["area"]](row, step - 1L)] * total]
  flows[, rate := data.table::fifelse(area_anterior > 1e-9, flow / area_anterior, 0)]
  flows[, count := round(flow)]
  flows[, id_trans := model[["id_trans"]][cbind(row, col)]]
  flows[, is_viable := model[["viable"]][cbind(row, col)]]

  # measured against the historic envelope itself, not against the margin around it: the
  # margin is a modelling convenience, and on a large class it is a lot of unremarked flow
  flows[, max_flow := model[["rate"]][["max"]][cbind(row, col)] * area_anterior]
  flows[, min_flow := model[["rate"]][["min"]][cbind(row, col)] * area_anterior]
  flows[, above_max_rate := flow > max_flow + 1e-6]
  flows[, below_min_rate := flow < min_flow - 1e-6]

  if (is.null(id_periods)) {
    areas[, id_period := NA_integer_]
    flows[, id_period := NA_integer_]
  } else {
    areas[, id_period := c(NA_integer_, id_periods)[step + 1L]]
    flows[, id_period := id_periods[step]]
  }

  list(areas = areas, flows = flows)
}

#' @describeIn trans_rate_lp Summarise how far the solution had to depart from the target
#' and from observed history. These are results, not debug output: a solver that does not
#' report how far outside history it went is actively misleading.
#'
#' @param tables A list from [trans_rate_lp_tables()].
#' @param reachability The precheck table, as returned by `reachability_verdict()`.
#' @keywords internal
trans_rate_lp_diagnostics <- function(tables, values, model, reachability) {
  areas <- tables[["areas"]]
  flows <- tables[["flows"]]
  ids <- model[["ids"]]
  target <- model[["target"]]
  off_diagonal <- flows[["id_lulc_anterior"]] != flows[["id_lulc_posterior"]]

  final_area <- areas[step == model[["n_step"]]][match(ids, id_lulc)][["area"]]

  list(
    reachability = reachability,
    target_error = data.table::data.table(
      id_lulc = ids,
      area_init = model[["init"]],
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
      total_flow = flows[off_diagonal, sum(flow)],
      # flow the historic envelope does not support, and by how much it overshoots
      flow_above_max_rate = flows[off_diagonal & above_max_rate == TRUE, sum(flow)],
      excess_above_max_rate = flows[off_diagonal, sum(pmax(flow - max_flow, 0))],
      shortfall_below_min_rate = flows[off_diagonal, sum(pmax(min_flow - flow, 0))],
      flow_non_viable = flows[is_viable == FALSE, sum(flow)]
    ),
    shape = data.table::data.table(
      id_lulc = ids,
      shape = model[["shape"]],
      curvature_slack = if (isTRUE(model[["blocks"]][["shape"]])) {
        vapply(
          seq_len(model[["n_lulc"]]),
          \(l) {
            sum(values[model[["ix"]][["shape"]](l, seq_len(model[["n_step"]] - 1L))]) *
              model[["total"]]
          },
          numeric(1L)
        )
      } else {
        rep(NA_real_, model[["n_lulc"]])
      }
    )
  )
}
