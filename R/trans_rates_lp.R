#' R6 Class for the Demand-Driven Transition Rate Program
#'
#' @description
#' A coupled linear program that derives per-transition flows from per-class area targets.
#' It is assembled one constraint block at a time; every block has an `add_` method of the
#' same name, and [lp_problem] keeps the resulting rows tagged with that name so a program
#' can be solved over a subset of its blocks. That is what makes the reachability precheck
#' the same program as the solve, rather than a second implementation of it.
#'
#' @details
#' # Units
#'
#' The program is solved in shares of the landscape: shares and absolute areas are the same
#' program up to a scalar, but the share version spans six fewer orders of magnitude.
#' Everything the object returns is in cells, rehydrated against the number of cells
#' `lulc_data` holds at the anchor period.
#'
#' # Periods
#'
#' Following [trans_rates_t], the flows of period `p` take the landscape from its state at
#' `p - 1` to its state at `p`. Area variables therefore exist at the anchor period -- the
#' last observed one -- and at every extrapolated period, while flow variables exist at the
#' extrapolated periods only.
#'
#' # Variables
#'
#' All variables are non-negative. `id_lulc` identifies the class a class-level variable
#' belongs to; `id_lulc_anterior` and `id_lulc_posterior` identify a transition-level one.
#'
#' | **block** | **keys** | **meaning** |
#' | --- | --- | --- |
#' | `flow` | transition, period | cells moving along a transition |
#' | `area` | class, period | area of a class at a state |
#' | `rate_lower`, `rate_upper` | transition, period | rate-bound violation |
#' | `historic` | transition, period | distance from the historic outflow pattern |
#' | `shape`, `smoothness` | class, period | curvature violation |
#' | `target_over`, `target_under` | class | terminal fit either side of the target |
#' | `fairness` | none | worst per-class rate-bound violation |
#'
#' # Constraint blocks
#'
#' `initial`, `conservation` and `closure` are the mass balance; `forbidden` and
#' `rate_limits` are the hard statements about what a transition may do; the rest carry
#' slack variables and are paid for in the objective. `blocks` records which are enabled,
#' which enter a solve and which enter the reachability precheck.
#'
#' # Installation
#'
#' Solving needs `lpSolve`, which evoland only suggests: most of the package never solves a
#' linear program. `trans_rate_lp$new()` fails with an actionable message if it is missing,
#' so install it before constructing one -- `install.packages("lpSolve")`.
#'
#' @examples
#' periods <- create_periods_t("P10Y", "1990-01-01", "2020-01-01", "2040-01-01")
#' lulc_data <- as_lulc_data_t(data.table::data.table(
#'   id_run = 0L,
#'   id_coord = 1:10000,
#'   id_period = 4L,
#'   id_lulc = rep(1:2, c(6000, 4000))
#' ))
#' bounds <- data.table::data.table(
#'   id_trans = c(NA, 1L, NA, 2L),
#'   id_lulc_anterior = c(1L, 1L, 2L, 2L),
#'   id_lulc_posterior = c(1L, 2L, 2L, 1L),
#'   min_rate = c(0.9, 0, 0.95, 0),
#'   max_rate = c(1, 0.1, 1, 0.05),
#'   ref_rate = c(0.95, 0.05, 0.97, 0.03),
#'   is_viable = TRUE
#' )
#'
#' solver <- trans_rate_lp$new(
#'   lulc_data = lulc_data,
#'   bounds = bounds,
#'   periods = periods,
#'   targets = data.table::data.table(id_lulc = 1:2, share = c(0.5, 0.5))
#' )
#' solver$solve()
#' solver$areas
#'
#' @seealso [trans_rates_solver], [lp_problem], [trans_rates_t]
#' @export
trans_rate_lp <- R6::R6Class(
  classname = "trans_rate_lp",
  inherit = lp_problem,

  public = list(
    #' @field id_run Run the solved rates belong to, see [runs_t]. Set it before reading
    #' `trans_rates_t`; one demand solution is usually written to several runs.
    id_run = NULL,

    #' @description Set up the program from an observed landscape and a scenario demand.
    #' Constraint blocks are added immediately, so the object is ready to solve. Requires
    #' the suggested `lpSolve` package to be installed.
    #'
    #' @param lulc_data A [lulc_data_t] for a single `id_run`. The areas of its last
    #' observed period are the initial state.
    #' @param bounds Per-transition rate bounds, see [trans_rate_bounds()].
    #' @param periods A [periods_t]. Its extrapolated periods are the steps to solve for,
    #' and their lengths set the time scale of the trajectory-shape constraints.
    #' @param targets A data.table with `id_lulc` and either `area` (cells on the same grid
    #' as `lulc_data`) or `share` (of the landscape, rehydrated against it). Without
    #' targets the object can only answer `reachability`.
    #' @param shapes A data.table with `id_lulc` and `shape`, one of `"instant growth"`,
    #' `"delayed growth"`, `"constant change"`, `"instant decline"`, `"delayed decline"`.
    #' Note that a straight line satisfies every one of these one-sided curvature
    #' constraints, so shapes only bind when `shape_strictness > 0`.
    #' @param lambda_bounds Penalty weight on rate-bound violation.
    #' @param mu_shape Penalty weight on trajectory-shape violation.
    #' @param mu_smooth Penalty weight on the second difference of the trajectory; a
    #' tie-breaker among otherwise equivalent trajectories.
    #' @param mu_target Penalty weight on the L1 distance between the solved terminal area
    #' and the target. Without it, a hard terminal band is treated as free real estate and
    #' every class parks on a band edge.
    #' @param mu_historic Penalty weight on the L1 distance between flows and the historic
    #' outflow pattern (`ref_rate`). Zero by default; raising it keeps flows near the
    #' observed pattern where the target does not force otherwise.
    #' @param margin Slack around the rate bounds before a violation is penalised.
    #' @param terminal_band Relative half-width of a *hard* band around the terminal
    #' target, or `NA` (the default) to rely on `mu_target` alone. A hard band and
    #' `forbid_non_viable` together turn an out-of-reach target into an infeasible program
    #' rather than a near miss: on the SSP-CH demand that combination is infeasible for
    #' three of five scenarios, while the L1 fit lands as close as the viable transitions
    #' allow and reports the shortfall.
    #' @param shape_strictness Minimum curvature a shaped trajectory must exhibit, as a
    #' fraction of the class's mean per-step change.
    #' @param monotone Whether to hard-constrain each class to move monotonically in the
    #' direction of `sign(target - init)`.
    #' @param fairness Minimax bound on the worst per-class rate-bound violation. `TRUE`
    #' uses `lambda_bounds` as its weight; a number sets the weight explicitly.
    #' @param forbid_non_viable Whether to hard-zero flows on transitions that
    #' [trans_meta_t] marks as non-viable. Such flows have no [trans_pot_t] rows and would
    #' be silently dropped at allocation time, so the trajectory would not materialise.
    #' @param max_reachability_ratio Refuse to solve if a target asks for more than this
    #' multiple of the historically achievable change. Everything below the threshold is
    #' reported, not gated.
    #'
    #' @return A new `trans_rate_lp` object
    initialize = function(
      lulc_data,
      bounds,
      periods,
      targets = NULL,
      shapes = NULL,
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
      # checked again in lp_problem, but there is no point building the model tables first
      require_suggested("lpSolve", "build a trans_rate_lp")

      stopifnot(
        inherits(lulc_data, "lulc_data_t"),
        inherits(periods, "periods_t"),
        "bounds needs min_rate, max_rate and is_viable columns" = all(
          c("id_lulc_anterior", "id_lulc_posterior", "min_rate", "max_rate", "is_viable") %in%
            names(bounds)
        ),
        "penalty weights and tolerances must be single numbers" = all(
          lengths(list(
            lambda_bounds,
            mu_shape,
            mu_smooth,
            mu_target,
            mu_historic,
            margin,
            terminal_band,
            shape_strictness,
            max_reachability_ratio
          )) ==
            1L
        ),
        "terminal_band must be non-negative" = is.na(terminal_band) || terminal_band >= 0
      )

      private$.lambda_bounds <- lambda_bounds
      private$.mu_shape <- mu_shape
      private$.mu_smooth <- mu_smooth
      private$.mu_target <- mu_target
      private$.mu_historic <- mu_historic
      private$.margin <- margin
      private$.terminal_band <- terminal_band
      private$.shape_strictness <- shape_strictness
      private$.monotone <- isTRUE(monotone)
      private$.forbid_non_viable <- isTRUE(forbid_non_viable)
      private$.max_reachability_ratio <- max_reachability_ratio
      private$.fair_weight <- data.table::fcase(
        isTRUE(fairness)     , lambda_bounds            ,
        is.numeric(fairness) , as.numeric(fairness)[1L] ,
        default = 0
      )
      private$.has_targets <- !is.null(targets)

      private$build_steps(periods)
      private$build_scenario(lulc_data, targets, shapes)
      private$build_transitions(bounds)
      private$build_blocks()

      super$initialize(private$build_variables())
      private$build_anterior_area()
      self$add_all()

      invisible(self)
    },

    #' @description Add every enabled constraint block, by calling the `add_` method of the
    #' same name.
    #' @return The `trans_rate_lp` object, invisibly
    add_all = function() {
      for (block in private$.blocks[is_enabled == TRUE, block]) {
        self[[paste0("add_", block)]]()
      }
      invisible(self)
    },

    #' @description The landscape starts in the observed state.
    #' @return The `trans_rate_lp` object, invisibly
    add_initial = function() {
      anchor_area <-
        private$vars("area")[id_period == private$.anchor][
          private$.scenario[, .(id_lulc, share_init)],
          on = "id_lulc",
          nomatch = NULL
        ]

      self$add_constraints(
        "initial",
        anchor_area[, .(id_row = .I, id_var, coefficient = 1, dir = "=", rhs = share_init)]
      )
    },

    #' @description The classes cover the whole landscape at every state. Redundant given
    #' closure in both directions, but a cheap numerical anchor.
    #' @return The `trans_rate_lp` object, invisibly
    add_conservation = function() {
      self$add_constraints(
        "conservation",
        private$vars("area")[,
          .(id_var, coefficient = 1, dir = "=", rhs = 1),
          by = .(id_row = id_period)
        ]
      )
    },

    #' @description Every cell of a class leaves it along exactly one transition, and every
    #' cell of a state arrived along one. Together with `initial` this conserves area
    #' exactly.
    #' @return The `trans_rate_lp` object, invisibly
    add_closure = function() {
      flows <- private$vars("flow")
      areas <- private$vars("area")

      # what leaves class i during period p is the area it held at the previous state
      outflow <-
        rbind(
          flows[, .(id_lulc = id_lulc_anterior, id_period, id_var, coefficient = 1)],
          private$.anterior_area[,
            .(id_lulc = id_lulc_anterior, id_period, id_var = id_var_area, coefficient = -1)
          ]
        )[, id_row := .GRP, by = .(id_lulc, id_period)]

      # what class j holds at state p is everything that arrived during period p
      inflow <-
        rbind(
          areas[
            id_period %in% private$.steps[["id_period"]],
            .(id_lulc, id_period, id_var, coefficient = 1)
          ],
          flows[, .(id_lulc = id_lulc_posterior, id_period, id_var, coefficient = -1)]
        )[, id_row := .GRP + max(outflow[["id_row"]]), by = .(id_lulc, id_period)]

      self$add_constraints(
        "closure",
        rbind(outflow, inflow)[, `:=`(dir = "=", rhs = 0)]
      )
    },

    #' @description A non-viable transition carries no flow at all. It is zero, not merely
    #' expensive: it has no [trans_pot_t] rows and would be dropped at allocation time. One
    #' row per transition and period -- a single row summing the periods says the same
    #' thing about non-negative flows, but it is dense enough for [lpSolve::lp()]'s default
    #' scaling to fail on it numerically.
    #' @return The `trans_rate_lp` object, invisibly
    add_forbidden = function() {
      forbidden <-
        private$vars("flow")[
          private$.transitions[is_forbidden == TRUE, .(id_lulc_anterior, id_lulc_posterior)],
          on = .(id_lulc_anterior, id_lulc_posterior),
          nomatch = NULL
        ]

      self$add_constraints(
        "forbidden",
        forbidden[, .(id_row = .I, id_var, coefficient = 1, dir = "=", rhs = 0)]
      )
    },

    #' @description No transition moves faster than it ever has, as a *hard* bound with no
    #' slack and no margin, and with persistence left free. This is the loosest honest
    #' question about what a class can reach, and it is the block `reachability` solves
    #' over. It is deliberately absent from a solve, where the same statement appears as
    #' the softly penalised `rate_bounds`.
    #' @return The `trans_rate_lp` object, invisibly
    add_rate_limits = function() {
      limits <- private$flow_with_area(
        private$.transitions[is_persistence == FALSE & is_forbidden == FALSE]
      )

      self$add_constraints(
        "rate_limits",
        rbind(
          limits[, .(id_row = .I, id_var, coefficient = 1)],
          limits[, .(id_row = .I, id_var = id_var_area, coefficient = -max_rate)]
        )[, `:=`(dir = "<=", rhs = 0)]
      )
    },

    #' @description No transition moves much faster or much slower than it historically
    #' has, as a softly penalised bound widened by `margin`. Soft because elicited targets
    #' routinely require flows outside the observed envelope; how far outside is reported
    #' in `diagnostics` rather than suppressed.
    #' @return The `trans_rate_lp` object, invisibly
    add_rate_bounds = function() {
      bounded <- private$flow_with_area(private$.transitions[is_forbidden == FALSE])
      bounded[
        private$vars("rate_upper")[, .(id_lulc_anterior, id_lulc_posterior, id_period, id_var)],
        id_var_upper := i.id_var,
        on = .(id_lulc_anterior, id_lulc_posterior, id_period)
      ]
      bounded[
        private$vars("rate_lower")[, .(id_lulc_anterior, id_lulc_posterior, id_period, id_var)],
        id_var_lower := i.id_var,
        on = .(id_lulc_anterior, id_lulc_posterior, id_period)
      ]

      # a bound at or above 1 is implied by closure, one at or below 0 by non-negativity
      upper <- bounded[max_rate + private$.margin < 1]
      lower <- bounded[min_rate - private$.margin > 0]

      self$add_constraints(
        "rate_bounds",
        rbind(
          upper[, .(id_row = .I, id_var, coefficient = 1)],
          upper[, .(
            id_row = .I,
            id_var = id_var_area,
            coefficient = -(max_rate + private$.margin)
          )],
          upper[, .(id_row = .I, id_var = id_var_upper, coefficient = -1)],
          lower[, .(id_row = .I + nrow(upper), id_var, coefficient = -1)],
          lower[,
            .(
              id_row = .I + nrow(upper),
              id_var = id_var_area,
              coefficient = min_rate - private$.margin
            )
          ],
          lower[, .(id_row = .I + nrow(upper), id_var = id_var_lower, coefficient = -1)]
        )[, `:=`(dir = "<=", rhs = 0)]
      )
    },

    #' @description Flows stay near the historic outflow pattern, as an L1 penalty on
    #' `|flow - ref_rate * area|`. This is the term that stops the program inventing
    #' transitions that happen to be cheap.
    #' @return The `trans_rate_lp` object, invisibly
    add_historic = function() {
      preferred <- private$flow_with_area(
        private$.transitions[is_persistence == FALSE & is_forbidden == FALSE]
      )
      preferred[
        private$vars("historic")[, .(id_lulc_anterior, id_lulc_posterior, id_period, id_var)],
        id_var_historic := i.id_var,
        on = .(id_lulc_anterior, id_lulc_posterior, id_period)
      ]

      self$add_constraints(
        "historic",
        rbind(
          preferred[, .(id_row = .I, id_var, coefficient = 1)],
          preferred[, .(id_row = .I, id_var = id_var_area, coefficient = -ref_rate)],
          preferred[, .(id_row = .I, id_var = id_var_historic, coefficient = -1)],
          preferred[, .(id_row = .I + nrow(preferred), id_var, coefficient = -1)],
          preferred[, .(
            id_row = .I + nrow(preferred),
            id_var = id_var_area,
            coefficient = ref_rate
          )],
          preferred[, .(id_row = .I + nrow(preferred), id_var = id_var_historic, coefficient = -1)]
        )[, `:=`(dir = "<=", rhs = 0)]
      )
    },

    #' @description The landscape ends where the scenario asks it to: an L1 fit that
    #' degrades gracefully, plus a hard band when `terminal_band` is set.
    #' @return The `trans_rate_lp` object, invisibly
    add_target = function() {
      terminal <-
        private$vars("area")[id_period == private$.horizon][
          private$.scenario[, .(id_lulc, share_target)],
          on = "id_lulc",
          nomatch = NULL
        ]
      terminal[
        private$vars("target_over")[, .(id_lulc, id_var)],
        id_var_over := i.id_var,
        on = "id_lulc"
      ]
      terminal[
        private$vars("target_under")[, .(id_lulc, id_var)],
        id_var_under := i.id_var,
        on = "id_lulc"
      ]

      # area - over + under = target, so that the objective pays for the distance either way
      fit <-
        rbind(
          terminal[, .(id_row = .I, id_var, coefficient = 1)],
          terminal[, .(id_row = .I, id_var = id_var_over, coefficient = -1)],
          terminal[, .(id_row = .I, id_var = id_var_under, coefficient = 1)]
        )[
          terminal[, .(id_row = .I, rhs = share_target)],
          on = "id_row"
        ][, dir := "="]

      band <- private$.terminal_band
      n_class <- nrow(terminal)
      if (!is.na(band)) {
        fit <- rbind(
          fit,
          terminal[, .(id_row = .I + n_class, id_var, coefficient = 1, dir = ">=")][,
            rhs := (1 - band) * terminal[["share_target"]]
          ],
          terminal[, .(id_row = .I + 2L * n_class, id_var, coefficient = 1, dir = "<=")][,
            rhs := (1 + band) * terminal[["share_target"]]
          ]
        )
      }

      self$add_constraints("target", fit)
    },

    #' @description Each class moves in the direction of its target and does not turn back,
    #' as a hard constraint with no tolerance.
    #' @return The `trans_rate_lp` object, invisibly
    add_monotonicity = function() {
      areas <- private$vars("area")[, .(id_lulc, id_period, id_var)]
      moving <-
        private$.steps[, .(id_period, id_period_prev)][
          areas,
          on = "id_period",
          nomatch = NULL
        ][
          private$.scenario[monotone_sign != 0, .(id_lulc, monotone_sign)],
          on = "id_lulc",
          nomatch = NULL
        ]
      moving[areas, id_var_prev := i.id_var, on = .(id_lulc, id_period_prev = id_period)]
      moving[, dir := data.table::fifelse(monotone_sign > 0, ">=", "<=")]

      self$add_constraints(
        "monotonicity",
        rbind(
          moving[, .(id_row = .I, id_var, coefficient = 1, dir, rhs = 0)],
          moving[, .(id_row = .I, id_var = id_var_prev, coefficient = -1, dir, rhs = 0)]
        )
      )
    },

    #' @description Each class trajectory curves the way its elicited shape says, as a
    #' softly penalised one-sided constraint on the change in the per-year rate of change.
    #' `"instant"` front-loads the change and `"delayed"` back-loads it; `"constant
    #' change"` asks for both at once, which is what makes it an equality.
    #' @return The `trans_rate_lp` object, invisibly
    add_shape = function() {
      shaped <- private$curvature_terms(
        private$.scenario[shape_binds == TRUE, .(id_lulc, shape, strict)],
        "shape"
      )

      front_loaded <- shaped[shape %chin% c("instant growth", "delayed decline", "constant change")]
      back_loaded <- shaped[shape %chin% c("delayed growth", "instant decline", "constant change")]
      # a shape that asks for constant change asks for no curvature in either direction
      front_loaded[, rhs := data.table::fifelse(shape == "constant change", 0, strict)]
      back_loaded[, rhs := data.table::fifelse(shape == "constant change", 0, -strict)]

      self$add_constraints(
        "shape",
        rbind(
          private$curvature_rows(front_loaded, slack_coefficient = 1, dir = ">="),
          private$curvature_rows(
            back_loaded,
            slack_coefficient = -1,
            dir = "<=",
            id_row_offset = nrow(front_loaded)
          )
        )
      )
    },

    #' @description Each class trajectory prefers a small second difference. A tie-breaker
    #' among the many trajectories that satisfy everything else, not a modelling statement.
    #' @return The `trans_rate_lp` object, invisibly
    add_smoothness = function() {
      smooth <- private$curvature_terms(private$.scenario[, .(id_lulc)], "smoothness")
      # the second difference, as opposed to the change in per-year rate of change, bounded
      # from both sides so that the slack absorbs its absolute value
      positive <- data.table::copy(smooth)[,
        `:=`(coefficient_prev = 1, coefficient_here = -2, coefficient_next = 1, rhs = 0)
      ]
      negative <- data.table::copy(smooth)[,
        `:=`(coefficient_prev = -1, coefficient_here = 2, coefficient_next = -1, rhs = 0)
      ]

      self$add_constraints(
        "smoothness",
        rbind(
          private$curvature_rows(positive, slack_coefficient = -1, dir = "<="),
          private$curvature_rows(
            negative,
            slack_coefficient = -1,
            dir = "<=",
            id_row_offset = nrow(positive)
          )
        )
      )
    },

    #' @description No class carries a much worse rate-bound violation than the others. A
    #' minimax of linear expressions is itself linear, so this needs no quadratic solver.
    #' @return The `trans_rate_lp` object, invisibly
    add_fairness = function() {
      violation <- rbind(private$vars("rate_lower"), private$vars("rate_upper"))
      violation[
        private$.scenario[, .(id_lulc, weight)],
        weight := i.weight,
        on = .(id_lulc_anterior = id_lulc)
      ]
      worst <- private$vars("fairness")[["id_var"]]

      self$add_constraints(
        "fairness",
        rbind(
          violation[, .(id_lulc_anterior, id_var, coefficient = weight)],
          violation[,
            .(id_lulc_anterior = unique(id_lulc_anterior), id_var = worst, coefficient = -1)
          ]
        )[, id_row := .GRP, by = id_lulc_anterior][, `:=`(dir = "<=", rhs = 0)]
      )
    },

    #' @description Solve for the flows that take the landscape to its targets, after
    #' checking that the targets are not grossly beyond what history supports.
    #' @return The `trans_rate_lp` object, invisibly
    solve = function() {
      stopifnot(
        "no targets were given; this program can only answer reachability" = private$.has_targets
      )
      private$assert_reachable_targets()

      super$solve(
        objective = private$objective_coefficients(),
        direction = "min",
        blocks = private$.blocks[is_enabled == TRUE & in_solution == TRUE, block]
      )

      if (self$status != 0L) {
        stop(glue::glue(
          "The transition rate LP has no solution (lpSolve status {self$status}). ",
          "A hard terminal_band on an out-of-reach target is the usual cause; ",
          "set terminal_band = NA to fall back on the mu_target penalty, ",
          "or inspect the reachability field."
        ))
      }
      invisible(self)
    },

    #' @description Print a summary of the program.
    #' @param ... Ignored.
    print = function(...) {
      cat(glue::glue(
        "<trans_rate_lp>\n",
        "{nrow(private$.scenario)} classes, {nrow(private$.steps)} steps ",
        "from period {private$.anchor} to {private$.horizon}\n",
        "{format(private$.total, big.mark = ',')} cells, ",
        "{nrow(private$.transitions[is_forbidden == FALSE])} allowed transitions\n",
        "status: {self$status %||% 'unsolved'}\n\n"
      ))
      print(private$.blocks)
      invisible(self)
    }
  ),

  active = list(
    #' @field blocks The constraint blocks, whether each is enabled, and which programs
    #' each takes part in.
    blocks = function() private$.blocks[],

    #' @field scenario Per class: the initial and target areas, the elicited shape and the
    #' objective weight.
    scenario = function() private$.scenario[],

    #' @field transitions Every ordered pair of classes with its rate bounds, whether it is
    #' viable, and whether the program forbids it.
    transitions = function() private$.transitions[],

    #' @field steps The extrapolated periods, the state each starts from, and its length.
    steps = function() private$.steps[],

    #' @field reachability Per class and period: the areas reachable under mass balance and
    #' hard historic rates, ignoring targets. Solved on demand and then cached.
    reachability = function() {
      if (is.null(private$.reachability)) {
        private$.reachability <- private$solve_reachability()
      }
      private$.reachability[]
    },

    #' @field areas The solved class areas, in cells, per class and state.
    areas = function() {
      self$values[block == "area", .(id_lulc, id_period, area = value * private$.total)][
        order(id_period, id_lulc)
      ]
    },

    #' @field flows The solved transition flows, in cells.
    flows = function() {
      private$solved_flows()[,
        .(
          id_trans,
          id_lulc_anterior,
          id_lulc_posterior,
          id_period,
          flow,
          count,
          is_viable
        )
      ]
    },

    #' @field rates The solved flows as rates of their anterior class, which is what
    #' [adjusted_trans_pot_v()] and the allocators consume.
    rates = function() {
      private$solved_flows()[,
        .(
          id_trans,
          id_lulc_anterior,
          id_lulc_posterior,
          id_period,
          count,
          rate,
          is_viable
        )
      ]
    },

    #' @field trans_rates_t The solved rates as a [trans_rates_t] for the current `id_run`.
    #' Persistence and non-viable transitions are dropped: neither has an `id_trans` or any
    #' [trans_pot_t] rows to be allocated against.
    trans_rates_t = function() {
      stopifnot("id_run must be set" = length(self$id_run) == 1L && !is.na(self$id_run))
      flows <- private$solved_flows()
      stopifnot(
        "flow was allocated to non-viable transitions; set forbid_non_viable = TRUE" = flows[
          is_viable == FALSE,
          sum(count)
        ] ==
          0
      )

      flows[
        is_viable == TRUE & !is.na(id_trans),
        .(id_run = as.integer(self$id_run), id_period, id_trans, count, rate)
      ] |>
        as_trans_rates_t()
    },

    #' @field diagnostics How far the solution had to depart from the target and from
    #' observed history. These are results, not debug output: a solver that does not report
    #' how far outside history it went is actively misleading.
    diagnostics = function() {
      flows <- private$solved_flows()
      off_diagonal <- flows[id_lulc_anterior != id_lulc_posterior]

      list(
        reachability = private$reachability_verdict(),
        target_error = private$.scenario[
          self$areas[id_period == private$.horizon],
          on = "id_lulc",
          .(
            id_lulc,
            area_init,
            area_final = i.area,
            target = area_target,
            error = i.area - area_target,
            pct_error = 100 * (i.area - area_target) / pmax(area_target, 1)
          )
        ],
        bound_violation = flows[
          dev_lower > 1e-6 | dev_upper > 1e-6,
          .(id_trans, id_lulc_anterior, id_lulc_posterior, id_period, dev_lower, dev_upper)
        ],
        flow_summary = data.table::data.table(
          total_flow = off_diagonal[, sum(flow)],
          # flow the historic envelope does not support, and by how much it overshoots
          flow_above_max_rate = off_diagonal[flow > max_flow + 1e-6, sum(flow)],
          excess_above_max_rate = off_diagonal[, sum(pmax(flow - max_flow, 0))],
          shortfall_below_min_rate = off_diagonal[, sum(pmax(min_flow - flow, 0))],
          flow_non_viable = flows[is_viable == FALSE, sum(flow)]
        ),
        shape = private$shape_diagnostics()
      )
    }
  ),

  private = list(
    .lambda_bounds = NULL,
    .mu_shape = NULL,
    .mu_smooth = NULL,
    .mu_target = NULL,
    .mu_historic = NULL,
    .margin = NULL,
    .terminal_band = NULL,
    .shape_strictness = NULL,
    .monotone = NULL,
    .forbid_non_viable = NULL,
    .fair_weight = NULL,
    .max_reachability_ratio = NULL,
    .has_targets = NULL,

    .blocks = NULL,
    .scenario = NULL,
    .transitions = NULL,
    .steps = NULL,
    .curvature = NULL,
    .anterior_area = NULL,
    .reachability = NULL,
    .anchor = NULL,
    .horizon = NULL,
    .total = NULL,

    ## Model tables ----

    # the extrapolated periods, each with the state it starts from and its length in years
    build_steps = function(periods) {
      periods <- data.table::as.data.table(periods)
      stopifnot(
        "periods_t needs an extrapolated period to solve for" = nrow(periods[
          is_extrapolated == TRUE
        ]) >
          0L,
        "periods_t needs an observed period to start from" = nrow(periods[
          id_period > 0L & is_extrapolated == FALSE
        ]) >
          0L
      )

      private$.anchor <- periods[id_period > 0L & is_extrapolated == FALSE, max(id_period)]
      private$.steps <-
        periods[is_extrapolated == TRUE][order(id_period)][,
          .(
            id_period,
            id_period_prev = data.table::shift(id_period, fill = private$.anchor),
            period_length_y = period_length_d / 365.25
          )
        ]
      private$.horizon <- private$.steps[, max(id_period)]

      # curvature compares the per-year change over consecutive steps, so it is defined on
      # every pair of them and keyed by the earlier one
      private$.curvature <-
        private$.steps[,
          .(
            id_period,
            id_period_prev,
            id_period_next = data.table::shift(id_period, -1L),
            ratio = period_length_y / data.table::shift(period_length_y, -1L)
          )
        ][!is.na(id_period_next)]
      invisible(self)
    },

    # one row per class: where it starts, where it is asked to go, and how it may travel
    build_scenario = function(lulc_data, targets, shapes) {
      scenario <-
        data.table::as.data.table(lulc_data)[
          id_period == private$.anchor,
          .(area_init = .N),
          by = id_lulc
        ][order(id_lulc)]
      stopifnot(
        "lulc_data must hold a single id_run" = data.table::uniqueN(lulc_data[["id_run"]]) == 1L,
        "lulc_data has no cells in the last observed period" = nrow(scenario) > 0L,
        "lulc_data must cover the last observed period of periods_t" = private$.anchor %in%
          lulc_data[["id_period"]]
      )

      private$.total <- scenario[, sum(area_init)]
      scenario[, share_init := area_init / private$.total]
      scenario[, weight := 1 / pmax(share_init, 1e-9)]

      if (is.null(targets)) {
        private$.scenario <- scenario
        return(invisible(self))
      }

      scenario[
        private$target_shares(targets, scenario[["id_lulc"]]),
        share_target := i.share_target,
        on = "id_lulc"
      ]
      scenario[, area_target := share_target * private$.total]
      scenario[, shape := NA_character_]
      if (!is.null(shapes)) {
        scenario[canonical_shapes(shapes), shape := i.shape, on = "id_lulc"]
      }
      scenario[,
        monotone_sign := if (private$.monotone) sign(area_target - area_init) else 0
      ]
      # a shape elicited against the opposite direction of change says nothing
      scenario[,
        shape_binds := data.table::fcase(
          shape %chin% c("instant growth", "delayed growth")   , area_target > area_init ,
          shape %chin% c("instant decline", "delayed decline") , area_target < area_init ,
          shape == "constant change"                           , TRUE                    ,
          default = FALSE
        )
      ]
      scenario[,
        strict := private$.shape_strictness *
          abs(share_target - share_init) /
          nrow(private$.steps)
      ]

      private$.scenario <- scenario
      invisible(self)
    },

    # targets may be stated in cells on our own grid, or as a share of the landscape, which
    # is the only form that transfers between grids
    target_shares = function(targets, classes) {
      targets <- data.table::as.data.table(targets)
      stopifnot(
        "targets needs an id_lulc column" = "id_lulc" %in% names(targets),
        "targets needs either an area or a share column" = any(
          c("area", "share") %in% names(targets)
        ),
        "targets must cover exactly the classes of lulc_data" = setequal(
          targets[["id_lulc"]],
          classes
        ),
        "targets must be non-negative" = all(
          targets[[
            if ("share" %in% names(targets)) "share" else "area"
          ]] >=
            0
        )
      )

      if ("share" %in% names(targets)) {
        total_share <- targets[, sum(share)]
        if (abs(total_share - 1) > 1e-6) {
          warning(sprintf("target shares sum to %.6f, renormalising to 1", total_share))
        }
        return(targets[, .(id_lulc, share_target = share / total_share)])
      }

      stopifnot(
        "target areas must sum to the area of lulc_data; state them as shares to rehydrate" = abs(
          targets[, sum(area)] - private$.total
        ) <=
          1e-6 * private$.total
      )
      targets[, .(id_lulc, share_target = area / private$.total)]
    },

    # every ordered pair of classes, whether or not it was ever observed
    build_transitions = function(bounds) {
      # id_trans and ref_rate are only needed to write rates out and to prefer the historic
      # pattern; a caller interested in reachability alone need not supply them
      bounds <- data.table::copy(data.table::as.data.table(bounds))
      for (optional in setdiff(c("id_trans", "ref_rate"), names(bounds))) {
        data.table::set(bounds, j = optional, value = NA)
      }

      classes <- private$.scenario[["id_lulc"]]
      transitions <- data.table::CJ(id_lulc_anterior = classes, id_lulc_posterior = classes)
      transitions[
        bounds,
        `:=`(
          id_trans = as.integer(i.id_trans),
          min_rate = i.min_rate,
          max_rate = i.max_rate,
          ref_rate = as.numeric(i.ref_rate),
          is_viable = i.is_viable
        ),
        on = .(id_lulc_anterior, id_lulc_posterior)
      ]

      transitions[, is_persistence := id_lulc_anterior == id_lulc_posterior]
      transitions[, has_history := !is.na(min_rate)]
      transitions[
        has_history == FALSE,
        `:=`(min_rate = 0, max_rate = 0, ref_rate = 0, is_viable = FALSE)
      ]
      # a transition with no recorded historic mean has no historic pattern to prefer
      transitions[is.na(ref_rate), ref_rate := 0]
      # a class must be able to stay itself, even where no history says how often
      transitions[is_persistence == TRUE, is_viable := TRUE]
      transitions[is_persistence == TRUE & has_history == FALSE, max_rate := 1]
      transitions[, is_forbidden := private$.forbid_non_viable & !is_viable]

      private$.transitions <- transitions
      invisible(self)
    },

    # which blocks this program has, and which of its two questions each answers
    build_blocks = function() {
      targets <- private$.has_targets
      curved <- nrow(private$.curvature) > 0L

      # fmt: skip
      private$.blocks <- data.table::data.table(
        block = c(
          "initial",   "conservation", "closure",
          "forbidden", "rate_limits",  "rate_bounds",
          "historic",  "target",       "monotonicity",
          "shape",     "smoothness",   "fairness"
        ),
        is_enabled = c(
          # initial to closure
          TRUE,
          TRUE,
          TRUE,
          # forbidden to rate_bounds
          private$.transitions[, any(is_forbidden)],
          TRUE,
          targets,
          # historic to monotonicity
          targets && private$.mu_historic > 0,
          targets,
          targets && private$.scenario[, any(monotone_sign != 0)],
          # shape to fairness
          targets && curved && private$.scenario[, any(shape_binds)],
          targets && curved && private$.mu_smooth > 0,
          targets && private$.fair_weight > 0
        ),
        in_solution = c(
          TRUE, TRUE,  TRUE,
          TRUE, FALSE, TRUE,
          TRUE, TRUE,  TRUE,
          TRUE, TRUE,  TRUE
        ),
        in_reachability = c(
          TRUE,  TRUE,  TRUE,
          TRUE,  TRUE,  FALSE,
          FALSE, FALSE, FALSE,
          FALSE, FALSE, FALSE
        )
      )
      invisible(self)
    },

    # one row per decision variable, keyed by what it describes rather than by position
    build_variables = function() {
      enabled <- function(name) private$.blocks[block == name, is_enabled]
      per_step <- function(name) {
        private$.transitions[,
          .(block = name, id_period = private$.steps[["id_period"]]),
          by = .(id_lulc_anterior, id_lulc_posterior, id_trans)
        ]
      }
      per_state <- function(name, id_periods) {
        private$.scenario[, .(block = name, id_period = id_periods), by = id_lulc]
      }

      variables <- data.table::rbindlist(
        list(
          per_step("flow"),
          per_state("area", c(private$.anchor, private$.steps[["id_period"]])),
          if (enabled("rate_bounds")) per_step("rate_lower"),
          if (enabled("rate_bounds")) per_step("rate_upper"),
          if (enabled("historic")) per_step("historic"),
          if (enabled("shape")) per_state("shape", private$.curvature[["id_period"]]),
          if (enabled("smoothness")) per_state("smoothness", private$.curvature[["id_period"]]),
          if (enabled("target")) per_state("target_over", private$.horizon),
          if (enabled("target")) per_state("target_under", private$.horizon),
          if (enabled("fairness")) data.table::data.table(block = "fairness")
        ),
        use.names = TRUE,
        fill = TRUE
      )

      variables[, id_var := .I]
      data.table::setcolorder(
        variables,
        c("id_var", "block", "id_lulc", "id_lulc_anterior", "id_lulc_posterior", "id_trans")
      )
      data.table::setindex(variables, block)
      variables[]
    },

    # the area a step draws from, which is the state it starts at
    build_anterior_area = function() {
      private$.anterior_area <-
        private$vars("area")[
          private$.steps,
          on = .(id_period = id_period_prev),
          .(id_lulc_anterior = id_lulc, id_period = i.id_period, id_var_area = id_var)
        ]
      invisible(self)
    },

    ## Constraint helpers ----

    vars = function(name) private$.variables[block %chin% name],

    # the flow variables of a set of transitions, with the area variable they draw from and
    # the rate bounds that apply to them
    flow_with_area = function(transitions) {
      flows <- private$vars("flow")[
        transitions[, .(id_lulc_anterior, id_lulc_posterior, min_rate, max_rate, ref_rate)],
        on = .(id_lulc_anterior, id_lulc_posterior),
        nomatch = NULL
      ]
      flows[
        private$.anterior_area,
        id_var_area := i.id_var_area,
        on = .(id_lulc_anterior, id_period)
      ]
      flows
    },

    # expand a per-class table over the curvature periods, attaching the three area
    # variables a curvature row spans and the slack variable that absorbs its violation
    curvature_terms = function(classes, slack_block) {
      areas <- private$vars("area")[, .(id_lulc, id_period, id_var)]
      terms <- classes[, .(id_period = private$.curvature[["id_period"]]), by = names(classes)]
      terms[
        private$.curvature,
        on = "id_period",
        `:=`(
          id_period_prev = i.id_period_prev,
          id_period_next = i.id_period_next,
          ratio = i.ratio
        )
      ]

      terms[areas, id_var_here := i.id_var, on = .(id_lulc, id_period)]
      terms[areas, id_var_prev := i.id_var, on = .(id_lulc, id_period_prev = id_period)]
      terms[areas, id_var_next := i.id_var, on = .(id_lulc, id_period_next = id_period)]
      terms[
        private$vars(slack_block)[, .(id_lulc, id_period, id_var)],
        id_var_slack := i.id_var,
        on = .(id_lulc, id_period)
      ]

      # the change in per-year rate of change over consecutive steps, scaled by the length
      # of the earlier one
      terms[, `:=`(coefficient_prev = -1, coefficient_here = 1 + ratio, coefficient_next = -ratio)]
      terms
    },

    curvature_rows = function(terms, slack_coefficient, dir, id_row_offset = 0L) {
      rbind(
        terms[, .(
          id_row = .I + id_row_offset,
          id_var = id_var_prev,
          coefficient = coefficient_prev
        )],
        terms[, .(
          id_row = .I + id_row_offset,
          id_var = id_var_here,
          coefficient = coefficient_here
        )],
        terms[, .(
          id_row = .I + id_row_offset,
          id_var = id_var_next,
          coefficient = coefficient_next
        )],
        terms[, .(
          id_row = .I + id_row_offset,
          id_var = id_var_slack,
          coefficient = slack_coefficient
        )]
      )[
        terms[, .(id_row = .I + id_row_offset, rhs)],
        on = "id_row"
      ][, dir := ..dir][]
    },

    ## Objective ----

    objective_coefficients = function() {
      weights <- private$.scenario[, .(id_lulc, weight)]
      by_class <- function(name, mu) {
        private$vars(name)[weights, on = "id_lulc", .(id_var, coefficient = mu * weight)]
      }
      by_anterior <- function(name, mu) {
        private$vars(name)[
          weights,
          on = .(id_lulc_anterior = id_lulc),
          .(id_var, coefficient = mu * weight)
        ]
      }
      enabled <- function(name) private$.blocks[block == name, is_enabled]

      data.table::rbindlist(list(
        by_anterior("rate_lower", private$.lambda_bounds),
        by_anterior("rate_upper", private$.lambda_bounds),
        if (enabled("historic")) by_anterior("historic", private$.mu_historic),
        if (enabled("shape")) by_class("shape", private$.mu_shape),
        if (enabled("smoothness")) by_class("smoothness", private$.mu_smooth),
        if (enabled("target")) by_class("target_over", private$.mu_target),
        if (enabled("target")) by_class("target_under", private$.mu_target),
        if (enabled("fairness")) {
          data.table::data.table(
            id_var = private$vars("fairness")[["id_var"]],
            coefficient = private$.fair_weight
          )
        }
      ))
    },

    ## Reachability ----

    solve_reachability = function() {
      blocks <- private$.blocks[is_enabled == TRUE & in_reachability == TRUE, block]
      extreme_area <- function(id_var, direction) {
        solution <- private$run(
          objective = data.table::data.table(id_var = id_var, coefficient = 1),
          direction = direction,
          blocks = blocks
        )
        stopifnot(
          "the reachability program is infeasible; check bounds and lulc_data" = solution[[
            "status"
          ]] ==
            0L
        )
        solution[["objval"]] * private$.total
      }

      states <- private$vars("area")[id_period != private$.anchor, .(id_lulc, id_period, id_var)]
      states[private$.scenario, area_init := i.area_init, on = "id_lulc"]
      states[, `:=`(
        area_min = vapply(id_var, extreme_area, numeric(1L), direction = "min"),
        area_max = vapply(id_var, extreme_area, numeric(1L), direction = "max")
      )]
      states[order(id_period, id_lulc), .(id_lulc, id_period, area_init, area_min, area_max)]
    },

    # how far each target lies beyond what the observed transitions can deliver
    reachability_verdict = function() {
      verdict <- self$reachability[id_period == private$.horizon]
      verdict[private$.scenario, target := i.area_target, on = "id_lulc"]
      verdict[, asked := target - area_init]
      verdict[,
        achievable := data.table::fifelse(asked >= 0, area_max - area_init, area_min - area_init)
      ]

      # a class that cannot move at all in the direction asked is unreachable outright, not
      # unreachable by a very large multiple
      verdict[, immovable := 1e-6 * pmax(area_init, 1)]
      verdict[,
        ratio := data.table::fcase(
          abs(asked) <= abs(achievable) + immovable ,   1 ,
          abs(achievable) < immovable               , Inf ,
          default = abs(asked) / abs(achievable)
        )
      ]
      verdict[,
        verdict := cut(
          ratio,
          breaks = c(-Inf, 1 + 1e-9, 1.5, 3, Inf),
          labels = c("reachable", "near the edge", "outside history", "far outside history")
        )
      ]
      verdict[, .(
        id_lulc,
        area_init,
        area_min,
        area_max,
        target,
        asked,
        achievable,
        ratio,
        verdict
      )]
    },

    # the precheck quantifies and reports; it gates only above max_reachability_ratio,
    # because scenario targets are normative and a precheck that failed on every departure
    # from observed dynamics would abort every scenario
    assert_reachable_targets = function() {
      over_threshold <- private$reachability_verdict()[ratio > private$.max_reachability_ratio]
      if (nrow(over_threshold) == 0L) {
        return(invisible(self))
      }

      stop(glue::glue(
        "Targets for id_lulc {toString(over_threshold[['id_lulc']])} ",
        "ask for {toString(round(over_threshold[['ratio']], 2L))} ",
        "times the historically achievable change. ",
        "This is above the current max_reachability_ratio = ",
        "{private$.max_reachability_ratio}; see the reachability field."
      ))
    },

    ## Solution ----

    # the solved flows in cells, with the rate they imply and the envelope they sit in
    solved_flows = function() {
      values <- self$values
      flows <- values[block == "flow"]
      flows[
        private$.anterior_area,
        id_var_area := i.id_var_area,
        on = .(id_lulc_anterior, id_period)
      ]
      flows[values, share_anterior := i.value, on = .(id_var_area = id_var)]
      flows[
        private$.transitions,
        `:=`(is_viable = i.is_viable, min_rate = i.min_rate, max_rate = i.max_rate),
        on = .(id_lulc_anterior, id_lulc_posterior)
      ]

      if (private$.blocks[block == "rate_bounds", is_enabled]) {
        flows[
          values[block == "rate_lower"],
          dev_lower := i.value * private$.total,
          on = .(id_lulc_anterior, id_lulc_posterior, id_period)
        ]
        flows[
          values[block == "rate_upper"],
          dev_upper := i.value * private$.total,
          on = .(id_lulc_anterior, id_lulc_posterior, id_period)
        ]
      } else {
        flows[, `:=`(dev_lower = 0, dev_upper = 0)]
      }

      flows[, `:=`(
        flow = value * private$.total,
        area_anterior = share_anterior * private$.total,
        rate = data.table::fifelse(share_anterior > 1e-12, value / share_anterior, 0)
      )]
      flows[, count := round(flow)]
      # measured against the historic envelope itself, not against the margin around it:
      # the margin is a modelling convenience, and on a large class it is a lot of
      # unremarked flow
      flows[, `:=`(max_flow = max_rate * area_anterior, min_flow = min_rate * area_anterior)]
      flows[order(id_period, id_lulc_anterior, id_lulc_posterior)]
    },

    shape_diagnostics = function() {
      shapes <- private$.scenario[, .(id_lulc, shape, shape_binds)]
      if (!private$.blocks[block == "shape", is_enabled]) {
        return(shapes[, curvature_slack := NA_real_][])
      }
      slack <- self$values[
        block == "shape",
        .(curvature_slack = sum(value) * private$.total),
        by = id_lulc
      ]
      shapes[slack, curvature_slack := i.curvature_slack, on = "id_lulc"][]
    }
  )
)
