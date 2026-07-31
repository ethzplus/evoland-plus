library(tinytest)

# Rates compound multiplicatively, so rescaling to a shorter interval is not a division.
expect_equal(evoland:::rescale_trans_rate(0.1, 10, 10), 0.1)
expect_equal(evoland:::rescale_trans_rate(1 - 0.9^2, 10, 5), 0.1)
expect_equal(evoland:::rescale_trans_rate(0.81, 10, 5, is_persistence = TRUE), 0.9)

periods <-
  data.table::rowwiseDT(
    id_period=,  start_date=,  end_date=,   is_extrapolated=, # nolint
    0,          "2020-01-01", "2020-01-01", FALSE,
    1,          "1990-01-01", "1999-12-31", FALSE,
    2,          "2000-01-01", "2009-12-31", FALSE,
    3,          "2010-01-01", "2019-12-31", FALSE,
    4,          "2020-01-01", "2029-12-31", TRUE,
    5,          "2030-01-01", "2039-12-31", TRUE
  ) |>
  as_periods_t()

# period 0 is static and carries no transition; the first period has no predecessor
intervals <- evoland:::period_interval_years(periods)
expect_equal(intervals[["id_period"]], 1:5)
expect_true(is.na(intervals[["interval_years"]][1]))
expect_equal(round(intervals[["interval_years"]][-1]), rep(10, 4))

trans_meta <-
  data.table::data.table(
    id_trans = 1:4,
    id_lulc_anterior = c(1L, 2L, 2L, 3L),
    id_lulc_posterior = c(2L, 1L, 3L, 1L),
    cardinality = c(100L, 60L, 40L, 5L),
    frequency_rel = c(0.5, 0.3, 0.2, 0.02),
    frequency_abs = c(0.01, 0.006, 0.004, 0.0005),
    is_viable = c(TRUE, TRUE, TRUE, FALSE)
  ) |>
  as_trans_meta_t()

obs_rates <-
  data.table::rowwiseDT(
    id_run=, id_period=, id_trans=, count=,      rate=, # nolint
    0,       2,          1,         NA_integer_, 0.05,
    0,       2,          2,         NA_integer_, 0.02,
    0,       3,          1,         NA_integer_, 0.07,
    0,       3,          2,         NA_integer_, 0.02,
    0,       3,          3,         NA_integer_, 0.04
  ) |>
  as_trans_rates_t()

bounds <- trans_rate_bounds(obs_rates, periods, trans_meta)

expect_equal(attr(bounds, "step_years"), 10)
# transition 3 is absent in period 2: that is a rate of 0, not a missing observation
expect_equal(bounds[id_trans == 3, min_rate], 0)
expect_equal(round(bounds[id_trans == 3, max_rate], 4), 0.04)
# the diagonal is not part of trans_meta_t but the LP needs it
expect_equal(bounds[id_lulc_anterior == id_lulc_posterior, sort(id_lulc_anterior)], 1:3)
expect_true(all(is.na(bounds[id_lulc_anterior == id_lulc_posterior, id_trans])))
expect_false(bounds[id_trans == 4, is_viable])
# class 3 was never observed transitioning, so it persists entirely
expect_equal(bounds[id_lulc_anterior == 3 & id_lulc_posterior == 3, min_rate], 1)

# Reachability against a hand-computable case: only 1 -> 2 may move, at most 10% per step.
simple_bounds <- data.table::data.table(
  id_lulc_anterior = c(1L, 1L, 2L, 2L),
  id_lulc_posterior = c(1L, 2L, 1L, 2L),
  min_rate = 0,
  max_rate = c(1, 0.1, 0, 1),
  is_viable = TRUE
)
init_area <- data.table::data.table(id_lulc = 1:2, area = c(5000, 2000))

reach_1 <- trans_rate_reachability(init_area, simple_bounds, n_steps = 1L)
expect_equal(reach_1[["area_max"]], c(5000, 2500))
expect_equal(reach_1[["area_min"]], c(4500, 2000))

reach_2 <- trans_rate_reachability(init_area, simple_bounds, n_steps = 2L)
expect_equal(round(reach_2[["area_max"]], 6), c(5000, 2950))

# A three-class scenario the observed bounds can accommodate.
init_area <- data.table::data.table(id_lulc = 1:3, area = c(5000, 3000, 2000))
targets <- data.table::data.table(id_lulc = 1:3, area = c(4600, 3200, 2200))
shapes <- data.table::data.table(id_lulc = 1:3, shape = c("Instant decline", "Delayed growth", NA))

solution <- solve_trans_rates(init_area, targets, shapes, bounds, periods = periods)

expect_equal(solution[["status"]], 0L)
# mass is conserved at every step and the terminal areas meet the targets
expect_equal(solution[["areas"]][, sum(area), by = step][["V1"]], rep(10000, 3))
expect_equal(round(solution[["diagnostics"]][["target_error"]][["error"]], 6), rep(0, 3))
# non-viable transitions have no trans_pot_t rows, so any flow on them would be lost
expect_equal(solution[["flows"]][is_viable == FALSE, sum(flow)], 0)
# each class moves monotonically towards its target
expect_true(solution[["areas"]][order(step), all(diff(area) <= 1e-6), by = id_lulc][
  id_lulc == 1,
  V1
])
expect_true(solution[["areas"]][order(step), all(diff(area) >= -1e-6), by = id_lulc][
  id_lulc == 2,
  V1
])
# rates are the LP's own flow over its own predicted source area
expect_true(all(solution[["rates"]][["rate"]] >= 0))
expect_true(
  solution[["rates"]][,
    all(sum(rate) <= 1 + 1e-9),
    by = .(id_lulc_anterior, step)
  ][[
    "V1"
  ]] |>
    all()
)

# Targets stated as a share of the landscape are grid independent, and rehydrate exactly.
share_targets <- data.table::data.table(id_lulc = 1:3, share = c(0.46, 0.32, 0.22))
share_solution <- solve_trans_rates(init_area, share_targets, shapes, bounds, periods = periods)
expect_equal(
  round(share_solution[["diagnostics"]][["target_error"]][["area_final"]], 6),
  round(solution[["diagnostics"]][["target_error"]][["area_final"]], 6)
)

# Elicited targets are normative and routinely lie outside the observed envelope, so the
# precheck reports rather than gates: the target is still met, by paying rate-bound slack.
stretch <- data.table::data.table(id_lulc = 1:3, area = c(1000, 7000, 2000))
stretched <- solve_trans_rates(init_area, stretch, NULL, bounds, periods = periods)
expect_true(stretched[["diagnostics"]][["reachability"]][id_lulc == 1, ratio] > 2)
expect_true(stretched[["diagnostics"]][["flow_summary"]][["flow_above_max_rate"]] > 0)
expect_equal(round(stretched[["diagnostics"]][["target_error"]][["error"]], 6), rep(0, 3))

# Only gross violations fail, on a configurable threshold.
expect_error(
  solve_trans_rates(
    init_area,
    stretch,
    NULL,
    bounds,
    periods = periods,
    max_reachability_ratio = 2
  ),
  "max_reachability_ratio"
)

# One demand solution is usually written to several runs.
rates_t <- trans_rates_from_solution(solution, id_run = c(11L, 12L))
expect_inherits(rates_t, "trans_rates_t")
expect_equal(sort(unique(rates_t[["id_run"]])), c(11L, 12L))
expect_equal(sort(unique(rates_t[["id_period"]])), c(4L, 5L))
expect_false(any(is.na(rates_t[["id_trans"]])))
expect_silent(validate(rates_t))

# The area trajectory has to be recoverable from the rate table alone, otherwise realised
# areas cannot be compared against the ones the solver promised.
replayed <- trans_rate_areas(init_area, rates_t[id_run == 11L], trans_meta)
lp_areas <- solution[["areas"]][,
  .(id_lulc, id_period = data.table::fifelse(is.na(id_period), 3L, id_period), lp = area)
]
comparison <- merge(
  replayed[, .(id_lulc, id_period, replayed = area)],
  lp_areas,
  by = c("id_lulc", "id_period")
)
expect_equal(nrow(comparison), 9L)
expect_true(comparison[, max(abs(replayed - lp))] < 1e-6)

# A straight line satisfies every one of the one-sided shape constraints, so shapes only
# bend the trajectory once a minimum curvature is demanded.
flat <- solve_trans_rates(init_area, targets, shapes, bounds, periods = periods)
expect_equal(round(flat[["diagnostics"]][["shape"]][["curvature_slack"]], 6), rep(0, 3))

bent <- solve_trans_rates(
  init_area,
  targets,
  shapes,
  bounds,
  periods = periods,
  shape_strictness = 0.5,
  mu_smooth = 0
)
front_loaded <- bent[["areas"]][id_lulc == 1][order(step), diff(area)]
expect_true(abs(front_loaded[1]) > abs(front_loaded[length(front_loaded)]) + 1e-6)
