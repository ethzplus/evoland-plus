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

bounds <-
  trans_rate_bounds(obs_rates, periods, trans_meta) |>
  data.table::setindex(NULL)

# id_trans 3 is absent in period 2: that is a rate of 0, not a missing observation
# -> min_rate is 0
# the persistence "transitions" are not part of trans_meta_t, but the LP needs it
# -> complements of sum of all other rates with same id_lulc_anterior; id_trans is NA
# class 3 was never observed transitioning, so it persists entirely
bounds_ref <- data.table::fread(
  text = "
    id_trans id_lulc_anterior id_lulc_posterior is_viable   min_rate   max_rate   ref_rate
          NA                1                 1      TRUE 0.93000924 0.94999333 0.93995349
           1                1                 2      TRUE 0.05000667 0.06999076 0.06004651
           2                2                 1      TRUE 0.01999729 0.02000271 0.02000000
          NA                2                 2      TRUE 0.94000796 0.97999729 0.95981520
           3                2                 3      TRUE 0.00000000 0.03999464 0.02018096
           4                3                 1     FALSE 0.00000000 0.00000000 0.00000000
          NA                3                 3      TRUE 1.00000000 1.00000000 1.00000000
  "
)
expect_equal(bounds_ref, bounds)

# Reachability against a hand-computable case: only 1 -> 2 may move, at most 10% per step.
simple_bounds <-
  data.table::data.table(
    id_lulc_anterior = c(1L, 1L, 2L, 2L),
    id_lulc_posterior = c(1L, 2L, 1L, 2L),
    min_rate = 0,
    max_rate = c(1, 0.1, 0, 1),
    is_viable = TRUE
  )
init_area <-
  data.table::data.table(
    id_lulc = 1:2,
    area = c(5000, 2000)
  )

expect_equal(
  trans_rate_reachability(init_area, simple_bounds, n_steps = 2L),
  data.table::data.table(
    id_lulc = 1:2,
    area_init = c(5000, 2000),
    area_min = c(4050, 2000), # compound change: 10% of 5000, then 10% of 4500
    area_max = c(5000, 2950)
  )
)

# A three-class scenario the observed bounds can accommodate.
init_area <- data.table::data.table(id_lulc = 1:3, area = c(5000, 3000, 2000))
area_targets <- data.table::data.table(id_lulc = 1:3, area = c(4600, 3200, 2200))
shapes <- data.table::data.table(id_lulc = 1:3, shape = c("Instant decline", "Delayed growth", NA))

area_solution <- solve_trans_rates(init_area, area_targets, shapes, bounds, periods = periods)

expect_equal(area_solution[["status"]], 0L)
# mass is conserved at every step and the terminal areas meet the targets
expect_equal(area_solution[["areas"]][, sum(area), by = step][["V1"]], rep(10000, 3))
expect_equal(
  area_solution[["diagnostics"]][["target_error"]][["error"]],
  rep(0, 3),
  tolerance = 1e-6
)
# non-viable transitions have no trans_pot_t rows, so any flow on them would be lost
expect_equal(area_solution[["flows"]][is_viable == FALSE, sum(flow)], 0)
# each class moves monotonically towards its target
expect_true(
  area_solution[["areas"]][
    order(step),
    .(monotone = {
      # change in area -> rounded -> ensure only one sign per id_lulc
      area |> diff() |> round(6) |> sign() |> (\(x) length(unique(x)) == 1L)()
    }),
    by = id_lulc
  ][, all(monotone)]
)

# rates in [0, 1]
expect_true(all(area_solution[["rates"]][["rate"]] >= 0))
expect_true(
  area_solution[["rates"]][,
    .(lte_unity = all(sum(rate) <= 1 + 1e-9)), # tolerance
    by = .(id_lulc_anterior, step)
  ][, all(lte_unity)]
)

# Targets stated as a share of the landscape are grid independent, and rehydrate exactly.
share_targets <- data.table::data.table(id_lulc = 1:3, share = c(0.46, 0.32, 0.22))
share_solution <- solve_trans_rates(init_area, share_targets, shapes, bounds, periods = periods)
expect_equal(
  share_solution[["diagnostics"]][["target_error"]],
  area_solution[["diagnostics"]][["target_error"]],
  tolerance = 1e-6
)

# Elicited targets are normative and routinely lie outside the observed envelope, so the
# precheck reports rather than gates: the target is still met, by paying rate-bound slack.
stretch_targets <- data.table::data.table(id_lulc = 1:3, area = c(1000, 7000, 2000))
stretch_solution <- solve_trans_rates(init_area, stretch_targets, NULL, bounds, periods = periods)
expect_equal(
  stretch_solution[["diagnostics"]][["reachability"]][, verdict := as.character(verdict)],
  data.table::fread(
    text = '
     id_lulc area_init area_min area_max target asked achievable    ratio               verdict
           1      5000 4324.586 5118.816   1000 -4000  -675.4141 5.922293 "far outside history"
           2      3000 2650.815 3675.414   7000  4000   675.4141 5.922293 "far outside history"
           3      2000 2000.000 2249.165   2000     0   249.1654 1.000000           "reachable"
    '
  ),
  tolerance = 1e-6
)

expect_true(stretch_solution[["diagnostics"]][["flow_summary"]][["flow_above_max_rate"]] > 0)
expect_equal(round(stretch_solution[["diagnostics"]][["target_error"]][["error"]], 6), rep(0, 3))

# Only gross violations fail, on a configurable threshold.
expect_error(
  solve_trans_rates(
    init_area,
    stretch_targets,
    NULL,
    bounds,
    periods = periods,
    max_reachability_ratio = 2
  ),
  "max_reachability_ratio"
)

# One demand solution is usually written to several runs.
rates_t <- trans_rates_from_solution(area_solution, id_run = c(11L, 12L))
expect_inherits(rates_t, "trans_rates_t")
expect_equal(sort(unique(rates_t[["id_run"]])), c(11L, 12L))
expect_equal(sort(unique(rates_t[["id_period"]])), c(4L, 5L))
expect_false(any(is.na(rates_t[["id_trans"]])))

# The area trajectory has to be recoverable from the rate table alone, otherwise realised
# areas cannot be compared against the ones the solver promised.
replayed <- trans_rate_areas(init_area, rates_t[id_run == 11L], trans_meta)
lp_areas <- area_solution[["areas"]][,
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
flat <- solve_trans_rates(init_area, area_targets, shapes, bounds, periods = periods)
expect_equal(round(flat[["diagnostics"]][["shape"]][["curvature_slack"]], 6), rep(0, 3))

bent <- solve_trans_rates(
  init_area,
  area_targets,
  shapes,
  bounds,
  periods = periods,
  shape_strictness = 0.5,
  mu_smooth = 0
)
front_loaded <- bent[["areas"]][id_lulc == 1][order(step), diff(area)]
expect_true(abs(front_loaded[1]) > abs(front_loaded[length(front_loaded)]) + 1e-6)
