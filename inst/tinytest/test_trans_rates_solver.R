library(tinytest)

# Rates compound multiplicatively, so rescaling to a shorter period is not a division.
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

# The observed landscape the solver starts from: the last period that is not extrapolated.
lulc_data <-
  data.table::data.table(
    id_run = 0L,
    id_coord = seq_len(10000),
    id_period = 3L,
    id_lulc = rep(1:3, c(5000, 3000, 2000))
  ) |>
  as_lulc_data_t()

# Reachability against a hand-computable case: only 1 -> 2 may move, at most 10% per step.
simple_bounds <-
  data.table::data.table(
    id_lulc_anterior = c(1L, 1L, 2L, 2L),
    id_lulc_posterior = c(1L, 2L, 1L, 2L),
    min_rate = 0,
    max_rate = c(1, 0.1, 0, 1),
    is_viable = TRUE
  )
simple_lulc_data <-
  data.table::data.table(
    id_run = 0L,
    id_coord = seq_len(7000),
    id_period = 3L,
    id_lulc = rep(1:2, c(5000, 2000))
  ) |>
  as_lulc_data_t()

expect_equal(
  trans_rate_reachability(simple_lulc_data, simple_bounds, periods),
  data.table::data.table(
    id_lulc = c(1L, 2L, 1L, 2L),
    id_period = c(4L, 4L, 5L, 5L),
    area_init = c(5000L, 2000L, 5000L, 2000L),
    # compound change: 10% of 5000, then 10% of 4500
    area_min = c(4500, 2000, 4050, 2000),
    area_max = c(5000, 2500, 5000, 2950)
  )
)

# A three-class scenario the observed bounds can accommodate.
area_targets <- data.table::data.table(id_lulc = 1:3, area = c(4600, 3200, 2200))
shapes <- data.table::data.table(id_lulc = 1:3, shape = c("Instant decline", "Delayed growth", NA))

area_solution <- solve_trans_rates(lulc_data, area_targets, shapes, bounds, periods)

expect_inherits(area_solution, "trans_rate_lp")
expect_equal(area_solution[["status"]], 0L)
# every block that is enabled and part of a solve made it into the program
expect_equal(
  sort(area_solution[["block_summary"]][["block"]]),
  sort(area_solution[["blocks"]][is_enabled == TRUE, block])
)
# mass is conserved at every state and the terminal areas meet the targets
expect_equal(area_solution[["areas"]][, sum(area), by = id_period][["V1"]], rep(10000, 3))
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
    order(id_period),
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
    by = .(id_lulc_anterior, id_period)
  ][, all(lte_unity)]
)

# Targets stated as a share of the landscape are grid independent, and rehydrate exactly.
share_targets <- data.table::data.table(id_lulc = 1:3, share = c(0.46, 0.32, 0.22))
share_solution <- solve_trans_rates(lulc_data, share_targets, shapes, bounds, periods)
expect_equal(
  share_solution[["diagnostics"]][["target_error"]],
  area_solution[["diagnostics"]][["target_error"]],
  tolerance = 1e-6
)

# Elicited targets are normative and routinely lie outside the observed envelope, so the
# precheck reports rather than gates: the target is still met, by paying rate-bound slack.
stretch_targets <- data.table::data.table(id_lulc = 1:3, area = c(1000, 7000, 2000))
stretch_solution <- solve_trans_rates(lulc_data, stretch_targets, NULL, bounds, periods)
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
    lulc_data,
    stretch_targets,
    NULL,
    bounds,
    periods,
    max_reachability_ratio = 2
  ),
  "max_reachability_ratio"
)

# One demand solution is usually written to several runs.
area_solution[["id_run"]] <- 11L
rates_t <- area_solution[["trans_rates_t"]]
expect_inherits(rates_t, "trans_rates_t")
expect_equal(unique(rates_t[["id_run"]]), 11L)
expect_equal(sort(unique(rates_t[["id_period"]])), c(4L, 5L))
expect_false(any(is.na(rates_t[["id_trans"]])))
expect_true(all(rates_t[["id_trans"]] %in% trans_meta[is_viable == TRUE, id_trans]))

# The area trajectory has to be recoverable from the rate table alone, otherwise realised
# areas cannot be compared against the ones the solver promised.
expect_equal(
  trans_rate_areas(lulc_data, rates_t, trans_meta),
  area_solution[["areas"]],
  tolerance = 1e-6
)

# A straight line satisfies every one of the one-sided shape constraints, so shapes only
# bend the trajectory once a minimum curvature is demanded.
expect_equal(
  area_solution[["diagnostics"]][["shape"]][["curvature_slack"]],
  rep(0, 3),
  tolerance = 1e-6
)

bent <- solve_trans_rates(
  lulc_data,
  area_targets,
  shapes,
  bounds,
  periods,
  shape_strictness = 0.5,
  mu_smooth = 0
)
# "instant decline" front-loads the change: the first step moves more than the last
front_loaded <- bent[["areas"]][id_lulc == 1][order(id_period), diff(area)]
expect_true(abs(front_loaded[1]) > abs(front_loaded[length(front_loaded)]) + 1e-6)

# Without targets the program still answers what is reachable, but refuses to solve.
precheck <- trans_rate_lp$new(lulc_data = lulc_data, bounds = bounds, periods = periods)
expect_equal(nrow(precheck[["reachability"]]), 6L)
expect_false(precheck[["blocks"]][block == "target", is_enabled])
expect_error(precheck$solve(), "no targets")

# Every constraint block is added by the method of the same name, which is what add_all()
# relies on to assemble the program.
expect_true(
  all(
    paste0("add_", area_solution[["blocks"]][["block"]]) %in%
      names(trans_rate_lp[["public_methods"]])
  )
)
