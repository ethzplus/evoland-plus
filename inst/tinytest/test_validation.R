library(tinytest)

# Helper function to create test rasters
make_test_raster <- function(ncol = 10, nrow = 10, values = NULL) {
  r <- terra::rast(
    ncols = ncol,
    nrows = nrow,
    xmin = 0,
    xmax = ncol,
    ymin = 0,
    ymax = nrow,
    crs = "epsg:4326"
  )

  if (!is.null(values)) {
    terra::values(r) <- values
  }

  r
}

# Test create_change_map with no changes
map_unchanged1 <- make_test_raster(values = rep(1, 100))
map_unchanged2 <- make_test_raster(values = rep(1, 100))

change_map_none <- create_change_map(map_unchanged1, map_unchanged2)

n_na <- terra::global(is.na(change_map_none), "sum", na.rm = FALSE)[1, 1]
expect_equal(n_na, 100)

# Test create_change_map with changes
vals_before <- rep(1, 100)
vals_after <- rep(1, 100)
vals_after[1:20] <- 2

map_before <- make_test_raster(values = vals_before)
map_after <- make_test_raster(values = vals_after)

change_map_some <- create_change_map(map_before, map_after)

n_non_na <- terra::global(!is.na(change_map_some), "sum", na.rm = FALSE)[1, 1]
expect_equal(n_non_na, 20)

changed_vals <- terra::values(change_map_some)[!is.na(terra::values(change_map_some))]
expect_true(all(changed_vals == 2))

# Test create_change_map with specific transition filter
vals_mixed1 <- c(rep(1, 30), rep(2, 30), rep(3, 40))
vals_mixed2 <- c(rep(2, 30), rep(3, 30), rep(1, 40))

map_mixed1 <- make_test_raster(values = vals_mixed1)
map_mixed2 <- make_test_raster(values = vals_mixed2)

change_map_filtered <- create_change_map(
  map_mixed1,
  map_mixed2,
  from_class = 1,
  to_class = 2
)

n_filtered <- terra::global(!is.na(change_map_filtered), "sum", na.rm = FALSE)[1, 1]
expect_equal(n_filtered, 30)

# Test calc_transition_similarity with perfect match
vals_initial <- rep(1, 100)
map_initial <- make_test_raster(values = vals_initial)

vals_observed <- rep(1, 100)
vals_observed[1:20] <- 2
map_observed <- make_test_raster(values = vals_observed)

map_simulated <- map_observed

result_trans <- calc_transition_similarity(
  initial_map = map_initial,
  observed_map = map_observed,
  simulated_map = map_simulated,
  from_class = 1,
  to_class = 2,
  window_size = 5L
)

expect_equal(result_trans$n_observed, 20)
expect_equal(result_trans$n_simulated, 20)
expect_equal(result_trans$similarity, 1.0, tolerance = 0.001)

# Test calc_transition_similarity with no transitions
map_static1 <- make_test_raster(values = rep(1, 100))
map_static2 <- make_test_raster(values = rep(1, 100))
map_static3 <- make_test_raster(values = rep(1, 100))

result_no_trans <- calc_transition_similarity(
  initial_map = map_static1,
  observed_map = map_static2,
  simulated_map = map_static3,
  from_class = 1,
  to_class = 2,
  window_size = 5L
)

expect_equal(result_no_trans$n_observed, 0)
expect_equal(result_no_trans$n_simulated, 0)
expect_true(is.na(result_no_trans$similarity))

# calc_transition_similarity only averages over changed cells, so the unchanged background
# does not inflate it: change placed far from the observed change scores 0
map_initial_large <- make_test_raster(nrow = 30, ncol = 30, values = rep(1, 900))
vals_obs_far <- rep(1, 900)
vals_obs_far[1:60] <- 2 # top two rows
vals_sim_far <- rep(1, 900)
vals_sim_far[841:900] <- 2 # bottom two rows
result_far <- calc_transition_similarity(
  initial_map = map_initial_large,
  observed_map = make_test_raster(nrow = 30, ncol = 30, values = vals_obs_far),
  simulated_map = make_test_raster(nrow = 30, ncol = 30, values = vals_sim_far),
  from_class = 1,
  to_class = 2,
  window_size = 5L
)
expect_equal(result_far$similarity, 0)

# a change displaced by one cell scores the decay at distance 1, in both directions
vals_obs_one <- rep(1, 900)
vals_obs_one[c(315, 615)] <- 2
vals_sim_one <- rep(1, 900)
vals_sim_one[c(316, 616)] <- 2
result_one <- calc_transition_similarity(
  initial_map = map_initial_large,
  observed_map = make_test_raster(nrow = 30, ncol = 30, values = vals_obs_one),
  simulated_map = make_test_raster(nrow = 30, ncol = 30, values = vals_sim_one),
  from_class = 1,
  to_class = 2,
  window_size = 5L,
  decay_divisor = 2
)
expect_equal(result_one$sim_obs_to_sim, exp(-1 / 2))
expect_equal(result_one$sim_sim_to_obs, exp(-1 / 2))
expect_equal(result_one$similarity, exp(-1 / 2))

# observed change with no simulated change scores 0 rather than NA
result_missing <- calc_transition_similarity(
  initial_map = map_initial_large,
  observed_map = make_test_raster(nrow = 30, ncol = 30, values = vals_obs_one),
  simulated_map = map_initial_large,
  from_class = 1,
  to_class = 2,
  window_size = 5L
)
expect_equal(result_missing$similarity, 0)
expect_true(is.na(result_missing$sim_sim_to_obs))

# calc_figure_of_merit, one cell of each kind:
# 1: 1 -> 2 simulated as 2 (hit)      2: 1 -> 2 simulated as 1 (miss)
# 3: 1 -> 1 simulated as 2 (false alarm)   4: 1 -> 1 simulated as 1 (correct persistence)
# 5: 2 -> 1 simulated as 3 (wrong hit)     6: 2 -> 2 simulated as 2 (correct persistence)
fom_initial <- c(1, 1, 1, 1, 2, 2)
fom_observed <- c(2, 2, 1, 1, 1, 2)
fom_simulated <- c(2, 1, 2, 1, 3, 2)

fom <- calc_figure_of_merit(fom_initial, fom_observed, fom_simulated)
expect_equal(
  unlist(fom[, .(hits, wrong_hits, misses, false_alarms)]),
  c(hits = 1, wrong_hits = 1, misses = 1, false_alarms = 1)
)
expect_equal(fom$figure_of_merit, 1 / 4)
expect_equal(fom$producers_accuracy, 1 / 3)
expect_equal(fom$users_accuracy, 1 / 3)
# random allocation within class 1 (4 cells, 2 observed and 2 simulated 1 -> 2) expects one
# hit over a union of 3; class 2 (2 cells, 2 -> 1 observed, 2 -> 3 simulated) no hit over 1.5
expect_equal(fom$figure_of_merit_null, 1 / 4.5)

fom_trans <- calc_figure_of_merit(fom_initial, fom_observed, fom_simulated, by_transition = TRUE)
expect_equal(fom_trans$id_lulc_anterior, c(1, 2, 2))
expect_equal(fom_trans$id_lulc_posterior, c(2, 1, 3))
expect_equal(fom_trans$figure_of_merit, c(1 / 3, 0, 0))
expect_equal(fom_trans$figure_of_merit_null, c(1 / 3, 0, 0))

# rasters give the same result as vectors, and an ensemble averages its members
fom_rast <- calc_figure_of_merit(
  make_test_raster(ncol = 3, nrow = 2, values = fom_initial),
  make_test_raster(ncol = 3, nrow = 2, values = fom_observed),
  make_test_raster(ncol = 3, nrow = 2, values = fom_simulated)
)
expect_equal(fom_rast, fom)

fom_ensemble <- calc_figure_of_merit(
  fom_initial,
  fom_observed,
  list(fom_simulated, fom_observed)
)
expect_equal(fom_ensemble$hits, (1 + 3) / 2)
expect_equal(fom_ensemble$false_alarms, 1 / 2)
expect_equal(
  fom_ensemble$figure_of_merit,
  2 / (2 + 0.5 + 0.5 + 0.5)
)
