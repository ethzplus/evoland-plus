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

# Test calc_fuzzy_similarity with perfect match
map1 <- make_test_raster(values = rep(1:4, length.out = 100))
map2 <- map1

result_perfect <- calc_fuzzy_similarity(
  map1,
  map2,
  window_size = 5L,
  use_exp_decay = FALSE
)

expect_equal(result_perfect$min_similarity, 1.0, tolerance = 0.001)
expect_equal(result_perfect$mean_sim1, 1.0, tolerance = 0.001)
expect_equal(result_perfect$mean_sim2, 1.0, tolerance = 0.001)

# Test calc_fuzzy_similarity with completely different maps
map_a <- make_test_raster(values = rep(1, 100))
map_b <- make_test_raster(values = rep(2, 100))

result_diff <- calc_fuzzy_similarity(
  map_a,
  map_b,
  window_size = 5L,
  use_exp_decay = FALSE
)

expect_true(result_diff$min_similarity < 0.1)

# Test calc_fuzzy_similarity with spatial shift
vals1 <- rep(1, 100)
vals1[45:55] <- 2
map_shift1 <- make_test_raster(values = vals1)

vals2 <- rep(1, 100)
vals2[46:56] <- 2
map_shift2 <- make_test_raster(values = vals2)

result_shift <- calc_fuzzy_similarity(
  map_shift1,
  map_shift2,
  window_size = 11L,
  use_exp_decay = TRUE
)

expect_true(result_shift$min_similarity > 0.7)

# Test window_size validation (must be odd)
map_test1 <- make_test_raster(values = rep(1, 100))

expect_error(
  calc_fuzzy_similarity(map_test1, map_test1, window_size = 10L),
  pattern = "window_size must be odd"
)

# Test dimension validation (maps must match)
map_small <- make_test_raster(ncol = 10, nrow = 10)
map_large <- make_test_raster(ncol = 20, nrow = 20)
terra::values(map_small) <- 1
terra::values(map_large) <- 1

expect_error(
  calc_fuzzy_similarity(map_small, map_large, window_size = 5L),
  pattern = "extents do not match"
)

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

# Test exponential decay vs constant weight
map_pattern1 <- make_test_raster(values = rep(1:4, length.out = 100))

vals_shifted <- rep(1:4, length.out = 100)
vals_shifted <- c(vals_shifted[2:100], vals_shifted[1])
map_pattern2 <- make_test_raster(values = vals_shifted)

result_exp_decay <- calc_fuzzy_similarity(
  map_pattern1,
  map_pattern2,
  window_size = 11L,
  use_exp_decay = TRUE,
  decay_divisor = 2.0
)

result_const_weight <- calc_fuzzy_similarity(
  map_pattern1,
  map_pattern2,
  window_size = 11L,
  use_exp_decay = FALSE
)

expect_true(result_exp_decay$min_similarity > 0)
expect_true(result_const_weight$min_similarity > 0)
# Both decay functions should produce valid similarity values
# The relationship between them depends on the spatial pattern
