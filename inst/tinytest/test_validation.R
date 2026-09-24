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

# create_change_map is TRUE where the land use changed, FALSE where it did not, NA where
# either map is NA
map_before <- make_test_raster(values = c(rep(1, 90), rep(NA, 10)))
vals_after <- rep(1, 100)
vals_after[1:20] <- 2
map_after <- make_test_raster(values = vals_after)

change_map <- evoland:::create_change_map(map_before, map_after)
expect_true(terra::is.bool(change_map))
expect_equal(
  as.vector(terra::values(change_map)),
  c(rep(TRUE, 20), rep(FALSE, 70), rep(NA, 10))
)

# only the requested transition counts as change
map_mixed1 <- make_test_raster(values = c(rep(1, 30), rep(2, 30), rep(3, 40)))
map_mixed2 <- make_test_raster(values = c(rep(2, 30), rep(3, 30), rep(1, 40)))
change_map_filtered <- evoland:::create_change_map(map_mixed1, map_mixed2, 1, 2)
expect_equal(terra::global(change_map_filtered, "sum")[1, 1], 30)
expect_error(evoland:::create_change_map(map_mixed1, map_mixed2, from_class = 1))

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
expect_equal(result_one$similarity_observed_to_simulated, exp(-1 / 2))
expect_equal(result_one$similarity_simulated_to_observed, exp(-1 / 2))
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
expect_true(is.na(result_missing$similarity_simulated_to_observed))
