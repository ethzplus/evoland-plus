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
  pattern = "same dimensions"
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

# Test tabular_to_raster with single period
test_coords <- create_coords_t_square(
  epsg = 4326,
  extent = terra::ext(c(xmin = 0, xmax = 2, ymin = 0, ymax = 2)),
  resolution = 1
)

test_lulc <- data.table::data.table(
  id_coord = test_coords$id_coord,
  id_lulc = rep(c(1L, 2L), length.out = nrow(test_coords))
)

rast_single <- tabular_to_raster(
  lulc_data = test_lulc,
  coords_t = test_coords,
  resolution = 0.5
)

expect_true(inherits(rast_single, "SpatRaster"))
expect_equal(terra::nlyr(rast_single), 1L)
expect_equal(names(rast_single), "id_lulc")

# Test tabular_to_raster with multiple periods
n_coords <- nrow(test_coords)
test_lulc_multi <- data.table::data.table(
  id_coord = rep(test_coords$id_coord, 2),
  id_lulc = rep(c(1L, 2L), length.out = n_coords * 2),
  id_period = rep(c(2000L, 2010L), each = n_coords)
)

rast_multi <- tabular_to_raster(
  lulc_data = test_lulc_multi,
  coords_t = test_coords,
  resolution = 0.5
)

expect_true(inherits(rast_multi, "SpatRaster"))
expect_equal(terra::nlyr(rast_multi), 2L)
expect_equal(names(rast_multi), c("period_2000", "period_2010"))

# Test tabular_to_raster with auto-detected resolution
test_coords_res <- test_coords
attr(test_coords_res, "resolution") <- 0.5

rast_auto_res <- tabular_to_raster(
  lulc_data = test_lulc,
  coords_t = test_coords_res,
  resolution = NULL
)

expect_true(inherits(rast_auto_res, "SpatRaster"))
