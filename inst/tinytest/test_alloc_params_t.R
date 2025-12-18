library(tinytest)

alloc_params_t <- as_alloc_params_t(list(
  id_trans = 1L,
  alloc_params = list(
    list(
      mean_patch_size = 1.3,
      patch_size_variance = 1.4,
      patch_isometry = 0.2,
      share_expander = 0.8
    )
  ),
  goodness_of_fit = list(
    list(
      window_size = 11,
      fuzzy_similarity = 0.8
    )
  )
))

expect_silent(alloc_params_t)
expect_equal(nrow(alloc_params_t), 1L)
expect_silent(print(alloc_params_t))
expect_inherits(alloc_params_t, "alloc_params_t")

# Test expected column structure
expected_cols <- c("id_trans", "alloc_params", "goodness_of_fit")
expect_true(all(expected_cols %in% names(alloc_params_t)))

# Test that the nested list structures are preserved
expect_true(is.list(alloc_params_t$alloc_params[[1]]))
expect_true(is.list(alloc_params_t$goodness_of_fit[[1]]))
expect_equal(alloc_params_t$alloc_params[[1]]$mean_patch_size, 1.3)
expect_equal(alloc_params_t$goodness_of_fit[[1]]$window_size, 11)

# Test create_alloc_params_t with simple synthetic rasters
if (requireNamespace("landscapemetrics", quietly = TRUE)) {
  # Create simple test rasters
  # 5x5 grid with a simple transition pattern
  lulc_ant <- terra::rast(
    ncols = 5,
    nrows = 5,
    xmin = 0,
    xmax = 5,
    ymin = 0,
    ymax = 5,
    crs = "epsg:4326"
  )
  lulc_post <- terra::rast(
    ncols = 5,
    nrows = 5,
    xmin = 0,
    xmax = 5,
    ymin = 0,
    ymax = 5,
    crs = "epsg:4326"
  )

  # Anterior: class 1 in center, class 2 elsewhere
  # fmt: skip
  terra::values(lulc_ant) <- c(
    2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2
  )

  # Posterior: class 1 expanded to edges (transition 1->2)
  # fmt: skip
  terra::values(lulc_post) <- c(
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
  )

  # Test transition from class 1 to class 2
  params <- evoland:::compute_alloc_params_single(
    lulc_ant = lulc_ant,
    lulc_post = lulc_post,
    id_lulc_ant = 1L,
    id_lulc_post = 2L
  )
  expect_equal(
    params,
    list(
      mean_patch_size = 8e-04,
      patch_size_variance = NA_real_,
      patch_isometry = 0.9803922,
      perc_expander = 100,
      perc_patcher = 0
    ),
    tolerance = 1e-07
  )

  # Percentages should sum to 100 (or close to it due to rounding)
  expect_equal(params$perc_expander + params$perc_patcher, 100, tolerance = 0.01)

  # Test with no transitions
  params_empty <- evoland:::compute_alloc_params_single(
    lulc_ant = lulc_ant,
    lulc_post = lulc_ant, # Same raster, no transitions
    id_lulc_ant = 1L,
    id_lulc_post = 2L
  )

  expect_equal(params_empty$mean_patch_size, 0)
  expect_equal(params_empty$perc_expander, 0)
  expect_equal(params_empty$perc_patcher, 0)
}
