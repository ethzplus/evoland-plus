library(tinytest)

alloc_params_t <- as_alloc_params_t(list(
  id_trans = 1L,
  mean_patch_size = 1.3,
  patch_size_variance = 1.4,
  patch_isometry = 0.2,
  frac_expander = 0.8,
  gof_window_size = 11,
  gof_fuzzy_similarity = 0.8
))

expect_silent(alloc_params_t)
expect_equal(nrow(alloc_params_t), 1L)
expect_silent(print(alloc_params_t))
expect_inherits(alloc_params_t, "alloc_params_t")

# Test create_alloc_params_t with simple synthetic rasters
# Create simple test rasters
# 5x5 grid with a simple transition pattern
lulc_ant <- terra::rast(
  ncols = 10,
  nrows = 5,
  xmin = 0,
  xmax = 10,
  ymin = 0,
  ymax = 5,
  crs = "epsg:4326"
)
lulc_post <- terra::rast(
  ncols = 10,
  nrows = 5,
  xmin = 0,
  xmax = 10,
  ymin = 0,
  ymax = 5,
  crs = "epsg:4326"
)

# Anterior: class 1 in center, class 2 elsewhere
# fmt: skip
terra::values(lulc_ant) <- c(
  2, 2, 2, 2, 2, 1, 1, 1, 1, 1,
  2, 1, 1, 1, 2, 1, 1, 1, 1, 1,
  2, 1, 1, 1, 2, 1, 1, 1, 1, 1,
  2, 1, 1, 1, 2, 1, 1, 1, 1, 1,
  2, 2, 2, 2, 2, 1, 1, 1, 1, 1
)

# Posterior: class 1 expanded to edges (transition 1->2)
# fmt: skip
terra::values(lulc_post) <- c(
  2, 2, 2, 2, 2, 1, 1, 1, 1, 1,
  2, 2, 2, 2, 2, 1, 1, 1, 2, 1,
  2, 2, 1, 2, 2, 1, 1, 1, 2, 1,
  2, 2, 2, 2, 2, 1, 1, 1, 2, 1,
  2, 2, 2, 2, 2, 1, 1, 1, 1, 1
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
    mean_patch_size = 3,
    patch_size_variance = NA_real_,
    patch_isometry = 2,
    frac_expander = 0.72727272,
    frac_patcher = 0.27272727
  ),
  tolerance = 1e-06
)

# Fractions should sum to 1 (or close to it due to rounding)
expect_equal(params$frac_expander + params$frac_patcher, 1, tolerance = 0.01)

# Test with no transitions
params_empty <- evoland:::compute_alloc_params_single(
  lulc_ant = lulc_ant,
  lulc_post = lulc_ant, # Same raster, no transitions
  id_lulc_ant = 1L,
  id_lulc_post = 2L
)

expect_equal(params_empty$mean_patch_size, 0)
expect_equal(params_empty$frac_expander, 0)
expect_equal(params_empty$frac_patcher, 0)
