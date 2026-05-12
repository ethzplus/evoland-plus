library(tinytest)

# --------------------------------------------------------------------------
# Unit tests for the CLUMPY allocation helpers
# --------------------------------------------------------------------------

# --- gart() -----------------------------------------------------------------

# Simple case: 2 states, equal probability
set.seed(42L)
P <- matrix(c(0.5, 0.5), nrow = 1L, ncol = 2L)
result <- evoland:::gart(P, c(1L, 2L))
expect_true(result %in% c(1L, 2L))

# With 100 cells, each gets exactly one state
set.seed(1L)
P100 <- matrix(rep(c(0.3, 0.7), each = 100L), nrow = 100L, ncol = 2L)
gart_results_many <- evoland:::gart(P100, c(10L, 20L))
expect_equal(length(gart_results_many), 100L)
expect_true(all(gart_results_many %in% c(10L, 20L)))

# "Stay" column: assign from_class when u > cumsum of all change probs
set.seed(7L)
P_stay <- matrix(c(0.0, 0.0, 1.0), nrow = 1L, ncol = 3L)  # all stay
res_stay <- evoland:::gart(P_stay, c(1L, 2L, 3L))
expect_equal(res_stay, 3L)

# --- sample_lognorm_area() --------------------------------------------------

set.seed(1L)
a <- evoland:::sample_lognorm_area(area_mean = 4, area_var = 2)
expect_true(a >= 1L)
expect_true(is.integer(a))

# With zero or NA mean, should return 1
expect_equal(evoland:::sample_lognorm_area(0,    1), 1L)
expect_equal(evoland:::sample_lognorm_area(NA,   1), 1L)

# --- raster_neighbors() -----------------------------------------------------

nbrs <- evoland:::raster_neighbors(3L, 4L)  # 3-row, 4-col raster (12 cells)
# Cell 1 is top-left: no above, no left
expect_equal(nbrs$above[1L], 0L)
expect_equal(nbrs$left[1L],  0L)
expect_equal(nbrs$below[1L], 5L)   # cell 5 is directly below in a 4-col grid
expect_equal(nbrs$right[1L], 2L)

# Cell 12 is bottom-right: no below, no right
expect_equal(nbrs$below[12L], 0L)
expect_equal(nbrs$right[12L], 0L)

# --- grow_patch_cpp() -------------------------------------------------------

# 4x4 raster, all class 1, no obstacles
n <- 16L
landscape   <- as.integer(rep(1L, n))
ant_land    <- as.integer(rep(1L, n))
probs       <- rep(0.8, n)
nbrs        <- evoland:::raster_neighbors(4L, 4L)

# Grow from pivot 1, target area 4
patch <- grow_patch_cpp(
  landscape    = landscape,
  ant_landscape = ant_land,
  probs        = probs,
  nbr_above    = nbrs$above,
  nbr_below    = nbrs$below,
  nbr_left     = nbrs$left,
  nbr_right    = nbrs$right,
  pivot        = 1L,
  target_area  = 4L,
  from_class   = 1L,
  to_class     = 2L,
  eccentricity = 0.5,
  ncol         = 4L
)
expect_true(length(patch) <= 4L)
expect_true(1L %in% patch)  # pivot always included
expect_true(all(patch >= 1L & patch <= n))

# Pivot with wrong class → empty result
landscape_wrong <- as.integer(rep(2L, n))
patch_empty <- grow_patch_cpp(
  landscape    = landscape_wrong,
  ant_landscape = ant_land,
  probs        = probs,
  nbr_above    = nbrs$above,
  nbr_below    = nbrs$below,
  nbr_left     = nbrs$left,
  nbr_right    = nbrs$right,
  pivot        = 1L,
  target_area  = 4L,
  from_class   = 1L,
  to_class     = 2L,
  eccentricity = 0.5,
  ncol         = 4L
)
expect_equal(length(patch_empty), 0L)
