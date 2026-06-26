library(tinytest)

# --------------------------------------------------------------------------
# Unit tests for the CLUMPY allocation backend (all in C++)
# --------------------------------------------------------------------------

# --- evoland:::gart_cpp() -------------------------------------------------------------

# Simple case: 2 states, equal probability
set.seed(42L)
P <- matrix(c(0.5, 0.5), nrow = 1L, ncol = 2L)
result <- evoland:::gart_cpp(P, c(1L, 2L))
expect_true(result %in% c(1L, 2L))

# With 100 cells, each gets exactly one state
set.seed(1L)
P100 <- matrix(rep(c(0.3, 0.7), each = 100L), nrow = 100L, ncol = 2L)
gart_results_many <- evoland:::gart_cpp(P100, c(10L, 20L))
expect_equal(length(gart_results_many), 100L)
expect_true(all(gart_results_many %in% c(10L, 20L)))

# "Stay" column: assign from_class when u > cumsum of all change probs
set.seed(7L)
P_stay <- matrix(c(0.0, 0.0, 1.0), nrow = 1L, ncol = 3L) # all stay
res_stay <- evoland:::gart_cpp(P_stay, c(1L, 2L, 3L))
expect_equal(res_stay, 3L)

# Negative / NaN probabilities are clamped to 0 (matches reference clumpy)
P_neg <- matrix(c(-1.0, 1.0), nrow = 1L, ncol = 2L)
expect_equal(evoland:::gart_cpp(P_neg, c(1L, 2L)), 2L)

# --- evoland:::sample_lognorm_area_cpp() ----------------------------------------------

set.seed(1L)
a <- evoland:::sample_lognorm_area_cpp(area_mean = 4, area_var = 2)
expect_true(a >= 1L)
expect_true(is.integer(a))

# With zero or NA mean, should return 1
expect_equal(evoland:::sample_lognorm_area_cpp(0, 1), 1L)
expect_equal(evoland:::sample_lognorm_area_cpp(NA_real_, 1), 1L)

# --- evoland:::raster_neighbors_cpp() -------------------------------------------------

nbrs <- evoland:::raster_neighbors_cpp(3L, 4L) # 3-row, 4-col raster (12 cells)
# Cell 1 is top-left: no above, no left
expect_equal(nbrs$above[1L], 0L)
expect_equal(nbrs$left[1L], 0L)
expect_equal(nbrs$below[1L], 5L) # cell 5 is directly below in a 4-col grid
expect_equal(nbrs$right[1L], 2L)

# Cell 12 is bottom-right: no below, no right
expect_equal(nbrs$below[12L], 0L)
expect_equal(nbrs$right[12L], 0L)

# --- evoland:::grow_patch_cpp() -------------------------------------------------------

# 4x4 raster, all class 1, no obstacles
n <- 16L
landscape <- as.integer(rep(1L, n))
ant_land <- as.integer(rep(1L, n))
probs <- rep(0.8, n)
nbrs <- evoland:::raster_neighbors_cpp(4L, 4L)

# Grow from pivot 1, target area 4
patch <- evoland:::grow_patch_cpp(
  landscape = landscape,
  ant_landscape = ant_land,
  probs = probs,
  nbr_above = nbrs$above,
  nbr_below = nbrs$below,
  nbr_left = nbrs$left,
  nbr_right = nbrs$right,
  pivot = 1L,
  target_area = 4L,
  from_class = 1L,
  to_class = 2L,
  eccentricity = 0.5,
  ncol = 4L
)
expect_true(length(patch) <= 4L)
expect_true(1L %in% patch) # pivot always included
expect_true(all(patch >= 1L & patch <= n))

# Pivot with wrong class -> empty result
landscape_wrong <- as.integer(rep(2L, n))
patch_empty <- evoland:::grow_patch_cpp(
  landscape = landscape_wrong,
  ant_landscape = ant_land,
  probs = probs,
  nbr_above = nbrs$above,
  nbr_below = nbrs$below,
  nbr_left = nbrs$left,
  nbr_right = nbrs$right,
  pivot = 1L,
  target_area = 4L,
  from_class = 1L,
  to_class = 2L,
  eccentricity = 0.5,
  ncol = 4L
)
expect_equal(length(patch_empty), 0L)

# --- evoland:::allocate_clumpy_cpp() --------------------------------------------------

nr <- 5L
nc <- 5L
ncell <- nr * nc
ant <- as.integer(rep(1L, ncell)) # all class 1
probs1col <- matrix(0.5, nrow = ncell, ncol = 1L) # one transition 1 -> 2

# uSAM: runs, returns full-length valid vector
set.seed(1L)
res_usam <- evoland:::allocate_clumpy_cpp(
  landscape = ant,
  ant_landscape = ant,
  nrow = nr,
  ncol = nc,
  from_classes = 1L,
  trans_from = 1L,
  trans_to = 2L,
  probs = probs1col,
  area_mean = 2.0,
  area_var = 1.0,
  elongation = 0.0,
  target_rate = 0.3,
  method = 0L,
  batch_size = 1L,
  rarefy = TRUE,
  shuffle = TRUE
)
expect_equal(length(res_usam), ncell)
expect_true(all(res_usam %in% c(1L, 2L)))

# uPAM: runs, returns full-length valid vector, respects a sane quota bound
set.seed(1L)
res_upam <- evoland:::allocate_clumpy_cpp(
  landscape = ant,
  ant_landscape = ant,
  nrow = nr,
  ncol = nc,
  from_classes = 1L,
  trans_from = 1L,
  trans_to = 2L,
  probs = probs1col,
  area_mean = 2.0,
  area_var = 1.0,
  elongation = 0.0,
  target_rate = 0.3,
  method = 1L,
  batch_size = 1L,
  rarefy = TRUE,
  shuffle = TRUE
)
expect_equal(length(res_upam), ncell)
expect_true(all(res_upam %in% c(1L, 2L)))
expect_true(sum(res_upam == 2L) <= ncell)

# Deterministic forcing: potential 1 + mono-pixel patches => every source cell
# transitions, regardless of the RNG draw (the GART rejection test is bypassed).
probs_forced <- matrix(1.0, nrow = ncell, ncol = 1L)
set.seed(123L)
res_forced <- evoland:::allocate_clumpy_cpp(
  landscape = ant,
  ant_landscape = ant,
  nrow = nr,
  ncol = nc,
  from_classes = 1L,
  trans_from = 1L,
  trans_to = 2L,
  probs = probs_forced,
  area_mean = 0.0,
  area_var = 0.0,
  elongation = 0.0,
  target_rate = 1.0,
  method = 0L,
  batch_size = 1L,
  rarefy = FALSE,
  shuffle = TRUE
)
expect_true(all(res_forced == 2L))

# Zero potential => nothing changes
probs_zero <- matrix(0.0, nrow = ncell, ncol = 1L)
set.seed(123L)
res_zero <- evoland:::allocate_clumpy_cpp(
  landscape = ant,
  ant_landscape = ant,
  nrow = nr,
  ncol = nc,
  from_classes = 1L,
  trans_from = 1L,
  trans_to = 2L,
  probs = probs_zero,
  area_mean = 2.0,
  area_var = 1.0,
  elongation = 0.0,
  target_rate = 0.3,
  method = 0L,
  batch_size = 1L,
  rarefy = TRUE,
  shuffle = TRUE
)
expect_true(all(res_zero == 1L))
