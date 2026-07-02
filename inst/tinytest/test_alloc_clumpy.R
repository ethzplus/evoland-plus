library(tinytest)

# --------------------------------------------------------------------------
# Unit tests for the CLUMPY allocation backend (all in C++)
# --------------------------------------------------------------------------

# --- evoland:::must_cpp() (Multinomial Sampling Test) ---------------------------------------------------

# Simple case: 2 states, equal probability
set.seed(42L)
P <- matrix(c(0.5, 0.5), nrow = 1L, ncol = 2L)
result <- evoland:::must_cpp(P, c(1L, 2L))
expect_equal(result, 2L)

# With 100 cells, each gets exactly one state
set.seed(1L)
P100 <- matrix(rep(c(0.3, 0.7), each = 100L), nrow = 100L, ncol = 2L)
must_results_many <- evoland:::must_cpp(P100, c(10L, 20L))
expect_equal(
  must_results_many,
  # fmt: skip
  c(
    10, 20, 20, 20, 10, 20, 20, 20, 20, 10, 10, 10, 20, 20, 20, 20, 20, 20, 20,
    20, 20, 10, 20, 10, 10, 20, 10, 20, 20, 20, 20, 20, 20, 10, 20, 20, 20, 10,
    20, 20, 20, 20, 20, 20, 20, 20, 10, 20, 20, 20, 20, 20, 20, 10, 10, 10, 20,
    20, 20, 20, 20, 10, 20, 20, 20, 10, 20, 20, 10, 20, 20, 20, 20, 20, 20, 20,
    20, 20, 20, 20, 20, 20, 20, 20, 20, 10, 20, 10, 10, 10, 10, 10, 20, 20, 20,
    20, 20, 20, 20, 20
  )
)


# "Stay" column: assign from_class when u > cumsum of all change probs
P_stay <- matrix(c(0.0, 0.0, 1.0), nrow = 1L, ncol = 3L) # all stay
res_stay <- evoland:::must_cpp(P_stay, c(1L, 2L, 3L))
expect_equal(res_stay, 3L)

# Negative / NaN probabilities are clamped to 0 (matches reference clumpy)
P_neg <- matrix(c(-1.0, 1.0), nrow = 1L, ncol = 2L)
expect_equal(evoland:::must_cpp(P_neg, c(1L, 2L)), 2L)

# --- area samplers ----------------------------------------------------------

set.seed(1L)
expect_equal(evoland:::sample_lognorm_area_cpp(area_mean = 4, area_var = 2), 3)
expect_equal(evoland:::sample_lognorm_area_cpp(0, 1), 1L)
expect_equal(evoland:::sample_lognorm_area_cpp(NA_real_, 1), 1L)

set.seed(666L)
expect_equal(evoland:::sample_normal_area_cpp(area_mean = 4, area_var = 2), 5)
expect_equal(evoland:::sample_normal_area_cpp(0, 1), 1L)
# Normal with zero variance returns the (rounded) mean
expect_equal(evoland:::sample_normal_area_cpp(5, 0), 5L)

# --- evoland:::raster_neighbors_cpp() ---------------------------------------

expect_equal(
  evoland:::raster_neighbors_cpp(3L, 4L), # 3-row, 4-col raster (12 cells)
  list(
    above = c(0L, 0L, 0L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
    below = c(5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 0L, 0L, 0L, 0L),
    left = c(0L, 1L, 2L, 3L, 0L, 5L, 6L, 7L, 0L, 9L, 10L, 11L),
    right = c(2L, 3L, 4L, 0L, 6L, 7L, 8L, 0L, 10L, 11L, 12L, 0L)
  )
)

# --- evoland:::grow_patch_cpp() ---------------------------------------------

# 4x4 raster, all class 1, no obstacles
n <- 16L
landscape <- as.integer(rep(1L, n))
ant_land <- as.integer(rep(1L, n))
probs <- rep(0.8, n)
nbrs <- evoland:::raster_neighbors_cpp(4L, 4L)

patch <- evoland:::grow_patch_cpp(
  landscape = landscape, # changes landscape by reference
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
  elongation = 0.5,
  ncol = 4L
)
expect_equal(patch, c(1L, 2L, 5L, 3L))
expect_equal(
  landscape,
  c(2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
)

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
  elongation = 0.5,
  ncol = 4L
)
expect_length(patch_empty, 0L)

# avoid_aggregation: a patch that would touch an existing patch fails entirely.
# 1x4 row [1,1,2,1] (cell 3 is an existing class-2 patch, originally class 1).
nbr14 <- evoland:::raster_neighbors_cpp(1L, 4L)
land_adj <- as.integer(c(1L, 1L, 2L, 1L))
ant_adj <- as.integer(c(1L, 1L, 1L, 1L))
probs_adj <- rep(1.0, 4L)

# With avoid_aggregation = TRUE, growing from cell 1 toward area 3 hits cell 3
# (a foreign patch) and fails -> nothing allocated.
patch_agg <- evoland:::grow_patch_cpp(
  landscape = land_adj,
  ant_landscape = ant_adj,
  probs = probs_adj,
  nbr_above = nbr14$above,
  nbr_below = nbr14$below,
  nbr_left = nbr14$left,
  nbr_right = nbr14$right,
  pivot = 1L,
  target_area = 3L,
  from_class = 1L,
  to_class = 2L,
  elongation = 0.0,
  ncol = 4L,
  avoid_aggregation = TRUE
)
expect_length(patch_agg, 0L)

# With avoid_aggregation = FALSE, it grows as far as it can (cells 1 and 2).
land_adj2 <- as.integer(c(1L, 1L, 2L, 1L))
patch_noagg <- evoland:::grow_patch_cpp(
  landscape = land_adj2,
  ant_landscape = ant_adj,
  probs = probs_adj,
  nbr_above = nbr14$above,
  nbr_below = nbr14$below,
  nbr_left = nbr14$left,
  nbr_right = nbr14$right,
  pivot = 1L,
  target_area = 3L,
  from_class = 1L,
  to_class = 2L,
  elongation = 0.0,
  ncol = 4L,
  avoid_aggregation = FALSE
)
expect_equal(sort(patch_noagg), c(1L, 2L))

# --- evoland:::allocate_clumpy_cpp() ----------------------------------------

# Sparse potentials are passed as per-transition lists. Helper: one transition
# over all `ncell` cells with constant potential `p`.
sparse_const <- function(ncell, p) {
  list(cell = list(seq_len(ncell)), value = list(rep(p, ncell)))
}

nr <- 5L
nc <- 5L
ncell <- nr * nc
ant <- as.integer(rep(1L, ncell)) # all class 1
sp <- sparse_const(ncell, 0.5) # one transition 1 -> 2, potential 0.5

# uSAM (method 0): mono-pixel single pass
set.seed(1L)
res_usam <- evoland:::allocate_clumpy_cpp(
  landscape = ant,
  nrow = nr,
  ncol = nc,
  trans_from = 1L,
  trans_to = 2L,
  prob_cell = sp$cell,
  prob_value = sp$value,
  area_mean = 1.0,
  area_var = 0.0,
  elongation = 0.0,
  target_rate = 0.3,
  method = 0L,
  batch_size = 1L,
  rarefy = TRUE,
  shuffle = TRUE,
  avoid_aggregation = FALSE,
  area_dist = 0L
)
expect_equal(
  res_usam,
  # fmt: skip
  c(
    2L, 2L, 1L, 1L, 2L,
    1L, 1L, 1L, 1L, 2L,
    2L, 2L, 1L, 2L, 1L,
    2L, 1L, 1L, 2L, 1L,
    1L, 2L, 1L, 2L, 2L
  )
)

# uPAM (method 1): iterative, multi-pixel, quota
set.seed(1L)
res_upam <- evoland:::allocate_clumpy_cpp(
  landscape = ant,
  nrow = nr,
  ncol = nc,
  trans_from = 1L,
  trans_to = 2L,
  prob_cell = sp$cell,
  prob_value = sp$value,
  area_mean = 2.0,
  area_var = 1.0,
  elongation = 0.0,
  target_rate = 0.3,
  method = 1L,
  batch_size = 1L,
  rarefy = TRUE,
  shuffle = TRUE,
  avoid_aggregation = TRUE,
  area_dist = 0L
)
expect_equal(
  res_upam,
  # fmt: skip
  c(
    1L, 1L, 1L, 2L, 1L,
    1L, 2L, 1L, 2L, 1L,
    1L, 2L, 1L, 1L, 1L,
    1L, 1L, 1L, 1L, 2L,
    1L, 1L, 1L, 1L, 2L
  )
)

# Deterministic forcing: potential 1 + uSAM => every source cell transitions,
# regardless of the RNG draw (the MuST rejection step is bypassed).
sp1 <- sparse_const(ncell, 1.0)
set.seed(123L)
res_forced <- evoland:::allocate_clumpy_cpp(
  landscape = ant,
  nrow = nr,
  ncol = nc,
  trans_from = 1L,
  trans_to = 2L,
  prob_cell = sp1$cell,
  prob_value = sp1$value,
  area_mean = 1.0,
  area_var = 0.0,
  elongation = 0.0,
  target_rate = 1.0,
  method = 0L,
  batch_size = 1L,
  rarefy = FALSE,
  shuffle = TRUE,
  avoid_aggregation = FALSE,
  area_dist = 0L
)
expect_true(all(res_forced == 2L))

# Empty sparse potentials (no entries) => nothing changes
set.seed(123L)
res_zero <- evoland:::allocate_clumpy_cpp(
  landscape = ant,
  nrow = nr,
  ncol = nc,
  trans_from = 1L,
  trans_to = 2L,
  prob_cell = list(integer(0)),
  prob_value = list(numeric(0)),
  area_mean = 2.0,
  area_var = 1.0,
  elongation = 0.0,
  target_rate = 0.3,
  method = 1L,
  batch_size = 1L,
  rarefy = TRUE,
  shuffle = TRUE,
  avoid_aggregation = TRUE,
  area_dist = 0L
)
expect_true(all(res_zero == 1L))

# avoid_aggregation: rejecting merging patches leaves fewer cells allocated.
# Deterministic setup so the comparison is a true invariant (not RNG/quota/FP
# dependent): a 1x5 row, forced potential 1 (deterministic MuST), area exactly 2
# (normal with variance 0), no shuffle, quota = everything (target_rate 1).
#   - without avoidance, patches tile the row -> all 5 cells change;
#   - with avoidance, the middle patch (cell 3) would touch cell 2's patch, so it
#     is rejected and cell 3 stays -> 4 cells change.
ant_row <- as.integer(rep(1L, 5L))
row_cell <- list(1:5L)
row_val <- list(rep(1.0, 5L))
run_row <- function(avoid_agg) {
  set.seed(1L)
  evoland:::allocate_clumpy_cpp(
    landscape = ant_row,
    nrow = 1L,
    ncol = 5L,
    trans_from = 1L,
    trans_to = 2L,
    prob_cell = row_cell,
    prob_value = row_val,
    area_mean = 2.0,
    area_var = 0.0,
    elongation = 0.0,
    target_rate = 1.0,
    method = 1L,
    batch_size = 1L,
    rarefy = FALSE,
    shuffle = FALSE,
    avoid_aggregation = avoid_agg,
    area_dist = 1L
  )
}
expect_equal(run_row(avoid_agg = FALSE), c(2, 2, 2, 2, 2))
expect_equal(run_row(avoid_agg = TRUE), c(2, 2, 1, 2, 2))

# A sparse subset: only some cells carry a potential; only those can change.
set.seed(9L)
some_cells <- c(1L, 7L, 13L, 19L, 25L)
res_subset <- evoland:::allocate_clumpy_cpp(
  landscape = ant,
  nrow = nr,
  ncol = nc,
  trans_from = 1L,
  trans_to = 2L,
  prob_cell = list(some_cells),
  prob_value = list(rep(1.0, length(some_cells))),
  area_mean = 1.0,
  area_var = 0.0,
  elongation = 0.0,
  target_rate = 1.0,
  method = 0L,
  batch_size = 1L,
  rarefy = FALSE,
  shuffle = TRUE,
  avoid_aggregation = FALSE,
  area_dist = 0L
)
# forced potential 1 on exactly those cells (mono-pixel) => exactly they change
expect_equal(which(res_subset == 2L), some_cells)

# Auto batch (batch_size = 0): scales with the pool, runs and stays valid.
big2 <- 40L
antc <- as.integer(rep(1L, big2 * big2))
spc <- sparse_const(big2 * big2, 0.4)
set.seed(11L)
res_auto <- evoland:::allocate_clumpy_cpp(
  landscape = antc,
  nrow = big2,
  ncol = big2,
  trans_from = 1L,
  trans_to = 2L,
  prob_cell = spc$cell,
  prob_value = spc$value,
  area_mean = 4.0,
  area_var = 2.0,
  elongation = 0.0,
  target_rate = 0.3,
  method = 1L,
  batch_size = 0L,
  rarefy = TRUE,
  shuffle = TRUE,
  avoid_aggregation = TRUE,
  area_dist = 0L
)
expect_length(res_auto, big2 * big2)
# quota-bounded: changed count should be near rate * pool, not wildly over
expect_true(sum(res_auto == 2L) <= ceiling(0.3 * big2 * big2) + 10L)
