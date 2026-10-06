library(tinytest)

# greedy_fill_cpp accepts candidates in order, one change per cell, up to each quota
accepted <- evoland:::greedy_fill_cpp(
  cell = c(1L, 2L, 1L, 3L, 4L, 2L),
  trans = c(1L, 1L, 2L, 2L, 1L, 2L),
  quota = c(2L, 1L),
  n_cells = 4L
)
# cell 1 -> trans 1, cell 2 -> trans 1 (quota 1 full), cell 1 again: claimed, cell 3 -> trans 2
# (quota 2 full), the rest has no quota left
expect_identical(accepted, c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

# a zero quota accepts nothing
expect_false(any(evoland:::greedy_fill_cpp(1:3, rep(1L, 3), 0L, 3L)))

# indices out of range are errors, not silent writes
expect_error(evoland:::greedy_fill_cpp(5L, 1L, 1L, 4L), "cell index out of range")
expect_error(evoland:::greedy_fill_cpp(1L, 2L, 1L, 4L), "transition index out of range")
