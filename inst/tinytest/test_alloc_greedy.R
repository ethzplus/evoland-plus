library(tinytest)

# greedy_fill accepts candidates in row order, one change per cell, up to each quota
candidates <- data.table::data.table(
  id_trans = c(1L, 1L, 2L, 2L, 1L, 2L),
  id_coord = c(1L, 2L, 1L, 3L, 4L, 2L)
)
quota <- data.table::data.table(id_trans = 1:2, quota = c(2L, 1L))
# cell 1 -> trans 1, cell 2 -> trans 1 (quota 2 full), cell 1 again: claimed, cell 3 -> trans 2
# (quota 1 full), the rest has no quota left
expect_identical(
  evoland:::greedy_fill(candidates, quota),
  c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
)

# a zero quota accepts nothing
expect_false(any(evoland:::greedy_fill(
  data.table::data.table(id_trans = 1L, id_coord = 1:3),
  data.table::data.table(id_trans = 1L, quota = 0L)
)))

# greedy_fill gives the same result as walking the candidates one by one in R
walk <- function(candidates, quota) {
  claimed <- integer(0)
  remaining <- stats::setNames(quota$quota, quota$id_trans)
  accepted <- logical(nrow(candidates))
  for (i in seq_len(nrow(candidates))) {
    t <- as.character(candidates$id_trans[i])
    if (candidates$id_coord[i] %in% claimed || remaining[[t]] <= 0L) {
      next
    }
    claimed <- c(claimed, candidates$id_coord[i])
    remaining[[t]] <- remaining[[t]] - 1L
    accepted[i] <- TRUE
  }
  accepted
}
set.seed(42)
identical_to_walk <- vapply(
  1:200,
  function(k) {
    n_coord <- sample(5:60, 1L)
    n_trans <- sample(1:5, 1L)
    cand <- unique(data.table::data.table(
      id_trans = sample(n_trans, 2L * n_coord, TRUE),
      id_coord = sample(n_coord, 2L * n_coord, TRUE)
    ))
    cand[, value := round(stats::runif(.N), 1)]
    # alternate joint and ordered arbitration orders
    if (k %% 2L) {
      data.table::setorder(cand, -value, id_trans, id_coord)
    } else {
      data.table::setorder(cand, id_trans, -value, id_coord)
    }
    quota <- data.table::data.table(id_trans = 1:n_trans, quota = sample(0:15, n_trans, TRUE))
    identical(evoland:::greedy_fill(cand, quota), walk(cand, quota))
  },
  logical(1)
)
expect_true(all(identical_to_walk))

# a candidate transition missing from quota is an error
expect_error(
  evoland:::greedy_fill(
    data.table::data.table(id_trans = 2L, id_coord = 1L),
    data.table::data.table(id_trans = 1L, quota = 1L)
  ),
  "quota must list every id_trans"
)

# the C++ kernel rejects indices out of range instead of writing past its arrays
expect_error(evoland:::greedy_fill_cpp(5L, 1L, 1L, 4L), "cell index out of range")
expect_error(evoland:::greedy_fill_cpp(1L, 2L, 1L, 4L), "transition index out of range")

# the greedy allocator on a hand-built allocation context, without a database: four cells of
# class 1, transition 1 -> 2 with a quota of 2 goes to the two most probable cells
ctx <- list(
  anterior = data.table::data.table(id_coord = 1:5, id_lulc = c(1L, 1L, 1L, 1L, 3L)),
  viable_trans = data.table::data.table(
    id_trans = 1L,
    id_lulc_anterior = 1L,
    id_lulc_posterior = 2L
  ),
  quota = data.table::data.table(id_trans = 1L, rate = 0.5, n_anterior = 4L, quota = 2L),
  potentials = data.table::data.table(id_trans = 1L, id_coord = 1:4, value = c(0.1, 0.4, 0.3, 0.2))
)
expect_identical(
  alloc_greedy_spec()$one_period(ctx)[order(id_coord)],
  data.table::data.table(id_coord = 1:5, id_lulc = c(1L, 2L, 2L, 1L, 3L))
)
# demand beyond the candidates is reported, not exceeded
ctx$quota[, quota := 5L]
expect_warning(
  short <- alloc_greedy_spec()$one_period(ctx),
  "allocated 4 of 5 cells"
)
expect_equal(short[id_coord == 5L, id_lulc], 3L)
expect_error(alloc_greedy_spec("ordered", order = 9L)$one_period(ctx), "viable id_trans")
