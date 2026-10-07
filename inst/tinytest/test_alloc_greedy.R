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

# the top-n rounds give the same result as walking the candidates one by one
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
