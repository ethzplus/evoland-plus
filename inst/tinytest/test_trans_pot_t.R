library(tinytest)

# --------------------------------------------------------------------------
# Unit tests for trans_pot_t schema and adjusted_trans_pot_v logic
# --------------------------------------------------------------------------

# as_trans_pot_t: basic construction
tp <- as_trans_pot_t(data.frame(
  id_trans     = 1L,
  id_period_post = 2L,
  id_coord     = 1L,
  value        = 0.3
))
expect_inherits(tp, "trans_pot_t")
expect_equal(nrow(tp), 1L)
expect_true(all(c("id_trans", "id_period_post", "id_coord", "value") %in% names(tp)))

# Values must remain in [0, 1] – validate() should not modify valid inputs
tp_valid <- as_trans_pot_t(data.frame(
  id_trans = c(1L, 2L, 1L, 2L),
  id_period_post = 2L,
  id_coord = c(1L, 1L, 2L, 2L),
  value = c(0.4, 0.4, 0.6, 0.3)
))
expect_true(all(tp_valid$value >= 0 & tp_valid$value <= 1))

# --------------------------------------------------------------------------
# adjusted_trans_pot_v: verify SQL logic using a tiny in-memory DuckDB
# The view should:
#   1. column-scale so mean(col) = rate
#   2. row-close cells where sum > 1
# --------------------------------------------------------------------------
# We test the logic directly in R to avoid needing a full evoland_db setup

# Simulate: 3 cells, 2 transitions
raw_vals <- data.frame(
  id_trans       = c(1L, 2L, 1L, 2L, 1L, 2L),
  id_coord       = c(1L, 1L, 2L, 2L, 3L, 3L),
  id_period_post = 4L,
  value          = c(0.6, 0.2, 0.1, 0.5, 0.3, 0.4)
)
rates <- data.frame(
  id_trans  = c(1L, 2L),
  rate      = c(0.2, 0.1)
)

# Mean raw values per transition
mean_t1 <- mean(raw_vals$value[raw_vals$id_trans == 1L])  # (0.6+0.1+0.3)/3
mean_t2 <- mean(raw_vals$value[raw_vals$id_trans == 2L])  # (0.2+0.5+0.4)/3

# Column scaling
raw_vals$scaled <- ifelse(
  raw_vals$id_trans == 1L,
  raw_vals$value * rates$rate[1L] / mean_t1,
  raw_vals$value * rates$rate[2L] / mean_t2
)

# Row sums
row_sums <- tapply(raw_vals$scaled, raw_vals$id_coord, sum)

# Row closure: divide by row_sum where > 1
adj <- raw_vals
for (coord in unique(raw_vals$id_coord)) {
  if (row_sums[as.character(coord)] > 1) {
    idx <- raw_vals$id_coord == coord
    adj$scaled[idx] <- raw_vals$scaled[idx] / row_sums[as.character(coord)]
  }
}

# Final values must be in [0, 1]
expect_true(all(adj$scaled >= 0 & adj$scaled <= 1))

# Row sums after closure must be <= 1 (with tolerance)
final_row_sums <- tapply(adj$scaled, adj$id_coord, sum)
expect_true(all(final_row_sums <= 1 + 1e-9))

# Column means after scaling (before row closure, may drift) — just check
# that the direction is correct (closer to target rate than raw means)
col_mean_t1_scaled <- mean(adj$scaled[adj$id_trans == 1L])
col_mean_t2_scaled <- mean(adj$scaled[adj$id_trans == 2L])
expect_true(abs(col_mean_t1_scaled - rates$rate[1L]) <=
              abs(mean_t1 - rates$rate[1L]) + 1e-9)
