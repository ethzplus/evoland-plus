library(tinytest)

# --------------------------------------------------------------------------
# Unit tests for trans_pot_t schema and adjusted_trans_pot_v logic
# --------------------------------------------------------------------------

# as_trans_pot_t: basic construction
tp <- as_trans_pot_t(data.frame(
  id_trans = 1L,
  id_period_post = 2L,
  id_coord = 1L,
  value = 0.3
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
