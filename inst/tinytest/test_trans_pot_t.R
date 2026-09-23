library(tinytest)

# --------------------------------------------------------------------------
# Unit tests for trans_pot_t schema
# --------------------------------------------------------------------------

# as_trans_pot_t: basic construction
tp <- as_trans_pot_t(data.frame(
  id_run = 0L,
  id_trans = 1L,
  id_period_post = 2L,
  id_coord = 1L,
  value = 0.3
))
expect_inherits(tp, "trans_pot_t")
expect_equal(nrow(tp), 1L)
expect_true(all(c("id_trans", "id_period_post", "id_coord", "value") %in% names(tp)))

# Values must remain in [0, 1]
expect_error(
  as_trans_pot_t(data.frame(
    id_run = 0L,
    id_trans = c(1L, 2L, 1L, 2L),
    id_period_post = 2L,
    id_coord = c(1L, 1L, 2L, 2L),
    value = c(1.4, 0.4, 0.6, 0.3)
  )),
  'all(x[["value"]] <= 1) is not TRUE',
  fixed = TRUE
)
