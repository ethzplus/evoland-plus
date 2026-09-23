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

# Potentials are keyed per run: a child run's write must not replace the parent's rows
db <- evoland_db$new(path = tempfile("trans_pot_t_runs"))
db$runs_t <- as_runs_t(data.frame(
  id_run = 0:1,
  parent_id_run = c(NA, 0L),
  description = c("Base", "Child")
))
pots <- data.frame(id_trans = 1L, id_period_post = 2L, id_coord = 1:3)

db$id_run <- 0L
db$trans_pot_t <- as_trans_pot_t(cbind(id_run = 0L, pots, value = c(0.1, 0.2, 0.3)))
db$id_run <- 1L
db$trans_pot_t <- as_trans_pot_t(cbind(id_run = 1L, pots, value = 0))

expect_equal(db$trans_pot_t[["value"]], c(0, 0, 0))
db$id_run <- 0L
expect_equal(db$trans_pot_t[["value"]], c(0.1, 0.2, 0.3))
