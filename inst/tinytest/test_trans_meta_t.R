library(tinytest)

trans_meta_t <- create_trans_meta_t()
expect_silent(trans_meta_t)
expect_equal(nrow(trans_meta_t), 0L)
expect_true(inherits(trans_meta_t, "trans_meta_t"))
expect_stdout(print(trans_meta_t), "empty")

# Test expected column structure
expected_cols <- c(
  "id_trans",
  "id_lulc_anterior",
  "id_lulc_posterior",
  "cardinality",
  "frequency_rel",
  "frequency_abs",
  "is_viable"
)
expect_true(all(expected_cols %in% names(trans_meta_t)))

set.seed(123124)
synthetic_transitions <- data.table::data.table(
  id_coord = rep(c(1L, 2L, 3L), 10),
  id_lulc_anterior = sample(1:4, 30, replace = TRUE),
  id_lulc_posterior = sample(1:4, 30, replace = TRUE),
  id_period = rep(2:4, each = )
)

expect_equal(
  create_trans_meta_t(synthetic_transitions)[3, frequency_rel],
  0.15
)
