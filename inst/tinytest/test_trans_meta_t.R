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

synthetic_lulc_data_t <- data.table::data.table(
  id_coord = c(1L, 2L, 1L),
  id_lulc = c(1L, 2L, 3L),
  id_period = c(1L, 1L, 2L)
)

create_trans_meta_t(synthetic_lulc_data_t)
