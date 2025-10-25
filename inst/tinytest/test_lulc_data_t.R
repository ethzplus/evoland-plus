library(tinytest)

expect_silent(
  lulc_data_t <- as_lulc_data_t(data.frame(
    id_coord = integer(),
    id_lulc = integer(),
    id_period = integer(),
    date = as.Date(numeric())
  ))
)
expect_stdout(print(lulc_data_t), "LULC Data Table \\(empty\\)")
expect_equal(nrow(lulc_data_t), 0L)

synthetic_lulc_data_t <- data.table::data.table(
  id_coord = c(1L, 2L, 1L),
  id_lulc = c(1L, 2L, 3L),
  id_period = c(1L, 1L, 2L),
  date = as.Date(c("2000-01-01", "2000-01-01", "2010-01-01"))
)

# Test successful conversion
expect_silent(
  converted_lulc_data_t <- as_lulc_data_t(synthetic_lulc_data_t)
)
expect_equal(nrow(converted_lulc_data_t), 3L)
expect_true(inherits(converted_lulc_data_t, "lulc_data_t"))
