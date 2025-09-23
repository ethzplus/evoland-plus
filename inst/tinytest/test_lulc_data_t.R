library(tinytest)

config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
db$config <- read_evoland_config(config_path)

expect_silent(lulc_data_t <- create_lulc_data_t())
expect_equal(nrow(lulc_data_t), 0L)

# First populate the database with reference tables following the pattern from other tests
expect_silent(coords_t <- create_coords_t(db$config))
expect_silent(db$coords_t <- coords_t)

expect_silent(periods_t <- create_periods_t(db$config))
expect_silent(db$periods_t <- periods_t)

expect_silent(lulc_meta_t <- create_lulc_meta_t(db$config))
expect_silent(db$lulc_meta_t <- lulc_meta_t)

expect_silent(db$lulc_data_t <- lulc_data_t)
expect_equal(db$row_count("lulc_data_t"), 0L)

synthetic_lulc_data_t <- data.table::data.table(
  id_coord = c(1L, 2L, 1L),
  id_lulc = c(1L, 2L, 3L),
  id_period = c(1L, 1L, 2L),
  date = as.Date(c("2000-01-01", "2000-01-01", "2010-01-01"))
)

expect_silent(db$lulc_data_t <- synthetic_lulc_data_t)
expect_equal(db$row_count("lulc_data_t"), 3L)

invalid_lulc_data_t <- data.table::data.table(
  id_coord = c(1L, 2L, 1L, 1L),
  id_lulc = c(1L, 2L, 3L, 20L),
  id_period = c(1L, 1L, 2L, 2L),
  date = as.Date(c("2000-01-01", "2000-01-01", "2010-01-01", "2010-01-01"))
)

expect_error(
  db$lulc_data_t <- invalid_lulc_data_t,
  '"id_lulc: 20" does not exist in the referenced table'
)
