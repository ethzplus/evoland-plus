library(tinytest)

config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
db$config <- read_evoland_config(config_path)

# expecting the validator to complain that fields are all-na
expect_silent(coords_t <- create_coords_t(db$config))
expect_stdout(print(db$coords_t), "(0 rows and 6 cols)")
expect_silent(db$coords_t <- coords_t)
expect_equal(coords_t, db$coords_t)
expect_equal(db$row_count("coords_t"), 100L)
