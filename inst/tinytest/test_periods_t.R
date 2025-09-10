library(tinytest)

config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
db$config <- read_evoland_config(config_path)

# expecting the validator to complain that fields are all-na
expect_silent(periods_t <- create_periods_t(db$config))
expect_stdout(print(db$periods_t), "(0 rows and 4 cols)")
expect_silent(db$periods_t <- periods_t)
expect_equal(periods_t, db$periods_t)
expect_equal(db$row_count("periods_t"), 15L)
