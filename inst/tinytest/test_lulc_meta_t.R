library(tinytest)

config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
db$config <- read_evoland_config(config_path)

# expecting the validator to complain that fields are all-na
expect_silent(lulc_meta_t <- create_lulc_meta_t(db$config))
expect_stdout(print(db$lulc_meta_t), "(0 rows and 5 cols)")
expect_silent(db$lulc_meta_t <- lulc_meta_t)
expect_equal(lulc_meta_t, db$lulc_meta_t)
expect_equal(db$row_count("lulc_meta_t"), 4L)
