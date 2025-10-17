library(tinytest)

config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
db$config <- read_evoland_config(config_path)

# Test creation and validation
intrv_meta_t <- create_intrv_meta_t(db$config)
expect_silent(print(intrv_meta_t))
expect_equal(nrow(intrv_meta_t), 3L)
expect_silent(db$intrv_meta_t <- intrv_meta_t)
expect_silent(db$intrv_meta_t <- intrv_meta_t)
expect_equal(db$intrv_meta_t, intrv_meta_t)
