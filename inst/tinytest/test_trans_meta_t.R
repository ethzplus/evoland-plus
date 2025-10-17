library(tinytest)

config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
db$config <- read_evoland_config(config_path)

# Test creation and validation
trans_meta_t <- create_trans_meta_t(db)
expect_silent(trans_meta_t)
expect_equal(nrow(trans_meta_t), 0L)
expect_silent(db$trans_meta_t <- trans_meta_t)
expect_equal(db$row_count("trans_meta_t"), nrow(trans_meta_t))
expect_stdout(print(db$trans_meta_t), "empty")
