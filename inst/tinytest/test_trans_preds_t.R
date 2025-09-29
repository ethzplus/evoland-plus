library(tinytest)

config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
db$config <- read_evoland_config(config_path)

# Test creation and validation
trans_preds_t <- create_trans_preds_t(db)
expect_silent(print(trans_preds_t))
expect_true(nrow(trans_preds_t) >= 0L)
expect_silent(db$trans_preds_t <- trans_preds_t)
expect_equal(db$row_count("trans_preds_t"), nrow(trans_preds_t))
