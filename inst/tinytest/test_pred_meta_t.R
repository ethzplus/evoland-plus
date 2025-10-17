library(tinytest)

config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
db$config <- read_evoland_config(config_path)

# Test creation and validation
pred_meta_t <- create_pred_meta_t(db$config)
expect_true(nrow(pred_meta_t) == 2L)
expect_silent(db$pred_meta_t <- pred_meta_t)
expect_stdout(print(db$pred_meta_t), "Number of predictors")
expect_equal(db$pred_meta_t, pred_meta_t)
