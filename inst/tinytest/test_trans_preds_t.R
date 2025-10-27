library(tinytest)


# Test creation and validation
trans_preds_t <- create_trans_preds_t()
expect_silent(print(trans_preds_t))
expect_true(nrow(trans_preds_t) >= 0L)
