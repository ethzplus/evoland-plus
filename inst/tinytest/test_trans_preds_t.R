library(tinytest)


# Test creation and validation
trans_preds_t <- create_trans_preds_t()
expect_silent(print(trans_preds_t))
expect_true(nrow(trans_preds_t) >= 0L)

db <- evoland_db$new(":memory:")
expect_silent(db$trans_preds_t <- trans_preds_t)
expect_equal(db$trans_preds_t, trans_preds_t)
