library(tinytest)

# Test creation and validation
trans_models_t <- as_trans_models_t(list(
  id_trans = 1L,
  id_period = 2L,
  model_family = "rf",
  model_params = list(
    list(depth = 100)
  ),
  goodness_of_fit = list(
    list(auc = 0.8)
  ),
  model_obj_part = list(
    charToRaw("some data")
  ),
  model_obj_full = list(
    charToRaw("some data")
  )
))
expect_silent(trans_models_t)
expect_equal(nrow(trans_models_t), 1L)
expect_stdout(print(trans_models_t), "With full models")
