library(tinytest)

# float subtype
pred_data_t_float <- as_pred_data_t(
  data.table::data.table(
    id_pred = 1:2,
    id_coord = 3:50,
    id_period = 1L,
    value = 3.14
  ),
  type = "float"
)
expect_inherits(
  pred_data_t_float,
  c("pred_data_t_float", "pred_data_t")
))
expect_equal(nrow(pred_data_t_float), 48L)

# Test creation and validation for int subtype
pred_data_t_int <- as_pred_data_t(
  data.table::data.table(
    id_pred = 1L,
    id_coord = 1L,
    id_period = 1L,
    value = 42L
  ),
  type = "int"
)

# Test creation and validation for bool subtype
expect_silent(
  pred_data_t_bool <-
    as_pred_data_t(
      data.table::data.table(
        id_pred = 1L,
        id_coord = 1L,
        id_period = 1L,
        value = TRUE
      ),
      type = "bool"
    )
)

expect_stdout(print(pred_data_t_bool), r"{Predictor Data Table \(bool\)}")
expect_stdout(print(pred_data_t_int), r"{Predictor Data Table \(int\)}")
expect_stdout(print(pred_data_t_float), r"{Predictor Data Table \(float\)}")
