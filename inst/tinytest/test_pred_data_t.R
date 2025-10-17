library(tinytest)

# Setup required data for testing
config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
db$config <- read_evoland_config(config_path)
db$pred_meta_t <- create_pred_meta_t(db$config)
db$coords_t <- create_coords_t(db$config)
db$periods_t <- create_periods_t(db$config)

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
expect_true(inherits(
  pred_data_t_float,
  c("pred_data_t_float", "pred_data_t")
))
expect_silent(db$pred_data_t_float <- pred_data_t_float)
expect_equal(db$row_count("pred_data_t_float"), 48L)
# repeated assignment should just be an upsert
expect_silent(db$pred_data_t_float <- pred_data_t_float)
expect_equal(db$row_count("pred_data_t_float"), 48L)

expect_error(
  # should only be able to insert the correct class
  db$pred_data_t_float <- data.table::data.table(
    id_pred = 1000:1002,
    id_coord = 3:50,
    id_period = 1L,
    value = 3.14
  ),
  r"(^inherits.* is not TRUE$)"
)
expect_error(
  # should violate foreign key constraint
  db$pred_data_t_float <- as_pred_data_t(
    data.table::data.table(
      id_pred = 1000:1002,
      id_coord = 3:50,
      id_period = 1L,
      value = TRUE
    ),
    type = "float"
  ),
  'key "id_pred: 1000" does not exist'
)

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
expect_silent(db$pred_data_t_int <- pred_data_t_int)
expect_equal(db$row_count("pred_data_t_int"), 1L)

# Test creation and validation for bool subtype
expect_silent(
  db$pred_data_t_bool <-
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
expect_equal(db$row_count("pred_data_t_bool"), 1L)

expect_stdout(print(db$pred_data_t_bool), r"{Predictor Data Table \(bool\)}")
expect_stdout(print(db$pred_data_t_int), r"{Predictor Data Table \(int\)}")
expect_stdout(print(db$pred_data_t_float), r"{Predictor Data Table \(float\)}")
