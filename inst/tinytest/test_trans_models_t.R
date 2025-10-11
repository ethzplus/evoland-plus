library(tinytest)

config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
db$config <- read_evoland_config(config_path)
db$coords_t <- create_coords_t(db$config)
db$periods_t <- create_periods_t(db$config)

# lulc meta and data
db$lulc_meta_t <- create_lulc_meta_t(db$config)
db$lulc_data_t <-
  data.table::data.table(
    id_coord = c(1L, 2L, 1L),
    id_lulc = c(1L, 2L, 3L),
    id_period = c(1L, 1L, 2L),
    date = as.Date(c("2000-01-01", "2000-01-01", "2010-01-01"))
  ) |>
  as_lulc_data_t()

db$trans_meta_t <-
  data.table::data.table(
    id_trans = 1:3,
    id_lulc_anterior = 1:3,
    id_lulc_posterior = 2:4,
    cardinality = c(100L, 2000L, 10L),
    frequency_rel = c(0.1, 0.1, 0.15),
    frequency_abs = c(0.1, 0.1, 0.15),
    is_viable = c(TRUE, FALSE, FALSE)
  ) |>
  as_trans_meta_t()

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
expect_silent(db$trans_models_t <- trans_models_t)
expect_equal(db$trans_models_t, trans_models_t)
