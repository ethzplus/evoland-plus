library(tinytest)

db <- evoland_db$new(":memory:")
db$coords_t <- create_coords_t_square(
  epsg = 2056,
  extent = terra::ext(c(
    xmin = 2697000,
    xmax = 2698000,
    ymin = 1252000,
    ymax = 1253000
  )),
  resolution = 100
)

db$periods_t <- create_periods_t(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2020-01-01",
  end_extrapolated = "2060-01-01"
)

# lulc meta and data
db$lulc_meta_t <- create_lulc_meta_t(list(
  closed_forest = list(
    pretty_name = "Dense Forest",
    description = "Normal forest; Forest strips; Afforestations",
    src_classes = c(50:53, 57L)
  ),
  arable = list(pretty_name = "Arable Land", src_classes = 41L),
  urban = list(
    pretty_name = "Urban areas",
    description = "Industrial and camping areas; Garden allotments; Cemeteries",
    src_classes = c(1:14, 19L, 29:36)
  ),
  static = list(
    pretty_name = "Static / immutable classes",
    description = "Airports; Airfields; Dumps; Quarries, mines, et cetera",
    src_classes = c(15:18, 20:28, 61:63, 66:71)
  )
))


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
expect_stdout(print(db$trans_models_t), "With full models")
