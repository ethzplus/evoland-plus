library(tinytest)

# Setup required data for testing
db <- evoland_db$new(":memory:")
# nolint start
pred_spec <- list(
  noise = list(
    unit = "dBa",
    pretty_name = "Maximum noise exposure",
    orig_format = "10m*10m raster",
    description = "The maximum across all sonBASE noise exposure categories, i.e. daytime & nighttime road & rail exposure",
    sources = list(
      list(
        url = "https://data.geo.admin.ch/ch.bafu.laerm-strassenlaerm_tag/laerm-strassenlaerm_tag/laerm-strassenlaerm_tag_2056.tif",
        md5sum = "a4b9f1c04ee63824f18852bfd1eecbdd"
      ),
      list(
        url = "https://data.geo.admin.ch/ch.bafu.laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht_2056.tif",
        md5sum = "4b782128495b5af8467e2259bd57def2"
      )
    )
  ),
  distance_to_lake = list(
    unit = "m",
    pretty_name = "Distance to closest lake",
    format = "vector",
    description = "Derived from swissTLM3D",
    sources = list(list(
      url = "https://data.geo.admin.ch/ch.swisstopo.swisstlm3d/swisstlm3d_2025-03/swisstlm3d_2025-03_2056_5728.gpkg.zip",
      md5sum = "ecb3bcfbf6316c6e7542e20de24f61b7"
    ))
  )
)
# nolint end

# Test creation and validation
db$pred_meta_t <- create_pred_meta_t(pred_spec)
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
