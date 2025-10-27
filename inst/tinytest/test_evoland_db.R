# Test that a new database can be set up using evoland_db$new()
library(tinytest)

expect_silent(db <- evoland_db$new(":memory:"))
expect_true(inherits(db, "evoland_db"))

# TODO this should be a harness iterating over different valid and some invalid examples
expected_tables <- c(
  "alloc_params_t",
  "coords_t",
  "intrv_masks_t",
  "intrv_meta_t",
  "lulc_data_t",
  "lulc_meta_long_v",
  "lulc_meta_t",
  "periods_t",
  "pred_data_t_bool",
  "pred_data_t_float",
  "pred_data_t_int",
  "pred_meta_t",
  "pred_sources_v",
  "reporting_t",
  "trans_meta_t",
  "trans_models_t",
  "trans_preds_t"
)
expect_identical(db$list_tables(), expected_tables)

# check active bindings on empty tables
for (table in expected_tables) {
  # exception for tables that are already populated at DB inception
  if (table %in% c("reporting_t")) {
    next
  }
  expect_equal(nrow(db[[table]]), 0L)
}

# Create synthetic, minimal test data
coords_t <- create_coords_t_square(
  epsg = 2056,
  extent = terra::ext(c(
    xmin = 2697000,
    xmax = 2698000,
    ymin = 1252000,
    ymax = 1253000
  )),
  resolution = 100
)

periods_t <- create_periods_t(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2020-01-01",
  end_extrapolated = "2060-01-01"
)

lulc_meta_t <- create_lulc_meta_t(list(
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

# Predictor metadata
pred_meta_t <- create_pred_meta_t(list(
  noise = list(
    unit = "dBa",
    pretty_name = "Maximum noise exposure",
    orig_format = "10m*10m raster",
    description = "max daytime & nighttime road & rail noise exposure",
    sources = list(
      list(
        url = "https://example.com/laerm-strassenlaerm_tag_2056.tif",
        md5sum = "a4b9f1c04ee63824f18852bfd1eecbdd"
      ),
      list(
        url = "https://example.com/laerm-bahnlaerm_nacht_2056.tif",
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
      url = "https://example.com/swisstlm3d_2025-03_2056_5728.gpkg.zip",
      md5sum = "ecb3bcfbf6316c6e7542e20de24f61b7"
    ))
  )
))

pred_data_t <- as_pred_data_t(
  data.table::data.table(
    id_pred = 1:2,
    id_coord = 3:50,
    id_period = 1L,
    value = 3.14
  ),
  type = "float"
)

intrv_meta_t <- create_intrv_meta_t(list(
  protected_areas = list(
    pre_allocation = TRUE,
    pretty_name = "Nature protection areas",
    description = "This intervention introduces additional protected areas",
    periods = c(7, 8),
    transitions = c(1, 2),
    sources = list(
      list(
        url = "file:///somedir/protected_areas.gpkg",
        md5sum = "something"
      )
    )
  ),
  glaciers = list(
    pre_allocation = FALSE,
    pretty_name = "Deglaciation",
    description = "Deterministic transitions based on GloGEM",
    sources = list(
      list(
        url = "file:///somedir/glacier_extent_2030.tif",
        md5sum = "something"
      ),
      list(
        url = "file:///somedir/glacier_extent_2040.tif",
        md5sum = "something"
      )
    )
  ),
  hydro_predictors = list(
    pre_allocation = TRUE,
    pretty_name = "Hydrological predictor variables",
    description = "Provide refined predictor variables based on the rsplash hydrological model.",
    params = list(
      tmpdir = "/mnt/ramdisk"
    )
  )
))

alloc_params_t <- as_alloc_params_t(list(
  id_trans = 1L,
  id_period = 2L,
  alloc_params = list(
    list(
      mean_patch_size = 1.3,
      patch_size_variance = 1.4,
      patch_isometry = 0.2,
      share_expander = 0.8
    )
  ),
  goodness_of_fit = list(
    list(
      window_size = 11,
      fuzzy_similarity = 0.8
    )
  )
))

trans_meta_t <- as_trans_meta_t(
  data.table::data.table(
    id_trans = 1:3,
    id_lulc_anterior = 1:3,
    id_lulc_posterior = 2:4,
    cardinality = c(100L, 2000L, 10L),
    frequency_rel = c(0.1, 0.1, 0.15),
    frequency_abs = c(0.1, 0.1, 0.15),
    is_viable = c(TRUE, FALSE, FALSE)
  )
)

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

intrv_masks_t <- as_intrv_masks_t(
  data.table::data.table(
    id_coord = 1:99,
    id_intrv = 1:3
  )
)

# Test DB roundtrips & integrity checks
expect_silent(db$coords_t <- coords_t)
expect_identical(db$coords_t, coords_t)

expect_silent(db$lulc_meta_t <- lulc_meta_t)
expect_identical(db$lulc_meta_t, lulc_meta_t)

expect_silent(db$periods_t <- periods_t)
expect_identical(db$periods_t, periods_t)

expect_silent(db$pred_meta_t <- pred_meta_t)
expect_identical(db$pred_meta_t, pred_meta_t)

expect_silent(db$intrv_meta_t <- intrv_meta_t)
expect_equal(db$intrv_meta_t, intrv_meta_t)

# check one of the foreign key constraint
expect_error(
  db$alloc_params_t <- alloc_params_t,
  '"id_trans: 1" does not exist'
)

expect_silent(db$trans_meta_t <- trans_meta_t)
expect_equal(db$trans_meta_t, trans_meta_t)

expect_silent(db$trans_models_t <- trans_models_t)
expect_equal(db$trans_models_t, trans_models_t)

# now that we have the trans_meta we shouldn't be violating the FK anymore
expect_silent(db$alloc_params_t <- alloc_params_t)
expect_equal(db$alloc_params_t, alloc_params_t)

# repeated upsert should be idempotent
expect_silent(db$pred_data_t_float <- pred_data_t)
expect_equal(db$row_count("pred_data_t_float"), 48L)
expect_silent(db$pred_data_t_float <- pred_data_t)
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

expect_error(
  db$lulc_data_t <- lulc_data_dt <- data.table::data.table(
    id_coord = c(1L, 2L, 1L),
    id_lulc = c(1L, 2L, 3L),
    id_period = c(1L, 1L, 2L),
    date = as.Date(c("2000-01-01", "2000-01-01", "2010-01-01"))
  ),
  r"(^inherits.* is not TRUE$)"
)
expect_silent(
  db$lulc_data_t <- as_lulc_data_t(lulc_data_dt)
)

# TODO once duckdb has fixed the "FK does not exist" bug, we can enable this test
# https://github.com/duckdb/duckdb/issues/16785
# Test copy DB and clean up
# test_db_path <- tempfile(fileext = ".duckdb")
# db$copy_db(test_db_path)
# expect_true(file.exists(test_db_path))
# rm(db)
# gc() # need to call gc for finalizer
# file.remove(test_db_path)
