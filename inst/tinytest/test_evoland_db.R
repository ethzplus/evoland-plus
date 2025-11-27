# Test evoland_db domain-specific functionality
# Generic parquet_duckdb tests are in test_parquet_duckdb.R
library(tinytest)

# Create temporary directory for testing
test_dir <- tempfile("evoland_test_")
on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

# Test 1: evoland_db initialization with reporting
expect_silent(
  db <- evoland_db$new(
    path = test_dir,
    report_name = "tinytest",
    report_username = "testuser"
  )
)
expect_true(inherits(db, "evoland_db"))
expect_true(inherits(db, "parquet_duckdb")) # Should inherit from parent

# Test 2: Reporting table is created and populated
expect_identical(db$list_tables(), "reporting_t")
reporting1 <- db$fetch("reporting_t")
expect_true("report_name" %in% reporting1$key)
expect_true("tinytest" %in% reporting1$value)

# Test 3: Persistence of reporting data
rm(db)
gc()
db <- evoland_db$new(
  path = test_dir,
  report_name = "tinytest",
  report_username = "testuser"
)
reporting2 <- db$fetch("reporting_t")
expect_equal(
  # upsert reorders rows
  sort(reporting1$key),
  sort(reporting2$key)
)

# Test 4: Domain-specific empty table structures via active bindings
empty_tables <- c(
  "alloc_params_t",
  "coords_t",
  "intrv_masks_t",
  "intrv_meta_t",
  "lulc_data_t",
  "lulc_meta_t",
  "periods_t",
  "pred_data_t_bool",
  "pred_data_t_float",
  "pred_data_t_int",
  "pred_meta_t",
  "trans_meta_t",
  "trans_models_t",
  "trans_preds_t"
)
for (table in empty_tables) {
  expect_equal(nrow(db[[table]]), 0L)
}

# Test 5: Create synthetic evoland-specific test data
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
pred_spec <- list(
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
    orig_format = "vector",
    description = "Derived from swissTLM3D",
    sources = list(list(
      url = "https://example.com/swisstlm3d_2025-03_2056_5728.gpkg.zip",
      md5sum = "ecb3bcfbf6316c6e7542e20de24f61b7"
    ))
  )
)
pred_meta_t <- create_pred_meta_t(pred_spec)

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

# Test 6: Active bindings - coords_t
expect_silent(db$coords_t <- coords_t)
expect_silent(db$coords_t <- coords_t)
expect_identical(db$coords_t, coords_t)
expect_identical(
  db$coords_minimal,
  data.table::as.data.table(coords_t[, 1:3])
)

# Test 7: Active bindings - lulc_meta_t
expect_silent(db$lulc_meta_t <- lulc_meta_t)
expect_identical(db$lulc_meta_t, lulc_meta_t)

# Test 8: Active bindings - periods_t
expect_silent(db$periods_t <- periods_t)
expect_identical(db$periods_t, periods_t)

# Test 9: Active bindings - pred_meta_t with auto-increment
expect_silent(db$pred_meta_t <- pred_meta_t[1, ])
expect_silent(db$pred_meta_t <- pred_meta_t)
# After committing, id_pred should be assigned (1:2 in this case)
retrieved_pred_meta <- db$pred_meta_t
expect_equal(retrieved_pred_meta[["id_pred"]], 1:2)
expect_equal(retrieved_pred_meta[["name"]], c("noise", "distance_to_lake"))
db$delete_from("pred_meta_t") # clear up, we want to ensure this works with add_predictor

# Test 10: Active bindings - intrv_meta_t (with MAP columns)
expect_silent(db$intrv_meta_t <- intrv_meta_t)
expect_equal(db$intrv_meta_t, intrv_meta_t)

# Test 11: Active bindings - trans_meta_t
expect_silent(db$trans_meta_t <- trans_meta_t)
expect_equal(db$trans_meta_t[, c(-1)], trans_meta_t)

# Test 12: Active bindings - trans_models_t (with MAP and BLOB columns)
expect_silent(db$trans_models_t <- trans_models_t)
expect_equal(db$trans_models_t, trans_models_t)

# Test 13: Active bindings - alloc_params_t (with MAP columns)
expect_silent(db$alloc_params_t <- alloc_params_t)
expect_equal(db$alloc_params_t, alloc_params_t)

# Test 14: add_predictor method - first predictor
# Test 15: add_predictor method - second predictor
expect_silent(
  db$add_predictor(
    pred_spec = pred_spec["noise"],
    pred_data = pred_data_t[id_pred == 1],
    pred_type = "float"
  )
)
expect_equal(db$row_count("pred_data_t_float"), 24L)

expect_silent(
  db$add_predictor(
    pred_spec = pred_spec["distance_to_lake"],
    pred_data = pred_data_t[id_pred == 2],
    pred_type = "float"
  )
)
# Test 16: add_predictor correctly updates metadata
retrieved_pred_meta <- db$pred_meta_t
expect_equal(retrieved_pred_meta[["id_pred"]], 1:2)
expect_equal(retrieved_pred_meta[["name"]], c("noise", "distance_to_lake"))

# Test 17: add_predictor correctly updates data
expect_equal(db$row_count("pred_data_t_float"), 48L)
# Test 18: Active bindings - pred_data_t_float upsert is idempotent
expect_silent(db$pred_data_t_float <- pred_data_t)
expect_equal(db$row_count("pred_data_t_float"), 48L)
expect_silent(db$pred_data_t_float <- pred_data_t)
expect_equal(db$row_count("pred_data_t_float"), 48L)
# Test 19: Active bindings enforce type validation
expect_error(
  # should only be able to insert the correct class
  db$pred_data_t_float <- data.table::data.table(
    id_pred = 100:102,
    id_coord = 3:50,
    id_period = 1L,
    value = 3.14
  ),
  r"(^inherits.* is not TRUE$)"
)

# Test 20: lulc_data_t requires proper type
expect_error(
  db$lulc_data_t <- lulc_data_dt <- data.table::data.table(
    id_coord = c(1L, 2L, 1L),
    id_lulc = c(1L, 2L, 3L),
    id_period = c(1L, 1L, 2L)
  ),
  r"(^inherits.* is not TRUE$)"
)
expect_silent(
  db$lulc_data_t <- as_lulc_data_t(lulc_data_dt)
)

# Test 21: Domain-specific view - coords_minimal
coords_minimal <- db$coords_minimal
expect_true(inherits(coords_minimal, "data.table"))
expect_equal(ncol(coords_minimal), 3L)
expect_true(all(c("id_coord", "lon", "lat") %in% names(coords_minimal)))
expect_equal(nrow(coords_minimal), nrow(coords_t))

# Test 22: Domain-specific view - extent
# Set up coords first
db$coords_t <- coords_t
extent <- db$extent
expect_true(inherits(extent, "SpatExtent"))

# Test 23: Domain-specific view - lulc_meta_long_v
db$lulc_meta_t <- lulc_meta_t
lulc_long <- db$lulc_meta_long_v
expect_true(inherits(lulc_long, "data.table"))
expect_true("src_class" %in% names(lulc_long))
# Should have one row per src_class
expect_true(nrow(lulc_long) > nrow(lulc_meta_t))

# Test 24: Domain-specific view - pred_sources_v
db$pred_meta_t <- pred_meta_t
sources <- db$pred_sources_v
expect_true(inherits(sources, "data.table"))
expect_true(all(c("url", "md5sum") %in% names(sources)))
expect_true(nrow(sources) > 0L)

# Test 25: set_coords method
test_dir_coords <- tempfile("evoland_coords_")
on.exit(unlink(test_dir_coords, recursive = TRUE), add = TRUE)
db_coords <- evoland_db$new(test_dir_coords)

expect_silent(
  db_coords$set_coords(
    type = "square",
    epsg = 2056,
    extent = terra::ext(c(xmin = 2697000, xmax = 2698000, ymin = 1252000, ymax = 1253000)),
    resolution = 100
  )
)
expect_true(db_coords$row_count("coords_t") > 0L)

# Should refuse to overwrite
expect_warning(
  db_coords$set_coords(
    type = "square",
    epsg = 2056,
    extent = terra::ext(c(xmin = 2697000, xmax = 2698000, ymin = 1252000, ymax = 1253000)),
    resolution = 100
  ),
  "not empty"
)

# Test 26: set_periods method
test_dir_periods <- tempfile("evoland_periods_")
on.exit(unlink(test_dir_periods, recursive = TRUE), add = TRUE)
db_periods <- evoland_db$new(test_dir_periods)

expect_silent(
  db_periods$set_periods(
    period_length_str = "P10Y",
    start_observed = "1985-01-01",
    end_observed = "2020-01-01",
    end_extrapolated = "2060-01-01"
  )
)
expect_true(db_periods$row_count("periods_t") > 0L)

# Should refuse to overwrite
expect_warning(
  db_periods$set_periods(),
  "not empty"
)
