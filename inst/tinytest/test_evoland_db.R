# Test that a new database can be set up using evoland_db$new()
library(tinytest)

# Create temporary directory for testing
test_dir <- tempfile("evoland_test_")
on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

expect_silent(
  db <- evoland_db$new(
    path = test_dir,
    report_name = "tinytest",
    report_username = "testuser"
  )
)
expect_true(inherits(db, "evoland_db"))

# In folder-based storage, only reporting_t exists initially
# Other tables are created on demand
expected_tables_initial <- c("reporting_t")
expect_identical(db$list_tables(), expected_tables_initial)

db$attach_table("reporting_t")
reporting1 <- db$get_query("from reporting_t;")
rm(db)
gc()
db <- evoland_db$new(
  path = test_dir,
  report_name = "tinytest",
  report_username = "testuser"
)
db$attach_table("reporting_t")
reporting2 <- db$get_query("from reporting_t;")
expect_equal(reporting1[1:4], reporting2[1:4])


# Check that accessing non-existent tables returns empty data.tables
# (these tables don't appear in list_tables() until they have data)
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

# Test DB roundtrips & integrity checks, repeated assignments (~upserts)
# TODO make this into a testing harness that iterates over a list of sample data
expect_silent(db$coords_t <- coords_t)
expect_silent(db$coords_t <- coords_t)
expect_identical(db$coords_t, coords_t)

expect_silent(db$lulc_meta_t <- lulc_meta_t)
expect_identical(db$lulc_meta_t, lulc_meta_t)

expect_silent(db$periods_t <- periods_t)
expect_identical(db$periods_t, periods_t)

expect_silent(db$pred_meta_t <- pred_meta_t[1, ])
expect_silent(db$pred_meta_t <- pred_meta_t)
# After committing, id_pred should be assigned (1:2 in this case)
retrieved_pred_meta <- db$pred_meta_t
expect_equal(retrieved_pred_meta[["id_pred"]], 1:2)
expect_equal(retrieved_pred_meta[["name"]], c("noise", "distance_to_lake"))
db$delete_from("pred_meta_t") # clear up, we want to ensure this works with add_predictor

expect_silent(db$intrv_meta_t <- intrv_meta_t)
expect_equal(db$intrv_meta_t, intrv_meta_t)

expect_silent(db$trans_meta_t <- trans_meta_t)
expect_equal(db$trans_meta_t, trans_meta_t)

expect_silent(db$trans_models_t <- trans_models_t)
expect_equal(db$trans_models_t, trans_models_t)

# now that we have the trans_meta we shouldn't be violating the FK anymore
expect_silent(db$alloc_params_t <- alloc_params_t)
expect_equal(db$alloc_params_t, alloc_params_t)

# repeated upsert should be idempotent
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
# check that add_predictor works for metadata
retrieved_pred_meta <- db$pred_meta_t
expect_equal(retrieved_pred_meta[["id_pred"]], 1:2)
expect_equal(retrieved_pred_meta[["name"]], c("noise", "distance_to_lake"))

# check that add_predictor works for data
expect_equal(db$row_count("pred_data_t_float"), 48L)
expect_silent(db$pred_data_t_float <- pred_data_t)
expect_equal(db$row_count("pred_data_t_float"), 48L)
expect_silent(db$pred_data_t_float <- pred_data_t)
expect_equal(db$row_count("pred_data_t_float"), 48L)
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

# Test delete_from functionality
# Setup: we already have pred_data_t_float with 48 rows (id_pred 1:2, id_coord 3:50, id_period 1)
expect_equal(db$row_count("pred_data_t_float"), 48L)

# Test 1: Delete with WHERE clause - delete specific predictor
deleted_count <- db$delete_from("pred_data_t_float", where = "id_pred = 1")
expect_equal(deleted_count, 24L)
expect_equal(db$row_count("pred_data_t_float"), 24L)

# Verify only id_pred = 2 remains
remaining <- db$pred_data_t_float
expect_equal(unique(remaining$id_pred), 2L)
expect_equal(nrow(remaining), 24L)

# Test 2: Delete with complex WHERE clause
# Add back some data first
db$pred_data_t_float <- pred_data_t
expect_equal(db$row_count("pred_data_t_float"), 48L)

# Delete only specific coordinates
deleted_count <- db$delete_from("pred_data_t_float", where = "id_coord < 10")
expect_true(deleted_count > 0L)
remaining <- db$pred_data_t_float
expect_true(all(remaining$id_coord >= 10))

# Test 3: Delete all rows (NULL where clause)
count_before_delete <- db$row_count("pred_data_t_float")
deleted_count <- db$delete_from("pred_data_t_float", where = NULL)
expect_equal(deleted_count, count_before_delete)
expect_equal(db$row_count("pred_data_t_float"), 0L)

# Test 4: Delete from non-existent table returns 0
deleted_count <- db$delete_from("nonexistent_table", where = "id = 1")
expect_equal(deleted_count, 0L)

# Test 5: Delete with WHERE that matches nothing
db$pred_data_t_float <- pred_data_t
initial_count <- db$row_count("pred_data_t_float")
deleted_count <- db$delete_from("pred_data_t_float", where = "id_pred = 999")
expect_equal(deleted_count, 0L)
expect_equal(db$row_count("pred_data_t_float"), initial_count)

# Test auto-increment functionality
# Create a new test database for auto-increment tests
test_dir_autoinc <- tempfile("evoland_autoinc_")
on.exit(unlink(test_dir_autoinc, recursive = TRUE), add = TRUE)
db_autoinc <- evoland_db$new(test_dir_autoinc)

# Test 1: Auto-increment on overwrite mode (new table)
test_data_1 <- data.table::data.table(
  name = c("predictor_a", "predictor_b", "predictor_c"),
  unit = c("m", "kg", "s")
)
db_autoinc$commit(
  test_data_1,
  "test_autoinc_t",
  mode = "overwrite",
  autoincrement_cols = "id_test"
)
result_1 <- db_autoinc$fetch("test_autoinc_t")
expect_equal(result_1$id_test, 1:3)
expect_equal(result_1$name, c("predictor_a", "predictor_b", "predictor_c"))

# Test 2: Auto-increment on append mode
test_data_2 <- data.table::data.table(
  name = c("predictor_d", "predictor_e"),
  unit = c("A", "V")
)
db_autoinc$commit(
  test_data_2,
  "test_autoinc_t",
  mode = "append",
  autoincrement_cols = "id_test"
)
result_2 <- db_autoinc$fetch("test_autoinc_t")
expect_equal(nrow(result_2), 5L)
expect_equal(result_2$id_test, 1:5)
expect_equal(result_2$name[4:5], c("predictor_d", "predictor_e"))

# Test 3: Auto-increment on upsert mode with new rows
test_data_3 <- data.table::data.table(
  name = c("predictor_f", "predictor_g"),
  unit = c("W", "J")
)
db_autoinc$commit(
  test_data_3,
  "test_autoinc_t",
  mode = "upsert",
  autoincrement_cols = "id_test"
)
result_3 <- db_autoinc$fetch("test_autoinc_t")
expect_equal(nrow(result_3), 7L)
expect_equal(result_3$id_test, 1:7)

# Test 4: Auto-increment preserves existing IDs in data
test_data_4 <- data.table::data.table(
  id_test = c(NA, 100L, NA),
  name = c("new_a", "existing", "new_b"),
  unit = c("x", "y", "z")
)
db_autoinc$commit(
  test_data_4,
  "test_autoinc2_t",
  mode = "overwrite",
  autoincrement_cols = "id_test"
)
result_4 <- db_autoinc$fetch("test_autoinc2_t")
expect_equal(result_4$id_test[2], 100L)
expect_equal(result_4$id_test[1], 101L)
expect_equal(result_4$id_test[3], 102L)

# Test 5: Multiple auto-increment columns
test_data_5 <- data.table::data.table(
  name = c("item1", "item2"),
  value = c(10, 20)
)
db_autoinc$commit(
  test_data_5,
  "test_multi_autoinc_t",
  mode = "overwrite",
  autoincrement_cols = c("id_a", "id_b")
)
result_5 <- db_autoinc$fetch("test_multi_autoinc_t")
expect_equal(result_5$id_a, 1:2)
expect_equal(result_5$id_b, 1:2)

# Test 6: Auto-increment continues from max in append
test_data_6a <- data.table::data.table(
  id_seq = c(5L, 10L, 15L),
  value = c(100, 200, 300)
)
db_autoinc$commit(
  test_data_6a,
  "test_continue_t",
  mode = "overwrite"
)
test_data_6b <- data.table::data.table(
  value = c(400, 500)
)
db_autoinc$commit(
  test_data_6b,
  "test_continue_t",
  mode = "append",
  autoincrement_cols = "id_seq"
)
result_6 <- db_autoinc$fetch("test_continue_t")
expect_equal(nrow(result_6), 5L)
expect_equal(result_6$id_seq[4:5], c(16L, 17L))
expect_equal(result_6$value[4:5], c(400, 500))
