# Test that a new database can be set up using evoland_db$new()
test_db_path <- tempfile(fileext = ".duckdb")
db <- evoland_db$new(test_db_path)
tinytest::expect_true(inherits(db, "evoland_db"))
tinytest::expect_true(file.exists(test_db_path))
tinytest::expect_identical(
  db$list_tables(),
  c(
    "alloc_params_t",
    "conf_t",
    "coords_t",
    "intrv_data_t",
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
)

# Clean up
file.remove(test_db_path)
