library(tinytest)

if (!at_home()) {
  # cannot easily mock the dinamica call; skip for now
  exit_file("Integration tests skipped (not at_home)")
}

source(file.path(system.file("tinytest", package = "evoland"), "helper_testdb.R"))
db <- make_test_db()
db$trans_rates_t <- db$get_obs_trans_rates()
db$trans_rates_t <- extrapolate_trans_rates(
  db$trans_rates_t,
  db$periods_t,
  coord_count = nrow(db$coords_t)
)
# test the package's standard glm quasibinomial fit and append to disk
expect_message(
  db$trans_models_t <- db$fit_partial_models(
    fit_fun = fit_glm,
    gof_fun = gof_glm,
    seed = 1244244,
  ),
  "Fitting partial models for 2 transitions..."
)
expect_message(
  db$trans_models_t <- db$fit_full_models(
    gof_criterion = "auc",
    gof_maximize = TRUE
  ),
  "Fitting full models for"
)

# no data for period 4 yet
expect_equal(nrow(db$fetch("lulc_data_t", where = "id_period = 4")), 0L)

# Test alloc_dinamica with a simple two-period simulation
if (Sys.which("DinamicaConsole") == "") {
  expect_warning(
    db$alloc_dinamica(
      id_periods = 4,
      gof_criterion = "auc",
      gof_maximize = TRUE,
      work_dir = file.path(db$path, "dinamica_test"),
      keep_intermediate = FALSE
    ),
    "Copying anterior.tif to posterior.tif as fallback so we can test."
  )
} else {
  expect_stdout(
    db$alloc_dinamica(
      id_periods = 4,
      work_dir = file.path(db$path, "dinamica_test"),
      keep_intermediate = FALSE
    ),
    "Starting to run model with Dinamica EGO"
  )
}

# 900 coords in period 4 (same as period 3)
expect_equal(nrow(db$fetch("lulc_data_t", cols = "id_coord", where = "id_period = 4")), 900L)

# Test eval_alloc_params_t
expect_message(
  db$alloc_params_t <-
    evaluated_params <-
      db$eval_alloc_params_t(
        gof_criterion = "auc",
        gof_maximize = TRUE,
        work_dir = file.path(db$path, "dinamica_eval"),
        keep_intermediate = FALSE
      ),
  "Evaluation Complete"
)

# Check evaluation results
expect_inherits(evaluated_params, "alloc_params_t")
expect_true(all(c("similarity", "frac_patcher") %in% names(evaluated_params)))

# Check that accuracy metrics are reasonable (between 0 and 1)
expect_true(all(
  evaluated_params$similarity <= 1 &
    evaluated_params$similarity >= 0,
  na.rm = TRUE
))

# Test error handling - invalid id_periods
expect_error(
  db$alloc_dinamica(
    id_periods = c(1L, 3L) # Non-contiguous
  ),
  "id_periods must be contiguous"
)
