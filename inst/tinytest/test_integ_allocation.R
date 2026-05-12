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

test_learner <- mlr3::lrn("classif.featureless", predict_type = "prob")
test_measures <- c("classif.auc")

# test the package's featureless learner fit and append to disk
expect_message(
  db$trans_models_t <- db$fit_partial_models(
    learner = test_learner,
    measures = test_measures,
    seed = 1244244,
  ),
  "Fitting partial models for 2 transitions..."
)

# Score-select mode: pick best partial model by classif.auc and retrain on full data
expect_message(
  db$trans_models_t <- db$fit_full_models(
    select_score = "classif.auc",
    select_maximize = TRUE
  ),
  "Fitting full models for"
)

# switching run to no 1, which is the base estimate for allocation parameters
db$id_run <- 1L
# no data for period 4 yet
expect_equal(nrow(db$fetch("lulc_data_t", where = "id_period = 4")), 0L)

# --------------------------------------------------------------------------
# Test Dinamica backend via generic alloc() entry point
# --------------------------------------------------------------------------
if (Sys.which("DinamicaConsole") == "") {
  expect_warning(
    db$alloc(
      method = "dinamica",
      id_periods = 4,
      select_score = "classif.auc",
      select_maximize = TRUE,
      work_dir = file.path(db$path, "dinamica_test"),
      keep_intermediate = FALSE
    ),
    "Copying anterior.tif to posterior.tif as fallback so we can test."
  )
} else {
  expect_message(
    db$alloc(
      method = "dinamica",
      id_periods = 4,
      select_score = "classif.auc",
      select_maximize = TRUE,
      work_dir = file.path(db$path, "dinamica_test"),
      keep_intermediate = FALSE
    ),
    "Starting to run model with Dinamica EGO"
  )
}

# 900 coords in period 4 (same as period 3)
expect_equal(nrow(db$fetch("lulc_data_t", cols = "id_coord", where = "id_period = 4")), 900L)

# trans_pot_t should now be populated
expect_true(nrow(db$fetch("trans_pot_t")) > 0L)

# adjusted_trans_pot_v should return values for period 4
adj_pots <- db$adjusted_trans_pot_v(4L)
expect_true(nrow(adj_pots) > 0L)
expect_true(all(adj_pots$value >= 0 & adj_pots$value <= 1))

# alloc_params_clumpy_v should return CLUMPY-format params
clumpy_params <- db$alloc_params_clumpy_v()
expect_true(nrow(clumpy_params) > 0L)
expect_true(all(c("area_mean", "area_var", "eccentricity") %in% names(clumpy_params)))

# --------------------------------------------------------------------------
# Test CLUMPY backend via generic alloc() entry point
# --------------------------------------------------------------------------
# Reset lulc_data_t for period 5 (not yet allocated)
expect_equal(nrow(db$fetch("lulc_data_t", where = "id_period = 5")), 0L)

expect_message(
  db$alloc(
    method = "clumpy",
    id_periods = 5L,
    select_score = "classif.auc",
    select_maximize = TRUE,
    seed = 42L
  ),
  "CLUMPY allocation"
)

# Period 5 should now be populated
expect_true(nrow(db$fetch("lulc_data_t", where = "id_period = 5")) > 0L)

# --------------------------------------------------------------------------
# Test error handling
# --------------------------------------------------------------------------
# alloc() with unknown method
expect_error(
  db$alloc(
    method = "unknown_backend",
    id_periods = 4L,
    select_score = "classif.auc",
    select_maximize = TRUE
  )
)

# Non-contiguous periods
expect_error(
  db$alloc_dinamica(
    id_periods = c(1L, 3L)
  ),
  "id_periods must be contiguous"
)

# --------------------------------------------------------------------------
# Test eval_alloc_params_t (Dinamica-only)
# --------------------------------------------------------------------------
expect_message(
  db$alloc_params_t <-
    evaluated_params <-
      db$eval_alloc_params_t(
        select_score = "classif.auc",
        select_maximize = TRUE,
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
    evaluated_params$similarity >= 0
))
