library(tinytest)

# vanilla R functions
expect_stdout(print(as_trans_models_t()), "Transition Models Table")

# Test creation with data
trans_models_t <- as_trans_models_t(data.table::data.table(
  id_run = 1000L,
  id_trans = 1L,
  learner_id = "classif.featureless",
  learner_params = list(
    list(method = "mode")
  ),
  learner_spec = list(
    charToRaw("learner spec blob")
  ),
  crossval_score = list(
    list(classif.auc = 0.8)
  ),
  crossval_predictions = list(
    charToRaw("predictions blob")
  ),
  learner_full = list(
    charToRaw("full learner blob")
  )
))
expect_equal(nrow(trans_models_t), 1L)
expect_stdout(print(trans_models_t), "With full models")

# Gate: skip during R CMD check; run with build_install_test()
if (!at_home()) {
  exit_file("Integration tests skipped (not at_home)")
}

if (!requireNamespace("mlr3", quietly = TRUE)) {
  exit_file("mlr3 not available; skipping integration tests")
}

# Load fixtures via helper
source(file.path(
  system.file("tinytest", package = "evoland"),
  "helper_testdb.R"
))
db <- make_test_db(include_neighbors = FALSE, include_trans_preds = TRUE)

# Use a simple featureless learner for fast, dependency-free testing
test_learner <- mlr3::lrn("classif.featureless", predict_type = "prob")
# measures can be passed as a character vector of IDs (convenience) or as a list of Measure objects
test_measures <- c("classif.auc", "classif.acc")

# Test fit_partial_models
expect_message(
  db$trans_models_t <- partial_models <- db$fit_partial_models(
    learner = test_learner,
    measures = test_measures,
    sample_frac = 0.7,
    seed = 123
  ),
  "Fitting partial models for 2 transitions..."
)
expect_length(
  partial_models,
  8L # columns
)
expect_true(all(
  c(
    "id_run",
    "id_trans",
    "learner_id",
    "learner_params",
    "learner_spec",
    "crossval_score",
    "crossval_predictions",
    "learner_full"
  ) %in%
    names(partial_models)
))
expect_equal(
  unique(partial_models$learner_id),
  "classif.featureless"
)
expect_true(all(
  vapply(partial_models$learner_spec, is.raw, logical(1))
))
expect_true(all(
  vapply(partial_models$crossval_predictions, is.raw, logical(1))
))
expect_true(all(
  vapply(partial_models$learner_full, is.null, logical(1))
))

# Test that learner_spec deserializes to an mlr3 Learner
deserialized_spec <- qs2::qs_deserialize(partial_models$learner_spec[[1]])
expect_true(inherits(deserialized_spec, "Learner"))
expect_equal(deserialized_spec$id, "classif.featureless")

# crossval_score should be named lists with measure IDs as keys
expect_true(all(vapply(
  partial_models$crossval_score,
  function(m) !is.null(m) && is.list(m) && "classif.auc" %in% names(m),
  logical(1)
)))

# Test fit_full_models, which reconstructs and retrains on full data
expect_message(
  db$trans_models_t <- full_models <- db$fit_full_models(
    learner = test_learner,
    measures = test_measures,
    gof_criterion = "classif.auc",
    gof_maximize = TRUE
  ),
  "Fitting full models for"
)

# Test DB round trip
expect_equal(nrow(full_models), 2L)
full_mods_roundtrip <- db$trans_models_t[id_trans == 2L & learner_id == "classif.featureless"]
data.table::setattr(full_mods_roundtrip, "parquet_db_t_class", NULL)
expect_identical(
  full_mods_roundtrip,
  full_models[id_trans == 2L & learner_id == "classif.featureless"]
)

# Check that both crossval_predictions and learner_full are present
expect_true(all(!vapply(full_models$crossval_predictions, is.null, logical(1))))
expect_true(all(!vapply(full_models$learner_full, is.null, logical(1))))

# Test that learner_full deserializes to a trained mlr3 Learner
deserialized_full <- qs2::qs_deserialize(full_models$learner_full[[1]])
expect_true(inherits(deserialized_full, "Learner"))
expect_false(is.null(deserialized_full$model))

# Test model selection with minimize criterion
expect_message(
  full_models_min <- db$fit_full_models(
    learner = test_learner,
    measures = test_measures,
    gof_criterion = "classif.acc",
    gof_maximize = FALSE
  ),
  "Fitting full models for"
)

# Test error handling - missing learner parameter
expect_error(
  db$fit_partial_models(
    measures = test_measures,
    sample_frac = 0.7
  ),
  "argument \"learner\" is missing"
)

# Test error handling - learner is not an mlr3 Learner
expect_error(
  db$fit_partial_models(
    learner = "not_a_learner",
    measures = test_measures
  ),
  "learner must be an mlr3 Learner or AutoTuner"
)

# Test error handling - invalid sample_frac
expect_error(
  db$fit_partial_models(
    learner = test_learner,
    measures = test_measures,
    sample_frac = 0
  ),
  "sample_frac must be between 0 and 1"
)

expect_error(
  db$fit_partial_models(
    learner = test_learner,
    measures = test_measures,
    sample_frac = 1
  ),
  "sample_frac must be between 0 and 1"
)

# Test error handling - missing trans_models_t for full model fitting
db$delete_from("trans_models_t")
expect_error(
  db$fit_full_models(
    learner = test_learner,
    measures = test_measures,
    gof_criterion = "classif.auc",
    gof_maximize = TRUE
  ),
  "trans_models_t is missing"
)

# Test fit function that throws an error (simulate via a bad learner)
# We achieve this by running with a learner that fails (no viable data after
# deleting the table above means we re-populate and then use a bad setup).
# Instead, test via a mock that warns on error using an out-of-range sample_frac
# -> use a real error scenario by removing all preds
db2 <- make_test_db(include_neighbors = FALSE, include_trans_preds = FALSE)
db2$trans_preds_t <- as_trans_preds_t(data.table::data.table(
  id_run = 0L,
  id_pred = 99999L, # non-existent predictor
  id_trans = 1L
))

expect_warning(
  partial_models_error <-
    db2$fit_partial_models(
      learner = test_learner,
      measures = test_measures,
      sample_frac = 0.7,
      seed = 123
    ),
  "No predictor columns|No data"
)
expect_equal(
  partial_models_error$learner_id,
  "error"
)

# Test print method
db3 <- make_test_db(include_neighbors = FALSE, include_trans_preds = TRUE)
db3$trans_models_t <- db3$fit_partial_models(
  learner = test_learner,
  measures = test_measures,
  seed = 42
)
db3$trans_models_t <- db3$fit_full_models(
  learner = test_learner,
  measures = test_measures,
  gof_criterion = "classif.auc",
  gof_maximize = TRUE
)
expect_stdout(
  print(db3$trans_models_t),
  "Transition Models Table|Total models"
)
