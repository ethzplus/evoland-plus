require(tinytest)

# vanilla R functions
expect_stdout(print(as_trans_models_t()), "Transition Models Table")

# Test creation with data
trans_models_t <- as_trans_models_t(data.table::data.table(
  id_run = 1000L,
  id_trans = 1L,
  model_family = "rf",
  model_params = list(
    list(depth = 100, ntrees = 500)
  ),
  goodness_of_fit = list(
    list(auc = 0.8, rmse = 0.15)
  ),
  fit_call = "fit_fun(data = data)",
  model_obj_part = list(
    charToRaw("partial model data")
  ),
  model_obj_full = list(
    charToRaw("full model data")
  )
))
expect_equal(nrow(trans_models_t), 1L)
expect_stdout(print(trans_models_t), "With full models")

# Gate: skip during R CMD check; run with build_install_test()
if (!at_home()) {
  exit_file("Integration tests skipped (not at_home)")
}

# Load fixtures via helper
source(file.path(
  system.file("tinytest", package = "evoland"),
  "helper_testdb.R"
))
db <- make_test_db(include_neighbors = FALSE, include_trans_preds = TRUE)

# Test fit_partial_models and fit_full_models workflow
# Define a simple mock fit function for testing
fit_mock_glm <- function(data, ...) {
  pred_cols <- grep("^id_pred_", names(data), value = TRUE)

  if (length(pred_cols) == 0) {
    stop("No predictor columns found")
  }

  # Create a simple formula
  formula_str <- paste("did_transition", "~", paste(pred_cols, collapse = " + "))
  formula <- as.formula(formula_str)

  # Fit a simple GLM
  model <- glm(formula, data = data, family = binomial())

  return(model)
}

# Define a goodness of fit function
gof_mock <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data, type = "response")
  actual <- test_data[["did_transition"]]

  # Simple correlation-based metric
  cor_metric <- cor(predictions, actual, use = "complete.obs")

  # Mean squared error
  mse <- mean((predictions - actual)^2, na.rm = TRUE)

  list(
    cor = cor_metric,
    mse = mse,
    n_test = nrow(test_data)
  )
}

# Test fit_partial_models
expect_message(
  partial_models <- db$fit_partial_models(
    fit_fun = fit_mock_glm,
    gof_fun = gof_mock,
    sample_frac = 0.7,
    seed = 123,
    other_param = "nonce"
  ),
  "Fitting partial models for 2 transitions..."
)
expect_length(
  partial_models,
  8L # columns; length is NULL if all fail
)
expect_match(
  partial_models$fit_call,
  r"{function \(data, ..., other_param = "nonce"\)}"
)
expect_equal(
  partial_models$model_params[[1]],
  list(n_predictors = 4, n_train = 748, sample_frac = 0.7, other_param = "nonce")
)
expect_true(all(
  vapply(partial_models$model_obj_part, is.raw, logical(1))
))
expect_equal(
  partial_models$goodness_of_fit,
  list(
    list(cor = 0.01941499, mse = 0.24902958, n_test = 319),
    list(cor = 0.01899595, mse = 0.24918655, n_test = 219)
  ),
  tolerance = 1e-6
)

# Test that model deserialization works
expect_inherits(
  qs2::qs_deserialize(partial_models$model_obj_part[[1]]),
  "glm"
)

# Test fit_full_models, which ensures we can retrieve and evaluate the embedded model function
expect_message(
  full_models <- db$fit_full_models(
    partial_models = partial_models,
    gof_criterion = "cor",
    maximize = TRUE,
  ),
  "Fitting full models for"
)

# test the package's standard rf fit and write to disk
expect_message(
  db$trans_models_t <- db$fit_partial_models(
    fit_fun = fit_ranger,
    gof_fun = gof_ranger,
    seed = 1244244,
  )
)
# test the package's standard glm quasibinomial fit and append to disk
expect_message(
  db$trans_models_t <- db$fit_partial_models(
    fit_fun = fit_glm,
    gof_fun = gof_glm,
    seed = 1244244,
  )
)
expect_message(
  full_models <- db$fit_full_models(
    partial_models = db$trans_models_t[model_family == "ranger"],
    gof_criterion = "auc",
    maximize = TRUE,
  ),
  "Fitting full models for"
)

# test DB round trip
expect_equal(nrow(full_models), 2L)
expect_silent(db$trans_models_t <- full_models)
expect_equal(db$row_count("trans_models_t"), 4L)
full_mods_roundtrip <- db$trans_models_t[id_trans == 2L & model_family == "ranger"]
data.table::setattr(full_mods_roundtrip, "parquet_db_t_class", NULL) # remove for testing equivalence
expect_identical(full_mods_roundtrip, full_models[id_trans == 2L & model_family == "ranger", ])

# Check that both partial and full models are present
expect_true(all(!vapply(full_models$model_obj_part, is.null, logical(1))))
expect_true(all(!vapply(full_models$model_obj_full, is.null, logical(1))))

# Test that full model deserialization works
expect_inherits(
  qs2::qs_deserialize(full_models$model_obj_full[[1]]),
  "ranger"
)

# Test model selection with minimize criterion
expect_message(
  full_models_min <- db$fit_full_models(
    partial_models = partial_models,
    gof_criterion = "mse",
    maximize = FALSE,
  ),
  "Fitting full models for"
)

# Test error handling - missing fit_fun parameter
expect_error(
  db$fit_partial_models(
    gof_fun = gof_mock,
    sample_frac = 0.7
  ),
  "argument \"fit_fun\" is missing"
)

# Test error handling - missing gof_fun parameter
expect_error(
  db$fit_partial_models(
    fit_fun = fit_mock_glm,
    sample_frac = 0.7
  ),
  "argument \"gof_fun\" is missing"
)

# Test error handling - fit_fun is not a function
expect_error(
  db$fit_partial_models(
    fit_fun = "not_a_function",
    gof_fun = gof_mock
  ),
  "fit_fun must be a function"
)

# Test error handling - gof_fun is not a function
expect_error(
  db$fit_partial_models(
    fit_fun = fit_mock_glm,
    gof_fun = "not_a_function"
  ),
  "gof_fun must be a function"
)

# Test error handling - invalid sample_frac
expect_error(
  db$fit_partial_models(
    fit_fun = fit_mock_glm,
    gof_fun = gof_mock,
    sample_frac = 0
  ),
  "sample_frac must be between 0 and 1"
)

expect_error(
  db$fit_partial_models(
    fit_fun = fit_mock_glm,
    gof_fun = gof_mock,
    sample_frac = 1
  ),
  "sample_frac must be between 0 and 1"
)

# Test error handling - invalid partial_models argument to fit_full_models
expect_error(
  db$fit_full_models(
    partial_models = "not_a_trans_models_t",
    gof_criterion = "cor"
  ),
  "partial_models must be a trans_models_t"
)

# Test error handling - empty partial_models
empty_models <- as_trans_models_t()
expect_error(
  db$fit_full_models(
    partial_models = empty_models,
    gof_criterion = "cor"
  ),
  "partial_models is empty"
)

# Test fit function that throws an error
fit_error <- function(data, ...) {
  stop("Intentional error for testing")
}

expect_warning(
  partial_models_error <-
    db$fit_partial_models(
      fit_fun = fit_error,
      gof_fun = gof_mock,
      sample_frac = 0.7,
      seed = 123
    ),
  "Intentional error for testing"
)

# Should return NULL when all transitions fail
expect_equal(
  partial_models_error,
  as_trans_models_t(data.table::data.table(
    id_run = 0L,
    id_trans = 1:2,
    model_family = "error",
    model_params = list(NULL),
    goodness_of_fit = list(
      list(failed = TRUE, message = "Intentional error for testing"),
      list(failed = TRUE, message = "Intentional error for testing")
    ),
    fit_call = "function (data, ...) \n {\n     stop(\"Intentional error for testing\")\n }",
    model_obj_part = list(NULL),
    model_obj_full = list(NULL)
  ))
)

# Test print method
expect_stdout(
  print(full_models),
  "Transition Models Table|Total models"
)
