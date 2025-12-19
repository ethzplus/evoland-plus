library(tinytest)

# Test creation and validation of empty trans_models_t
trans_models_empty <- as_trans_models_t()
expect_stdout(print(trans_models_empty), "Transition Models Table")
expect_equal(nrow(trans_models_empty), 0L)
expect_inherits(trans_models_empty, "trans_models_t")

# Test creation with data
trans_models_t <- as_trans_models_t(data.table::data.table(
  id_trans = 1L,
  model_family = "rf",
  model_params = list(
    list(depth = 100, ntrees = 500)
  ),
  goodness_of_fit = list(
    list(auc = 0.8, rmse = 0.15)
  ),
  fit_call = "fit_fun(data = data, result_col = \"result\")",
  model_obj_part = list(
    charToRaw("partial model data")
  ),
  model_obj_full = list(
    charToRaw("full model data")
  )
))
expect_equal(nrow(trans_models_t), 1L)
expect_stdout(print(trans_models_t), "With full models")

# Test fit_partial_models and fit_full_models workflow
test_dir_trans_models <- tempfile("evoland_trans_models_")
on.exit(unlink(test_dir_trans_models, recursive = TRUE), add = TRUE)
db_tm <- evoland_db$new(test_dir_trans_models)

# Set up coords and periods with 30x30 domain
db_tm$coords_t <- create_coords_t_square(
  epsg = 2056,
  extent = terra::ext(c(xmin = 2697000, xmax = 2700000, ymin = 1252000, ymax = 1255000)),
  resolution = 100
)

db_tm$periods_t <- create_periods_t(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2005-01-01",
  end_extrapolated = "2015-01-01"
)

# Set up LULC classes
db_tm$lulc_meta_t <- create_lulc_meta_t(list(
  forest = list(pretty_name = "Forest", src_classes = 1L),
  urban = list(pretty_name = "Urban", src_classes = 2L),
  agriculture = list(pretty_name = "Agriculture", src_classes = 3L)
))

# Create synthetic LULC data with known transitions
set.seed(42)
n_coords <- nrow(db_tm$coords_t)
lulc_data <- data.table::rbindlist(list(
  data.table::data.table(
    id_coord = 1:n_coords,
    id_lulc = sample(1:2, n_coords, replace = TRUE, prob = c(0.7, 0.3)),
    id_period = 1L
  ),
  data.table::data.table(
    id_coord = 1:n_coords,
    id_lulc = sample(1:2, n_coords, replace = TRUE, prob = c(0.5, 0.5)),
    id_period = 2L
  ),
  data.table::data.table(
    id_coord = 1:n_coords,
    id_lulc = sample(1:2, n_coords, replace = TRUE, prob = c(0.4, 0.6)),
    id_period = 3L
  )
))
db_tm$lulc_data_t <- as_lulc_data_t(lulc_data)

# Create transition metadata
transitions <- db_tm$trans_v
db_tm$trans_meta_t <- create_trans_meta_t(
  transitions,
  min_cardinality_abs = 5L
)

# Add predictor metadata
pred_spec_tm <- list(
  elevation = list(
    unit = "m",
    pretty_name = "Elevation",
    description = "Elevation above sea level",
    sources = list(list(url = "https://example.com/elevation.tif", md5sum = "abc123"))
  ),
  slope = list(
    unit = "degrees",
    pretty_name = "Slope",
    description = "Terrain slope",
    sources = list(list(url = "https://example.com/slope.tif", md5sum = "def456"))
  ),
  distance_to_road = list(
    unit = "m",
    pretty_name = "Distance to road",
    description = "Distance to nearest road",
    sources = list(list(url = "https://example.com/roads.gpkg", md5sum = "ghi789"))
  )
)
expect_silent(
  db_tm$pred_meta_t <- create_pred_meta_t(pred_spec_tm)
)

# Add predictor data - mix of static and time-varying
set.seed(43)
pred_data_static <- data.table::rbindlist(list(
  # Elevation (static, period 0)
  data.table::data.table(
    id_pred = 1L,
    id_coord = 1:n_coords,
    id_period = 0L,
    value = runif(n_coords, 400, 800)
  ),
  # Slope (static, period 0)
  data.table::data.table(
    id_pred = 2L,
    id_coord = 1:n_coords,
    id_period = 0L,
    value = runif(n_coords, 0, 30)
  )
))

# Time-varying predictor
pred_data_varying <- data.table::rbindlist(lapply(1:3, function(period) {
  data.table::data.table(
    id_pred = 3L, # distance_to_road
    id_coord = 1:n_coords,
    id_period = period,
    value = runif(n_coords, 0, 5000)
  )
}))

db_tm$pred_data_t_float <- as_pred_data_t(
  rbind(pred_data_static, pred_data_varying),
  type = "float"
)

db_tm$set_full_trans_preds()

# Test trans_pred_data_v
full_data <- db_tm$trans_pred_data_v(1L)
expect_equal(nrow(full_data), 733L)
expect_true("result" %in% names(full_data))
expect_true("id_coord" %in% names(full_data))
expect_true("id_period" %in% names(full_data))
expect_true(any(grepl("^id_pred_", names(full_data))))

# Define a simple mock fit function for testing
fit_mock_glm <- function(data, result_col = "result", ...) {
  pred_cols <- grep("^id_pred_", names(data), value = TRUE)

  if (length(pred_cols) == 0) {
    stop("No predictor columns found")
  }

  # Create a simple formula
  formula_str <- paste(result_col, "~", paste(pred_cols, collapse = " + "))
  formula <- as.formula(formula_str)

  # Fit a simple GLM
  model <- glm(formula, data = data, family = binomial())

  return(model)
}

# Define a goodness of fit function
gof_mock <- function(model, test_data, result_col = "result", ...) {
  predictions <- predict(model, newdata = test_data, type = "response")
  actual <- test_data[[result_col]]

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
  partial_models <- db_tm$fit_partial_models(
    fit_fun = fit_mock_glm,
    gof_fun = gof_mock,
    sample_pct = 70,
    seed = 123,
    na_value = 0,
    other_param = "nonce"
  ),
  "Fitting partial model"
)
expect_equal(
  partial_models$fit_call[1],
  r"{fit_mock_glm(data = data, result_col = "result", other_param = "nonce")}"
)
expect_equal(
  partial_models$model_params[[1]],
  list(n_predictors = 3, n_train = 514, sample_pct = 70, other_param = "nonce")
)
expect_true(all(
  !vapply(partial_models$model_obj_part, is.null, logical(1))
))
expect_equal(
  partial_models$goodness_of_fit[[1]],
  list(cor = -0.06258646, mse = 0.2519054, n_test = 219),
  tolerance = 1e07
)

# Test that model deserialization works
first_model_part <- qs2::qs_deserialize(partial_models$model_obj_part[[1]])
expect_inherits(first_model_part, "glm")

# test DB round trip
expect_silent(db_tm$trans_models_t <- partial_models)
expect_equivalent(db_tm$trans_models_t, partial_models)

# Test fit_full_models
expect_message(
  full_models <- db_tm$fit_full_models(
    partial_models = db_tm$trans_models_t,
    gof_criterion = "cor",
    maximize = TRUE,
    na_value = 0
  ),
  "Full model fitted"
)

# test DB round trip
expect_silent(db_tm$trans_models_t <- full_models)
expect_identical(db_tm$trans_models_t, full_models)

# Check that both partial and full models are present
expect_true(all(!vapply(full_models$model_obj_part, is.null, logical(1))))
expect_true(all(!vapply(full_models$model_obj_full, is.null, logical(1))))

# Test that full model deserialization works
first_model_full <- qs2::qs_deserialize(full_models$model_obj_full[[1]])
expect_inherits(first_model_full, "glm")

# Test model selection with minimize criterion
expect_message(
  full_models_min <- db_tm$fit_full_models(
    partial_models = partial_models,
    gof_criterion = "mse",
    maximize = FALSE,
    na_value = 0
  ),
  "selected by mse="
)

# Test error handling - missing fit_fun parameter
expect_error(
  db_tm$fit_partial_models(
    gof_fun = gof_mock,
    sample_pct = 70
  ),
  "argument \"fit_fun\" is missing"
)

# Test error handling - missing gof_fun parameter
expect_error(
  db_tm$fit_partial_models(
    fit_fun = fit_mock_glm,
    sample_pct = 70
  ),
  "argument \"gof_fun\" is missing"
)

# Test error handling - fit_fun is not a function
expect_error(
  db_tm$fit_partial_models(
    fit_fun = "not_a_function",
    gof_fun = gof_mock
  ),
  "fit_fun must be a function"
)

# Test error handling - gof_fun is not a function
expect_error(
  db_tm$fit_partial_models(
    fit_fun = fit_mock_glm,
    gof_fun = "not_a_function"
  ),
  "gof_fun must be a function"
)

# Test error handling - invalid sample_pct
expect_error(
  db_tm$fit_partial_models(
    fit_fun = fit_mock_glm,
    gof_fun = gof_mock,
    sample_pct = 0
  ),
  "sample_pct must be between 0 and 100"
)

expect_error(
  db_tm$fit_partial_models(
    fit_fun = fit_mock_glm,
    gof_fun = gof_mock,
    sample_pct = 100
  ),
  "sample_pct must be between 0 and 100"
)

# Test error handling - empty trans_preds_t
test_dir_no_preds <- tempfile("evoland_no_preds_")
on.exit(unlink(test_dir_no_preds, recursive = TRUE), add = TRUE)
db_no_preds <- evoland_db$new(test_dir_no_preds)
db_no_preds$coords_t <- db_tm$coords_t
db_no_preds$periods_t <- db_tm$periods_t
db_no_preds$lulc_meta_t <- db_tm$lulc_meta_t
db_no_preds$lulc_data_t <- db_tm$lulc_data_t
expect_warning(
  db_no_preds$trans_meta_t <- db_tm$trans_meta_t,
  "Overriding existing IDs"
)
expect_warning(db_no_preds$pred_meta_t <- db_tm$pred_meta_t, "Assign these IDs manually")

expect_error(
  db_no_preds$fit_partial_models(
    fit_fun = fit_mock_glm,
    gof_fun = gof_mock
  ),
  "trans_preds_t"
)

# Test error handling - invalid partial_models argument to fit_full_models
expect_error(
  db_tm$fit_full_models(
    partial_models = "not_a_trans_models_t",
    gof_criterion = "cor"
  ),
  "partial_models must be a trans_models_t"
)

# Test error handling - empty partial_models
empty_models <- as_trans_models_t()
expect_error(
  db_tm$fit_full_models(
    partial_models = empty_models,
    gof_criterion = "cor"
  ),
  "partial_models is empty"
)

# Test error handling - missing gof_criterion
expect_error(
  db_tm$fit_full_models(
    partial_models = partial_models
  ),
  "argument \"gof_criterion\" is missing"
)

# Test fit function that throws an error
fit_error <- function(data, result_col = "result", ...) {
  stop("Intentional error for testing")
}

expect_warning(
  partial_models_error <-
    db_tm$fit_partial_models(
      fit_fun = fit_error,
      gof_fun = gof_mock,
      sample_pct = 70,
      seed = 123
    ),
  "No models fitted for any transition"
)

# Should return NULL when all transitions fail
expect_true(is.null(partial_models_error))

# Test print method with various scenarios
expect_stdout(
  print(full_models),
  "Transition Models Table|Total models"
)

# Test that fit_call can be parsed back to a call (verifying serializability)
first_call_str <- full_models$fit_call[1]
expect_true(is.character(first_call_str))
expect_true(nchar(first_call_str) > 0)

first_call_parsed <- str2lang(first_call_str)
expect_true(is.call(first_call_parsed))
expect_equal(as.character(first_call_parsed[[1]]), "fit_mock_glm")

# FIXME: This test for create_alloc_params_t() and the buildup above should be spun out
# into a full integration test suite eventually that tests the complete workflow from
# data setup through allocation parameter estimation

# We can use the existing db_tm setup which already has:
# - coords_t, periods_t, lulc_meta_t, lulc_data_t, trans_meta_t
if (requireNamespace("landscapemetrics", quietly = TRUE)) {
  expect_message(
    alloc_params <- db_tm$create_alloc_params_t(n_perturbations = 3, sd = 10),
    "Successfully computed 8 allocation parameter sets" # 8 = (3 perturb + 1 estim) * 2 periods
  )
  expect_inherits(alloc_params, "alloc_params_t")

  # Check that alloc_params contains expected parameters
  # With 30x30 domain, values will differ from small domain
  expect_true(nrow(alloc_params) == 8) # (3 perturb + 1 estim) * 2 periods
  expect_true(all(
    c(
      "id_perturbation",
      "id_trans",
      "mean_patch_size",
      "patch_isometry",
      "frac_expander",
      "frac_patcher"
    ) %in%
      names(alloc_params)
  ))
  expect_true(all(alloc_params$frac_expander >= 0 & alloc_params$frac_expander <= 1))
  expect_true(all(alloc_params$frac_patcher >= 0 & alloc_params$frac_patcher <= 1))
  expect_true(all(alloc_params$mean_patch_size > 0, na.rm = TRUE))

  # DB roundtrip; upsert on id_perturbation, id_trans
  expect_silent(db_tm$alloc_params_t <- alloc_params)
  expect_silent(db_tm$alloc_params_t <- alloc_params)
  expect_equal(db_tm$alloc_params_t, alloc_params)

  # Edge case: no perturbations
  expect_message(
    db_tm$create_alloc_params_t(n_perturbations = 0L),
    r"(0 perturbations \+ best estimate)"
  )

  expect_error(
    db_tm$create_alloc_params_t(sd = -5),
    "sd must be a positive number"
  )

  # Test allocation and evaluation workflow
  # Note: This requires Dinamica EGO to be installed and on PATH
  # Skip if DinamicaConsole is not available
  if (Sys.which("DinamicaConsole") != "") {
    # We need trans_rates_t for allocation
    # Create simple transition rates for testing
    trans_rates <- data.table::data.table(
      id_trans = 1L,
      id_period = 1:3,
      rate = 0.1
    )
    db_tm$trans_rates_t <- as_trans_rates_t(trans_rates)

    # Commit the models so they can be used for prediction
    expect_silent(db_tm$trans_models_t <- full_models)

    # Test alloc_dinamica with a simple two-period simulation
    expect_message(
      sim_table <- db_tm$alloc_dinamica(
        id_periods = 1:3,
        id_perturbation = 1L,
        work_dir = file.path(test_dir_trans_models, "dinamica_test"),
        keep_intermediate = FALSE
      ),
      "Simulation complete"
    )

    # Check that results table was created
    expect_true(sim_table %in% db_tm$list_tables())
    expect_equal(unclass(sim_table), "lulc_data_t_perturbation_1")

    # Check simulation results
    expect_silent(sim_results <- db_tm$fetch(sim_table) |> as_lulc_data_t())
    expect_equivalent(unique(sim_results$id_period), 2:3)

    # Test eval_alloc_params_t
    # This will re-run the simulation and compare against observed data
    expect_message(
      evaluated_params <- db_tm$eval_alloc_params_t(
        id_perturbations = 1L,
        work_dir = file.path(test_dir_trans_models, "dinamica_eval"),
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
      db_tm$alloc_dinamica(
        id_periods = c(1L, 3L), # Non-contiguous
        id_perturbation = 1L
      ),
      "id_periods must be contiguous"
    )

    # Test error handling - invalid id_perturbation
    expect_error(
      db_tm$alloc_dinamica(
        id_periods = 1:2,
        id_perturbation = 999L # Doesn't exist
      ),
      "id_perturbation=999 not found in alloc_params_t"
    )
  } else {
    message("Skipping Dinamica allocation tests: DinamicaConsole not found on PATH")
  }
}
