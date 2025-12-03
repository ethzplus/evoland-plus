#' Random Forest Model Fitting for Transition Models
#'
#' Fits a random forest model using the ranger package for transition modeling.
#' Uses observation-based weighting and stratified downsampling to handle class
#' imbalance.
#'
#' @param data A data.table containing the result column and predictor columns
#'   (prefixed with "id_pred_")
#' @param result_col Name of the column representing the transition results
#'   (logical: TRUE = transition occurred, FALSE = no transition)
#' @param ... Additional arguments (currently unused, for future extensibility)
#'
#' @return A fitted ranger model object, optionally butchered to reduce memory footprint
#'
#' @details
#' The function:
#' - Uses ranger for efficient random forest implementation
#' - Applies observation-based weights (same approach as grrf_filter)
#' - Uses stratified downsampling via sample.fraction
#' - Returns probability predictions for the positive class
#' - Computes variable importance using impurity measure
#' - Applies butcher::butcher() if available to reduce model size
#'
#' Default hyperparameters:
#' - num.trees = 500
#' - mtry = floor(sqrt(n_predictors))
#' - min.node.size = 1
#'
#' @export
fit_ranger <- function(data, result_col = "result", num.trees = 500, ...) {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop(
      "Package 'ranger' is required but is not installed.\n",
      "Please install it with: install.packages('ranger')",
      call. = FALSE
    )
  }

  pred_cols <- grep("^id_pred_", names(data), value = TRUE)

  if (length(pred_cols) == 0) {
    stop("No predictor columns found (expected columns starting with 'id_pred_')")
  }

  # Prepare data
  x <- data[, ..pred_cols]
  y <- as.factor(data[[result_col]])

  # Compute observation-based weights (same approach as grrf_filter)
  weights <- compute_balanced_weights(data[[result_col]])

  # Get minority class size for downsampling
  class_counts <- table(y)
  nmin <- min(class_counts)

  # Fit ranger model
  model <- ranger::ranger(
    x = x,
    y = y,
    num.trees = num.trees,
    case.weights = weights,
    probability = TRUE, # For probability predictions
    importance = "impurity",
    ...
  )

  # Butcher the model if package is available
  if (requireNamespace("butcher", quietly = TRUE)) {
    model <- butcher::butcher(model)
  }

  return(model)
}


#' Goodness of Fit Evaluation for Random Forest Models
#'
#' Evaluates the goodness of fit for a ranger random forest model on test data,
#' computing multiple performance metrics including correlation, MSE, AUC, and
#' out-of-bag error.
#'
#' @param model A fitted ranger model object (from fit_ranger)
#' @param test_data A data.table containing test data with the same structure as
#'   training data
#' @param result_col Name of the column representing the transition results
#' @param ... Additional arguments (currently unused, for future extensibility)
#'
#' @return A named list containing:
#'   - `cor`: Pearson correlation between predictions and actual values
#'   - `mse`: Mean squared error
#'   - `auc`: Area under the ROC curve (if pROC package is available)
#'   - `oob_error`: Out-of-bag prediction error from the model
#'   - `n_test`: Number of test observations
#'
#' @details
#' The function extracts probability predictions for the TRUE class from the ranger
#' model. It uses the pROC package for AUC calculation if available. If pROC is not
#' installed, AUC will be NA.
#'
#' @export
gof_ranger <- function(model, test_data, result_col = "result", ...) {
  pred_cols <- grep("^id_pred_", names(test_data), value = TRUE)
  x_test <- test_data[, ..pred_cols]

  # Get probability predictions for the TRUE class
  predictions <- predict(model, data = x_test)$predictions[, "TRUE"]
  actual <- as.numeric(test_data[[result_col]])

  # Correlation-based metric
  cor_metric <- cor(predictions, actual, use = "complete.obs")

  # Mean squared error
  mse <- mean((predictions - actual)^2, na.rm = TRUE)

  # AUC if pROC is available
  auc <- NA_real_
  if (requireNamespace("pROC", quietly = TRUE)) {
    roc_obj <- pROC::roc(
      actual,
      predictions,
      quiet = TRUE,
      direction = "<"
    )
    auc <- as.numeric(pROC::auc(roc_obj))
  }

  # Out-of-bag error from model
  oob_error <- model$prediction.error

  list(
    cor = cor_metric,
    mse = mse,
    auc = auc,
    oob_error = oob_error,
    n_test = nrow(test_data)
  )
}
