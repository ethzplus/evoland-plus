#' GLM Model Fitting for Transition Models
#'
#' Fits a generalized linear model (GLM) with quasibinomial family for transition
#' modeling. The quasibinomial family is recommended over binomial as it better
#' handles overdispersion in the data.
#'
#' @param data A data.table containing the result column and predictor columns
#'   (prefixed with "id_pred_")
#' @param result_col Name of the column representing the transition results
#'   (logical: TRUE = transition occurred, FALSE = no transition)
#' @param ... Additional arguments (currently unused, for future extensibility)
#'
#' @return A fitted GLM model object, optionally butchered to reduce memory footprint
#'
#' @details
#' The function:
#' - Uses quasibinomial family to handle overdispersion
#' - Automatically detects predictor columns (those starting with "id_pred_")
#' - Applies butcher::butcher() if the package is available to reduce model size
#'
#' @export
fit_glm <- function(data, result_col = "result", ...) {
  pred_cols <- grep("^id_pred_", names(data), value = TRUE)

  if (length(pred_cols) == 0) {
    stop("No predictor columns found (expected columns starting with 'id_pred_')")
  }

  # Create formula
  formula_str <- paste(result_col, "~", paste(pred_cols, collapse = " + "))
  formula <- as.formula(formula_str)

  # Fit GLM with quasibinomial family (handles overdispersion better)
  model <- glm(formula, data = data, family = quasibinomial())

  # Butcher the model if package is available (reduces memory footprint)
  if (requireNamespace("butcher", quietly = TRUE)) {
    model <- butcher::butcher(model)
  }

  return(model)
}


#' Goodness of Fit Evaluation for GLM Models
#'
#' Evaluates the goodness of fit for a GLM model on test data, computing multiple
#' performance metrics including correlation, MSE, and AUC.
#'
#' @param model A fitted GLM model object (from fit_glm)
#' @param test_data A data.table containing test data with the same structure as
#'   training data
#' @param result_col Name of the column representing the transition results
#' @param ... Additional arguments (currently unused, for future extensibility)
#'
#' @return A named list containing:
#'   - `cor`: Pearson correlation between predictions and actual values
#'   - `mse`: Mean squared error
#'   - `auc`: Area under the ROC curve (if pROC package is available)
#'   - `n_test`: Number of test observations
#'
#' @details
#' The function uses the pROC package for AUC calculation if available. If pROC
#' is not installed, AUC will be NA.
#'
#' @export
gof_glm <- function(model, test_data, result_col = "result", ...) {
  predictions <- predict(model, newdata = test_data, type = "response")
  actual <- test_data[[result_col]]

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

  list(
    cor = cor_metric,
    mse = mse,
    auc = auc,
    n_test = nrow(test_data)
  )
}
