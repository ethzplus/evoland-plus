#' Guided Regularized Random Forest Feature Selection
#'
#' The `grrf_filter` returns a set of covariates for land use land cover change (LULCC) models based
#' on feature selection with Guided Regularized Random Forests. This is a two-stage random forest
#' approach: a first unregularized random forest estimates variable importance scores. These scores
#' are then used to guide a second regularized random forest that penalizes less important features,
#' resulting in a more parsimonious feature set.
#'
#' @param data A data.table of target variable and candidate covariates to be filtered; wide format
#'        with one predictor per column.
#' @param result_col Name of the column representing the transition results (0: no trans, 1: trans)
#' @param weights Optional named vector of class weights. If NULL, class-balanced weights
#'        are computed automatically using compute_grrf_weights().
#' @param gamma Numeric between 0-1 controlling the weight of the normalized importance
#'        score (the "importance coefficient"). When gamma = 0, we perform unguided
#'        regularized random forest (no guiding effect). When gamma = 1, we apply the
#'        strongest guiding effect, leading to the most penalization of redundant
#'        features and the most concise feature sets. Default is 0.5.
#' @param num.trees Number of trees to grow in each random forest. Default is 500.
#' @param ... Additional arguments passed to ranger::ranger().
#'
#' @return A character vector of column names (covariates) to retain, ordered by
#'         importance (most important first)
#'
#' @details
#' The Guided Regularized Random Forest (GRRF) algorithm works as follows:
#' 1. Fit an initial unregularized random forest to obtain variable importance scores
#' 2. Normalize these importance scores and use them to compute regularization
#'    coefficients: coefReg = (1 - gamma) + gamma * normalized_importance
#' 3. Fit a regularized random forest using these coefficients to penalize splits
#'    on less important variables
#' 4. Return variables with positive importance in the regularized model
#'
#' Class weights are used to handle class imbalance. Variables in terminal nodes are
#' weighted by class, and splits are evaluated using weighted Gini impurity.
#'
#' The ranger implementation uses the `split.select.weights` parameter to apply
#' regularization penalties, approximating the RRF regularization approach.
#'
#' @references
#' Deng, H., & Runger, G. (2013). Gene selection with guided regularized random forest.
#' Pattern Recognition, 46(12), 3483-3489. https://arxiv.org/pdf/1306.0237.pdf
#'
#' Original implementation by Antoine Adde, edited by Ben Black and adapted for
#' ranger by the evoland-plus team.
#'
#' @name grrf_filter
#'
#' @export

grrf_filter <- function(
  data,
  result_col = "result",
  weights = compute_balanced_weights(data[[result_col]]),
  gamma = 0.5,
  num.trees = 500,
  max.depth = 100,
  ...
) {
  # Check if ranger is available
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop(
      "Package 'ranger' is required for grrf_filter but is not installed.\n",
      "Please install it with: install.packages('ranger')",
      call. = FALSE
    )
  }

  data.table::setDT(data)

  # Validate inputs
  stopifnot(
    "gamma must be between 0 and 1" = gamma >= 0 && gamma <= 1,
    "result_col must exist in data" = result_col %in% names(data),
    "data must have at least one predictor column" = ncol(data) > 1
  )

  # Prepare data: separate predictors from response
  predictor_cols <- setdiff(names(data), result_col)
  y <- as.factor(data[[result_col]])
  x <- data[, ..predictor_cols]

  # Step 1: Run initial unregularized random forest to get importance scores
  rf_initial <- ranger::ranger(
    x = x,
    y = y,
    num.trees = num.trees,
    importance = "impurity",
    case.weights = weights,
    max.depth = max.depth,
    ...
  )

  # Extract and normalize importance scores
  imp_initial <- rf_initial$variable.importance
  # Normalize to [0,1] (importance values may be negative)
  imp_normalized <- {
    (imp_initial - min(imp_initial)) /
      (max(imp_initial) - min(imp_initial))
  }

  # Step 2: Calculate regularization coefficients (penalty weights)
  # Higher importance -> higher coefficient -> less penalty
  coef_reg <- (1 - gamma) + gamma * imp_normalized

  # Step 3: Run guided regularized random forest
  # Use split.select.weights to penalize variables with lower importance
  # Higher weight = more likely to be selected for splitting
  # FIXME silence expected warning about split weights without silencing progress output
  rf_grrf <- ranger::ranger(
    x = x,
    y = y,
    num.trees = num.trees,
    importance = "impurity",
    case.weights = weights,
    max.depth = max.depth,
    split.select.weights = coef_reg,
    ...
  )

  # Extract final importance scores
  imp_final <- rf_grrf$variable.importance

  # Select variables with positive importance
  selected_vars <- names(imp_final[imp_final > 0])

  if (length(selected_vars) == 0) {
    warning("No variables with positive importance found. Returning all variables.")
    selected_vars <- predictor_cols
  }

  # Order by importance (descending)
  selected_vars <- selected_vars[order(imp_final[selected_vars], decreasing = TRUE)]

  message(glue::glue(
    "Selected {length(selected_vars)}/{length(predictor_cols)} predictors"
  ))

  return(selected_vars)
}
