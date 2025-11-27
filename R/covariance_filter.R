#' Filter covariates for land use land cover change (LULCC) models
#'
#' This function filters a set of covariates for land use land cover change (LULCC)
#' models based on various statistical methods and correlation thresholds.
#'
#' @param data A data.table of target variable and candidate covariates to be filtered;
#'        wide format with one predictor per column.
#' @param result_col Name of the column representing the transition results (0: no
#'        trans, 1: trans)
#' @param rank_fun Optional function to compute ranking scores for each covariate.
#'        Should take arguments (x, y, weights, ...) and return a single numeric value
#'        (lower = better). Defaults to polynomial GLM p-value ranking.
#' @param weights Optional vector of weights to be used in the ranking function.
#'        If NULL and rank_fun uses default, class-balanced weights are computed automatically.
#' @param corcut Numeric threshold (0-1) for correlation filtering. Covariates with correlation
#'        coefficients above this threshold will be filtered out. Default is 0 (no filtering).
#' @param ... Additional arguments passed to rank_fun.
#'
#' @return A filtered data.table containing only the selected covariates after ranking
#'         by the specified method and filtering based on correlation threshold.
#'
#' @details
#' The function first ranks covariates using the provided ranking function (default:
#' quasibinomial polynomial GLM). Then, it iteratively removes highly correlated variables
#' based on the correlation cutoff threshold, preserving variables in order of their
#' ranking. See
#' <https://github.com/ethzplus/evoland-plus-legacy/blob/main/R/lulcc.covfilter.r> for
#' where the concept came from. The original author was Antoine Adde, with edits by
#' Benjamin Black.
#'
#' @name covariance_filter
#'
#' @export

covariance_filter <- function(
  data,
  result_col = "result",
  rank_fun = rank_poly_glm,
  weights = compute_balanced_weights(data[[result_col]]),
  corcut = 0.7,
  ...
) {
  # Early return for single covariate
  if (ncol(data) == 1) {
    return(data)
  }

  data.table::setDT(data)

  # Validate binary outcome
  stopifnot(
    "result_col must be binary (0/1)" = length(unique(data[[result_col]])) == 2,
    "corcut must be between 0 and 1" = corcut >= 0 && corcut <= 1
  )

  # Compute ranking scores for all covariates (vectorized where possible)
  scores <- vapply(
    data[, -..result_col],
    rank_fun,
    FUN.VALUE = numeric(1),
    y = data[[result_col]],
    weights = weights,
    ...
  )

  # Sort by scores (lower = better/more significant)
  ranked_order <- order(scores)
  data_ranked <- data[, ..ranked_order]

  # If no correlation filtering needed, return ranked data
  if (corcut == 1) {
    return(data_ranked)
  }

  # Compute correlation matrix once
  cor_mat <- abs(cor(data_ranked, use = "pairwise.complete.obs"))

  # Iteratively select covariates based on correlation threshold
  selected <- select_by_correlation(cor_mat, corcut)

  # Return selected covariates
  data_ranked[, ..selected, drop = FALSE]
}


#' @describeIn covariance_filter Default ranking function using polynomial GLM. Returns
#' the lower p value for each of the polynomial terms
#' @param x A numeric vector representing a single covariate
#' @param y A binary outcome vector (0/1)
#' @param weights Optional weights vector
#' @param ... Additional arguments (ignored)
#' @keywords internal
rank_poly_glm <- function(x, y, weights = NULL, ...) {
  fit <- glm.fit(
    x = cbind(1, poly(x, degree = 2, simple = TRUE)),
    y = y,
    family = binomial(),
    weights = weights
  )

  # Get p-values for linear and quadratic terms
  coef_summary <- summary.glm(fit)$coefficients

  # Return minimum p-value (most significant term)
  min(coef_summary[2:3, 4], na.rm = TRUE)
}


#' @describeIn covariance_filter Compute class-balanced weights for imbalanced binary
#' outcomes; returns a numeric vector
#' @param trans_result Binary outcome vector (0/1)
#' @param legacy Bool, use the legacy weighting?
#' @keywords internal
compute_balanced_weights <- function(trans_result, legacy = FALSE) {
  n_total <- length(trans_result)
  n_trans <- sum(trans_result == 1)
  n_non_trans <- sum(trans_result == 0)

  # Compute inverse frequency weights
  weights <- numeric(n_total)

  if (legacy) {
    # I found this weighting in evoland-plus-legacy, but the models wouldn't converge
    # https://github.com/ethzplus/evoland-plus-legacy/blob/main/R/lulcc.splitforcovselection.r
    # This is actually just setting the underrepresented class to the rounded imbalance ratio
    weights[trans_result == 0] <- 1
    weights[trans_result == 1] <- round(n_non_trans / n_trans)
    return(weights)
  }

  # This is the heuristic in scikit-learn, n_samples / (n_classes * np.bincount(y))
  # https://scikit-learn.org/stable/modules/generated/sklearn.utils.class_weight.compute_class_weight.html #nolint
  # This weighting maintains the exact imbalance ratio
  weights[trans_result == 1] <- n_total / (2 * n_trans)
  weights[trans_result == 0] <- n_total / (2 * n_non_trans)

  weights
}


#' @describeIn covariance_filter Select variables iteratively based on correlation
#' threshold; returns a character vector of selected variable names
#' @param cor_mat Absolute correlation matrix
#' @param corcut Correlation cutoff threshold
#' @keywords internal
select_by_correlation <- function(cor_mat, corcut) {
  var_names <- colnames(cor_mat)

  # Early return if all correlations are below threshold
  if (all(cor_mat[lower.tri(cor_mat)] < corcut)) {
    return(var_names)
  }

  selected <- character(0)
  remaining_idx <- seq_along(var_names)

  while (length(remaining_idx) > 0) {
    # Select the first remaining variable (highest ranked)
    current_var <- remaining_idx[1]
    selected <- c(selected, var_names[current_var])

    # Find variables with correlation <= corcut with current variable
    # (excluding the variable itself)
    keep_idx <- which(cor_mat[remaining_idx, current_var] <= corcut)
    remaining_idx <- remaining_idx[keep_idx]
  }

  selected
}
