# Compute per-sample class-balanced weights.
# Each sample gets a weight inversely proportional to its class frequency,
# so that every class contributes equally to the loss.
compute_balanced_weights <- function(y) {
  y <- as.factor(y)
  class_counts <- table(y)
  n_total <- length(y)
  n_classes <- length(class_counts)
  class_weights <- n_total / (n_classes * as.numeric(class_counts))
  names(class_weights) <- names(class_counts)
  as.numeric(class_weights[as.character(y)])
}

#' Guided Regularized Random Forest Learner
#'
#' An [mlr3::LearnerClassif] that fits a Guided Regularized Random Forest (GRRF)
#' for use in [mlr3filters::FilterImportance]-based feature selection.
#'
#' The learner implements a two-stage approach:
#' 1. An initial unregularized random forest estimates variable importance scores.
#' 2. Those scores are normalized and used to compute per-feature split-selection
#'    weights (`coefReg`), penalizing less informative predictors.
#' 3. A second "guided regularized" forest is trained with those weights, and its
#'    impurity importance scores are exposed via `$importance()`.
#'
#' Class-balanced sample weights are computed automatically from the target column,
#' so no external weight vector is required.
#'
#' @section Parameters:
#' - `gamma` (`numeric [0, 1]`, default `0.5`): Guidance coefficient.
#'   `0` = unguided regularized forest (equal penalty for all features);
#'   `1` = strongest guiding effect (most important features penalized least).
#' - `num.trees` (`integer >= 1`, default `500`): Number of trees in each forest.
#' - `max.depth` (`integer >= 0`, default `100`): Maximum tree depth (`0` = unlimited).
#'
#' @references
#' Deng, H., & Runger, G. (2013). Gene selection with guided regularized random forest.
#' *Pattern Recognition*, 46(12), 3483-3489. <https://doi.org/10.1016/j.patcog.2013.05.018>
#'
#' @examples
#' \dontrun{
#' library(mlr3)
#' library(mlr3filters)
#'
#' learner <- LearnerClassifGrrf$new()
#' learner$param_set$values <- list(gamma = 0.9, num.trees = 50L)
#'
#' filter <- mlr3filters::FilterImportance$new(learner = learner)
#' task <- mlr3::tsk("sonar")
#' filter$calculate(task)
#' as.data.table(filter)
#' }
#'
#' @export
LearnerClassifGrrf <- R6::R6Class(
  # nolint: object_name_linter.
  "LearnerClassifGrrf",
  inherit = mlr3::LearnerClassif,

  public = list(
    #' @description Initialise the learner with its parameter set.
    initialize = function() {
      ps <- paradox::ps(
        gamma = paradox::p_dbl(0, 1, default = 0.5, tags = "train"),
        num.trees = paradox::p_int(1L, default = 500L, tags = "train"),
        max.depth = paradox::p_int(0L, default = 100L, tags = "train")
      )
      super$initialize(
        id = "classif.grrf",
        packages = "ranger",
        feature_types = c("numeric", "integer", "logical", "factor", "ordered"),
        predict_types = "response",
        param_set = ps,
        properties = c("twoclass", "importance"),
        label = "Guided Regularized Random Forest",
        man = NA_character_
      )
    },

    #' @description Return impurity importance scores from the fitted GRRF.
    #' @return Named numeric vector of importance scores, sorted decreasingly.
    #'   All features present in the training task are included (unused features
    #'   receive a score of 0).
    importance = function() {
      stopifnot(
        "No model stored; call $train() first" = !is.null(self$model)
      )
      sort(self$model$variable.importance, decreasing = TRUE)
    }
  ),

  private = list(
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      gamma <- pv[["gamma"]] %||% 0.5
      # ranger does not accept a "gamma" argument; strip it before forwarding
      ranger_pv <- pv[setdiff(names(pv), "gamma")]

      data <- task$data()
      y <- data[[task$target_names]]
      x <- data[, task$feature_names, with = FALSE]
      weights <- compute_balanced_weights(y)

      # Stage 1: unregularized forest to obtain initial importance scores
      rf_initial <- do.call(
        ranger::ranger,
        c(
          list(x = x, y = y, importance = "impurity", case.weights = weights),
          ranger_pv
        )
      )

      # Normalize importance to [0, 1]; degenerate (all equal) -> uniform weight
      imp <- rf_initial$variable.importance
      imp_range <- max(imp) - min(imp)
      imp_normalized <- if (imp_range == 0) {
        rep(1, length(imp))
      } else {
        (imp - min(imp)) / imp_range
      }

      # Higher initial importance -> higher split-selection weight -> less penalty
      coef_reg <- (1 - gamma) + gamma * imp_normalized

      # Stage 2: guided regularized forest; importance() exposes these scores
      do.call(
        ranger::ranger,
        c(
          list(
            x = x,
            y = y,
            importance = "impurity",
            case.weights = weights,
            split.select.weights = coef_reg
          ),
          ranger_pv
        )
      )
    },

    .predict = function(task) {
      newdata <- task$data(cols = task$feature_names)
      list(response = predict(self$model, data = newdata)$predictions)
    }
  )
)
