#' Create Transition Models Table
#'
#' Creates a trans_models_t table for storing transition model metadata and
#' serialized model objects. This function creates an empty table with proper
#' structure for storing fitted models.
#'
#' @name trans_models_t
#'
#' @param x A list or data.frame coercible to a data.table
#'
#' @return A data.table of class "trans_models_t" with columns:
#'   - `id_trans`: Foreign key to trans_meta_t
#'   - `model_family`: Model family (e.g., "rf", "glm", "bayesian")
#'   - `model_params`: Map of model (hyper) parameters
#'   - `goodness_of_fit`: Map of various measures of fit (e.g., ROC AUC, RMSE)
#'   - `sampled_coords`: data.table with id_coord/id_period pairs used for training
#'   - `fit_call`: Character string of the original fit function call for reproducibility
#'   - `model_obj_part`: BLOB of serialized model object for validation
#'   - `model_obj_full`: BLOB of serialized model object for extrapolation
#' @export
as_trans_models_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_trans = integer(0),
      model_family = character(0),
      model_params = list(),
      goodness_of_fit = list(),
      sampled_coords = list(),
      fit_call = character(0),
      model_obj_part = list(),
      model_obj_full = list()
    )
  }
  new_evoland_table(
    x,
    "trans_models_t",
    c("id_trans")
  )
}

#' @describeIn trans_models_t Fit partial models for each viable transition using stratified
#' sampling. Models are trained on a subsample and evaluated on held-out data.
#'
#' @param fit_fun Function to fit models. Should accept (data, result_col = "result", ...) and
#'   return a fitted model object
#' @param gof_fun Function to evaluate goodness of fit. Should accept (model, test_data, result_col,
#' ...) and return a named list of metrics @param sample_pct Percentage (0-100) for stratified
#' sampling. This percentage of each result group (TRUE/FALSE) will be used for training, the rest
#' for validation.
#' @param seed Random seed for reproducible sampling
#' @param na_value Passed to db$trans_pred_data_v - if not NA, replace all NA predictor values with
#' this value
#' @param ... Additional arguments passed to fit_fun
evoland_db$set(
  "public",
  "fit_partial_models",
  function(
    fit_fun,
    gof_fun,
    sample_pct = 70,
    seed = NULL,
    na_value = NA,
    ...
  ) {
    viable_trans <- self$trans_meta_t[is_viable == TRUE]
    trans_preds <- self$trans_preds_t

    stopifnot(
      "No viable transitions found in trans_meta_t" = nrow(viable_trans) > 0L,
      "fit_fun must be a function" = is.function(fit_fun),
      "gof_fun must be a function" = is.function(gof_fun),
      "trans_preds_t is empty - run prune_trans_preds first" = nrow(trans_preds) > 0L,
      "sample_pct must be between 0 and 100" = sample_pct > 0 && sample_pct < 100
    )

    # Set seed for reproducibility if provided
    if (!is.null(seed)) {
      set.seed(seed)
    }

    results_list <- list()

    # Iterate over transitions
    for (i in seq_len(nrow(viable_trans))) {
      id_trans <- viable_trans$id_trans[i]
      id_lulc_ant <- viable_trans$id_lulc_anterior[i]
      id_lulc_post <- viable_trans$id_lulc_posterior[i]
      id_preds <- trans_preds$id_pred[trans_preds$id_trans == id_trans]

      message(glue::glue(
        "Fitting partial model for transition {i}/{nrow(viable_trans)}: ",
        "id_trans={id_trans} ({id_lulc_ant} -> {id_lulc_post})"
      ))

      if (length(id_preds) == 0L) {
        warning(glue::glue(
          "No predictors for transition {id_trans}, skipping"
        ))
        next
      }

      tryCatch(
        {
          # Fetch ALL data into memory
          trans_pred_data_full <- self$trans_pred_data_v(
            id_trans,
            id_preds,
            na_value
          )

          if (nrow(trans_pred_data_full) == 0L) {
            warning(glue::glue(
              "No data for transition {id_trans}, skipping"
            ))
            next
          }

          pred_cols <- grep("^id_pred_", names(trans_pred_data_full), value = TRUE)
          if (length(pred_cols) == 0L) {
            warning(glue::glue(
              "No predictor columns for transition {id_trans}, skipping"
            ))
            next
          }

          # Stratified sampling
          # Split by result (TRUE/FALSE)
          idx_true <- which(trans_pred_data_full$result)
          idx_false <- which(!trans_pred_data_full$result)

          # Sample from each group
          n_train_true <- ceiling(length(idx_true) * sample_pct / 100)
          n_train_false <- ceiling(length(idx_false) * sample_pct / 100)

          train_idx_true <- sample(idx_true, n_train_true)
          train_idx_false <- sample(idx_false, n_train_false)
          train_idx <- c(train_idx_true, train_idx_false)

          # Test set is everything not in train
          test_idx <- setdiff(seq_len(nrow(trans_pred_data_full)), train_idx)

          train_data <- trans_pred_data_full[train_idx, ]
          test_data <- trans_pred_data_full[test_idx, ]

          # Record sampled coordinates for reproducibility
          sampled_coords <- train_data[, .(id_coord, id_period)]

          message(glue::glue(
            "  Training on {nrow(train_data)} observations ",
            "({n_train_true} TRUE, {n_train_false} FALSE)"
          ))
          message(glue::glue(
            "  Testing on {nrow(test_data)} observations"
          ))

          # Fit model on training data and capture the actual call
          model <- fit_fun(
            data = train_data,
            result_col = "result",
            ...
          )

          # Capture the fit function call for reproducibility
          # Store the call as a deparsed string (serializable)
          fit_call_obj <- call(
            deparse(substitute(fit_fun)),
            data = quote(data), # Placeholder to be replaced
            result_col = "result"
          )
          # Add ... arguments to the call
          dots <- list(...)
          if (length(dots) > 0) {
            for (arg_name in names(dots)) {
              fit_call_obj[[arg_name]] <- dots[[arg_name]]
            }
          }
          # Deparse to character string for serialization
          fit_call <- deparse(fit_call_obj, width.cutoff = 500L)
          fit_call <- paste(fit_call, collapse = " ")

          # Evaluate on test data
          goodness_of_fit <- gof_fun(
            model = model,
            test_data = test_data,
            result_col = "result",
            ...
          )

          # Extract model family
          model_family <- if (!is.null(attr(model, "family"))) {
            as.character(attr(model, "family"))
          } else if (inherits(model, "glm")) {
            paste0("glm_", model$family$family)
          } else {
            class(model)[1]
          }

          # Extract model params (store metadata only, no functions)
          model_params <- list(
            n_predictors = length(pred_cols),
            n_train = nrow(train_data),
            sample_pct = sample_pct
          )

          # Serialize partial model
          model_obj_part <- list(qs2::qs_serialize(model))

          message(glue::glue(
            "  Model fitted successfully. GOF: {paste(names(goodness_of_fit), ",
            "'=', round(unlist(goodness_of_fit), 3), collapse = ', ')}"
          ))

          # Create result row
          results_list[[length(results_list) + 1]] <- data.table::data.table(
            id_trans = as.integer(id_trans),
            model_family = model_family,
            model_params = list(model_params),
            goodness_of_fit = list(goodness_of_fit),
            sampled_coords = list(sampled_coords),
            fit_call = fit_call,
            model_obj_part = model_obj_part,
            model_obj_full = list(NULL)
          )
        },
        error = function(e) {
          warning(glue::glue(
            "Error fitting model for transition {id_trans}: {e$message}"
          ))
        }
      )
    }

    # Combine all results
    if (length(results_list) == 0L) {
      warning("No models fitted for any transition")
      return(invisible(NULL))
    }

    result <- data.table::rbindlist(results_list, fill = TRUE)

    as_trans_models_t(result)
  }
)

#' @describeIn trans_models_t Fit full models on complete data using the best partial model
#' configuration for each transition.
#'
#' @param partial_models A trans_models_t table with partial models (from fit_partial_models)
#' @param gof_criterion Which goodness-of-fit metric to use for model selection (e.g., "auc")
#' @param maximize Logical. If TRUE, higher values of gof_criterion are better. If FALSE, lower is better.
#' @param na_value Passed to db$trans_pred_data_v
#' @param envir Environment in which to evaluate fit_call. Defaults to parent frame.
evoland_db$set(
  "public",
  "fit_full_models",
  function(
    partial_models,
    gof_criterion,
    maximize = TRUE,
    na_value = NA,
    envir = parent.frame()
  ) {
    stopifnot(
      "partial_models must be a trans_models_t" = inherits(partial_models, "trans_models_t"),
      "partial_models is empty" = nrow(partial_models) > 0L,
      "gof_criterion must be a character string" = is.character(gof_criterion) &&
        length(gof_criterion) == 1L
    )

    # get "appears that attributes have been reassigned" warning without this
    partial_models_dt <- data.table::as.data.table(partial_models)

    trans_preds <- self$trans_preds_t

    # Extract GOF criterion values
    data.table::set(
      partial_models_dt,
      j = "gof_value",
      value = vapply(
        partial_models_dt[["goodness_of_fit"]],
        function(gof) {
          val <- gof[[gof_criterion]]
          if (is.null(val)) NA_real_ else as.numeric(val)
        },
        numeric(1)
      )
    )

    # Select best model per transition
    if (maximize) {
      best_models <- partial_models_dt[, .SD[which.max(gof_value)], by = id_trans]
    } else {
      best_models <- partial_models_dt[, .SD[which.min(gof_value)], by = id_trans]
    }

    results_list <- list()

    # Iterate over best models
    for (i in seq_len(nrow(best_models))) {
      id_trans <- best_models$id_trans[i]
      id_preds <- trans_preds$id_pred[trans_preds$id_trans == id_trans]

      message(glue::glue(
        "Fitting full model for transition {i}/{nrow(best_models)}: ",
        "id_trans={id_trans} (selected by {gof_criterion}={round(best_models$gof_value[i], 3)})"
      ))

      tryCatch(
        {
          # Fetch full data
          trans_pred_data_full <- self$trans_pred_data_v(id_trans, id_preds, na_value)

          if (nrow(trans_pred_data_full) == 0L) {
            warning(glue::glue(
              "No data for transition {id_trans}, skipping"
            ))
            next
          }

          message(glue::glue(
            "  Fitting on {nrow(trans_pred_data_full)} observations"
          ))

          # Retrieve the fit call from best partial model (as character string)
          fit_call_str <- best_models$fit_call[i]

          # Check that fit_call exists
          if (is.na(fit_call_str) || fit_call_str == "") {
            stop(glue::glue(
              "fit_call not found for transition {id_trans}"
            ))
          }

          # Parse the character string back to a call object
          fit_call <- str2lang(fit_call_str)

          # Replace the data argument with actual data (not a symbol)
          fit_call$data <- trans_pred_data_full

          # Evaluate the call in the provided environment
          # This requires the fit function to be available in that environment
          model_full <- eval(fit_call, envir = envir)

          # Serialize full model
          model_obj_full <- list(qs2::qs_serialize(model_full))

          message(glue::glue(
            "  Full model fitted and serialized successfully"
          ))

          # Create result row - copy from partial model but update model_obj_full
          results_list[[length(results_list) + 1]] <- data.table::data.table(
            id_trans = as.integer(id_trans),
            model_family = best_models$model_family[i],
            model_params = list(best_models$model_params[[i]]),
            goodness_of_fit = list(best_models$goodness_of_fit[[i]]),
            sampled_coords = list(best_models$sampled_coords[[i]]),
            fit_call = best_models$fit_call[i],
            model_obj_part = list(best_models$model_obj_part[[i]]),
            model_obj_full = model_obj_full
          )
        },
        error = function(e) {
          warning(glue::glue(
            "Error fitting full model for transition {id_trans}: {e$message}"
          ))
        }
      )
    }

    # Combine all results
    if (length(results_list) == 0L) {
      warning("No full models fitted for any transition")
      return(invisible(NULL))
    }

    result <- data.table::rbindlist(results_list, fill = TRUE)

    as_trans_models_t(result)
  }
)

#' @export
validate.trans_models_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_trans",
      "model_family",
      "model_params",
      "goodness_of_fit",
      "sampled_coords",
      "fit_call",
      "model_obj_part",
      "model_obj_full"
    )
  )

  # Skip soft checks if empty
  if (nrow(x) == 0L) {
    return(x)
  }

  stopifnot(
    is.integer(x[["id_trans"]]),
    is.character(x[["model_family"]]),
    is.list(x[["model_params"]]),
    is.list(x[["goodness_of_fit"]]),
    is.list(x[["sampled_coords"]]),
    is.character(x[["fit_call"]]),
    is.list(x[["model_obj_part"]]),
    is.list(x[["model_obj_full"]]),
    all(x[["id_trans"]] > 0),
    !any(x[["model_family"]] == "")
  )

  return(x)
}

#' @export
#' @describeIn trans_models_t Print a trans_models_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.trans_models_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_trans <- data.table::uniqueN(x[["id_trans"]])
    model_families <- unique(x[["model_family"]])
    n_with_part_models <- sum(!vapply(x[["model_obj_part"]], is.null, logical(1)))
    n_with_full_models <- sum(!vapply(x[["model_obj_full"]], is.null, logical(1)))

    cat(glue::glue(
      "Transition Models Table\n",
      "Total models: {nrow(x)}\n",
      "Transitions: {n_trans}\n",
      "Model families: {paste(model_families, collapse = ', ')}\n",
      "With partial models: {n_with_part_models}, With full models: {n_with_full_models}\n\n"
    ))
  } else {
    cat("Transition Models Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
