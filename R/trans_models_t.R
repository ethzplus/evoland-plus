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
#'   - `id_run`: Foreign key to runs_t
#'   - `id_trans`: Foreign key to trans_meta_t
#'   - `model_family`: Model family (e.g., "rf", "glm", "bayesian")
#'   - `model_params`: Map of model (hyper) parameters
#'   - `goodness_of_fit`: Map of various measures of fit (e.g., ROC AUC, RMSE)
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

# Worker function for partial model fitting
# Not exported; used internally by fit_partial_models
fit_partial_model_worker <- function(
  item,
  db,
  fit_fun,
  gof_fun,
  na_value = NA,
  seed = NULL,
  ...
) {
  if (length(item[["id_preds"]]) == 0L) {
    warning(glue::glue(
      "No predictors for transition {item[['id_trans']]}, skipping"
    ))
    return(NULL)
  }

  tryCatch(
    {
      # Fetch ALL data into memory
      trans_pred_data_full <- db$trans_pred_data_v(
        id_trans = item[["id_trans"]],
        id_pred = item[["id_preds"]],
        na_value = na_value,
        # if seed is set, we want ordering for reproducible sampling
        ordered = !is.null(seed)
      )

      if (nrow(trans_pred_data_full) == 0L) {
        warning(glue::glue(
          "No data for transition {item[['id_trans']]}, skipping"
        ))
        return(NULL)
      }

      pred_cols <- grep("^id_pred_", names(trans_pred_data_full), value = TRUE)
      if (length(pred_cols) == 0L) {
        warning(glue::glue(
          "No predictor columns for transition {item[['id_trans']]}, skipping"
        ))
        return(NULL)
      }

      # Stratified sampling
      # Split by result (TRUE/FALSE)
      idx_true <- which(trans_pred_data_full$result)
      idx_false <- which(!trans_pred_data_full$result)

      # Sample from each group
      n_train_true <- ceiling(length(idx_true) * item[["sample_frac"]])
      n_train_false <- ceiling(length(idx_false) * item[["sample_frac"]])

      if (!is.null(seed)) {
        set.seed(seed)
      }

      train_idx <- c(
        sample(idx_true, n_train_true),
        sample(idx_false, n_train_false)
      )

      train_data <- trans_pred_data_full[train_idx]
      test_data <- trans_pred_data_full[!train_idx]

      # We modify the fit_fun by attaching the fit_fun_args to its formals. This allows
      # us to deparse it so as to store a string representation. When calling the
      # function object - possibly reconstructed using str2lang - only the data argument
      # should change (subsampled/partial or full)
      formals(fit_fun) <- c(formals(fit_fun), list(...))

      # Deparse to character string for storage
      fit_call_str <-
        deparse(fit_fun, width.cutoff = 500L) |>
        paste(collapse = "\n ")

      # actually evaluate the fit_fun
      model <- fit_fun(data = train_data)

      # Evaluate on test data
      goodness_of_fit <- gof_fun(model = model, test_data = test_data)

      # Extract model family
      model_family <- if (!is.null(attr(model, "family"))) {
        as.character(attr(model, "family"))
      } else if (inherits(model, "glm")) {
        paste0("glm_", model$family$family)
      } else {
        class(model)[1]
      }

      # Extract model params for subsetting
      model_params <- list(
        n_predictors = length(pred_cols),
        n_train = nrow(train_data),
        sample_frac = item[["sample_frac"]],
        ...
      )

      # Create result row
      data.table::data.table(
        id_trans = as.integer(item[["id_trans"]]),
        model_family = model_family,
        model_params = list(model_params),
        goodness_of_fit = list(goodness_of_fit),
        fit_call = fit_call_str,
        model_obj_part = list(qs2::qs_serialize(model)),
        model_obj_full = list(NULL)
      )
    },
    error = function(e) {
      warning(glue::glue(
        "Error fitting model for transition {item[['id_trans']]}: {e$message}"
      ))
      return(NULL)
    }
  )
}

# Worker function for full model fitting
# Not exported; used internally by fit_full_models
fit_full_model_worker <- function(item, db, na_value, envir, ...) {
  tryCatch(
    {
      # Fetch full data
      trans_pred_data_full <- db$trans_pred_data_v(
        id_trans = item[["id_trans"]],
        id_pred = item$id_preds,
        na_value = na_value
      )

      if (nrow(trans_pred_data_full) == 0L) {
        warning(glue::glue(
          "No data for transition {item[['id_trans']]}, skipping"
        ))
        return(NULL)
      }

      # Retrieve the fit call from best partial model (as character string)
      fit_call_str <- item$fit_call_str

      # Check that fit_call exists
      if (is.na(fit_call_str) || fit_call_str == "") {
        stop(glue::glue(
          "fit_call not found for transition {item[['id_trans']]}"
        ))
      }

      # Parse the character string to call object, reconstruct function, call
      fit_fun <- eval(str2lang(fit_call_str))
      model_full <- fit_fun(data = trans_pred_data_full)

      # Create result row - copy from partial model but update model_obj_full
      data.table::data.table(
        id_trans = as.integer(item[["id_trans"]]),
        model_family = item$model_family,
        model_params = list(item$model_params),
        goodness_of_fit = list(item$goodness_of_fit),
        fit_call = item$fit_call_str,
        model_obj_part = list(item$model_obj_part),
        model_obj_full = list(qs2::qs_serialize(model_full))
      )
    },
    error = function(e) {
      warning(glue::glue(
        "Error fitting full model for transition {item[['id_trans']]}: {e$message}"
      ))
      return(NULL)
    }
  )
}

fit_partial_models <- function(
  self,
  fit_fun,
  gof_fun,
  sample_frac = 0.7,
  seed = NULL,
  na_value = NA,
  cores = 1L,
  ...
) {
  viable_trans <- self$trans_meta_t[is_viable == TRUE]
  trans_preds <- self$trans_preds_t

  stopifnot(
    "No viable transitions found in trans_meta_t" = nrow(viable_trans) > 0L,
    "fit_fun must be a function" = is.function(fit_fun),
    "gof_fun must be a function" = is.function(gof_fun),
    "trans_preds_t is empty - run prune_trans_preds first" = nrow(trans_preds) > 0L,
    "sample_frac must be between 0 and 1" = sample_frac > 0 && sample_frac < 1
  )

  items <- lapply(seq_len(nrow(viable_trans)), function(i) {
    list(
      id_trans = viable_trans$id_trans[i],
      id_lulc_ant = viable_trans$id_lulc_anterior[i],
      id_lulc_post = viable_trans$id_lulc_posterior[i],
      id_preds = trans_preds$id_pred[trans_preds$id_trans == viable_trans$id_trans[i]],
      sample_frac = sample_frac
    )
  })

  message(glue::glue(
    "Fitting partial models for {length(items)} transitions..."
  ))

  results_list <- run_parallel_task(
    items = items,
    worker_fun = fit_partial_model_worker,
    db = self,
    cores = cores,
    fit_fun = fit_fun,
    gof_fun = gof_fun,
    na_value = na_value,
    seed = seed,
    ...
  )

  # Filter out NULLs
  results_list <- Filter(Negate(is.null), results_list)

  # Combine all results
  if (length(results_list) == 0L) {
    warning("No models fitted for any transition")
    return(invisible(NULL))
  }

  result <- data.table::rbindlist(results_list, fill = TRUE)

  as_trans_models_t(result)
}

fit_full_models <- function(
  self,
  partial_models,
  gof_criterion,
  maximize = TRUE,
  na_value = NA,
  envir = parent.frame(),
  cores = 1L
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
  # TODO should we also use this logic for retrieving the correct model for transition probability?
  if (maximize) {
    best_models <- partial_models_dt[, .SD[which.max(gof_value)], by = id_trans]
  } else {
    best_models <- partial_models_dt[, .SD[which.min(gof_value)], by = id_trans]
  }

  # Prepare items for parallel processing
  items <- lapply(seq_len(nrow(best_models)), function(i) {
    list(
      id_trans = best_models$id_trans[i],
      gof_value = best_models$gof_value[i],
      fit_call_str = best_models$fit_call[i],
      model_family = best_models$model_family[i],
      model_params = best_models$model_params[[i]],
      goodness_of_fit = best_models$goodness_of_fit[[i]],
      model_obj_part = best_models$model_obj_part[[i]],
      id_preds = trans_preds$id_pred[trans_preds$id_trans == best_models$id_trans[i]]
    )
  })

  message(glue::glue(
    "Fitting full models for {length(items)} transitions..."
  ))

  results_list <- run_parallel_task(
    items = items,
    worker_fun = fit_full_model_worker,
    db = self,
    cores = cores,
    na_value = na_value,
    envir = envir
  )

  # Filter out NULLs
  results_list <- Filter(Negate(is.null), results_list)

  # Combine all results
  if (length(results_list) == 0L) {
    warning("No full models fitted for any transition")
    return(invisible(NULL))
  }

  result <- data.table::rbindlist(results_list, fill = TRUE)

  as_trans_models_t(result)
}


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
