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
      id_run = integer(0),
      id_trans = integer(0),
      model_family = character(0),
      model_params = list(),
      goodness_of_fit = list(),
      fit_call = character(0),
      model_obj_part = list(),
      model_obj_full = list()
    )
  }
  as_parquet_db_t(
    x,
    "trans_models_t",
    key_cols = c("id_run", "id_trans", "fit_call"),
    map_cols = c("model_params", "goodness_of_fit"),
    partition_cols = "id_run"
  )
}

# Worker function for partial model fitting
# Not exported; used internally by fit_partial_models
fit_partial_model_worker <- function(
  item,
  db,
  fit_fun,
  gof_fun,
  seed = NULL,
  sample_frac = 0.7,
  ...
) {
  id_run_orig <- db$get_active_run()
  on.exit(db$set_active_run(id_run_orig), add = TRUE)
  db$set_active_run(item[["id_run"]])

  # We modify the fit_fun by attaching the fit_fun_args to its formals. This allows
  # us to deparse it so as to store a string representation. When calling the
  # function object - possibly reconstructed using str2lang - only the data argument
  # should change (subsampled/partial or full)
  formals(fit_fun) <- c(formals(fit_fun), list(...))

  # Deparse to character string for storage
  fit_call_str <-
    deparse(fit_fun, width.cutoff = 500L) |>
    paste(collapse = "\n ")

  tryCatch(
    {
      # Fetch ALL data into memory
      trans_pred_data_full <- db$trans_pred_data_v(
        id_trans = item[["id_trans"]],
        id_pred = item[["id_pred"]][[1L]],

        # if seed is set, we want ordering for reproducible sampling
        ordered = !is.null(seed)
      )

      if (nrow(trans_pred_data_full) == 0L) {
        stop(glue::glue(
          "No data for transition {item[['id_trans']]}, skipping"
        ))
      }

      pred_cols <- grep("^id_pred_", names(trans_pred_data_full), value = TRUE)
      if (length(pred_cols) == 0L) {
        stop(glue::glue(
          "No predictor columns for transition {item[['id_trans']]}, skipping"
        ))
      }

      # Stratified sampling
      # Split by result (TRUE/FALSE)
      idx_true <- which(trans_pred_data_full$result)
      idx_false <- which(!trans_pred_data_full$result)

      # Sample from each group
      n_train_true <- ceiling(length(idx_true) * sample_frac)
      n_train_false <- ceiling(length(idx_false) * sample_frac)

      if (!is.null(seed)) {
        set.seed(seed)
      }

      train_idx <- c(
        sample(idx_true, n_train_true),
        sample(idx_false, n_train_false)
      )

      train_data <- trans_pred_data_full[train_idx]
      test_data <- trans_pred_data_full[!train_idx]

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
        sample_frac = sample_frac,
        ...
      )

      # Create result row
      data.table::data.table(
        id_run = item[["id_run"]],
        id_trans = item[["id_trans"]],
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
      return(data.table::data.table(
        id_run = item[["id_run"]],
        id_trans = item[["id_trans"]],
        model_family = "error",
        model_params = list(NULL),
        goodness_of_fit = list(list(failed = TRUE, message = e$message)),
        fit_call = fit_call_str,
        model_obj_part = list(NULL),
        model_obj_full = list(NULL)
      ))
    }
  )
}

# Worker function for full model fitting
# Not exported; used internally by fit_full_models
fit_full_model_worker <- function(item, db, ...) {
  tryCatch(
    {
      # Fetch full data
      trans_pred_data_full <- db$trans_pred_data_v(
        id_trans = item[["id_trans"]],
        id_pred = item[["id_pred"]][[1L]],
      )

      if (nrow(trans_pred_data_full) == 0L) {
        stop(glue::glue(
          "No data for transition {item[['id_trans']]}, skipping"
        ))
      }

      # Retrieve the fit call from best partial model (as character string)
      fit_call_str <- item[["fit_call"]]

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
      list(
        id_run = item[["id_run"]],
        id_trans = item[["id_trans"]],
        model_obj_full = list(qs2::qs_serialize(model_full))
      )
    },
    error = function(e) {
      warning(glue::glue(
        "Error fitting full model for transition {item[['id_trans']]}: {e$message}"
      ))
      return(list(
        id_run = item[["id_run"]],
        id_trans = item[["id_trans"]],
        model_obj_full = list(NULL)
      ))
    }
  )
}


#' @describeIn trans_models_t Fit partial models for each viable transition and store
#' results in a trans_models_t table.
#' @param self, [evoland_db] instance to query for transitions and predictor data
#' @param fit_fun Function that takes a data.frame with predictors and result columns
#' and returns a fitted model object. The data argument is passed as the first argument
#' to the function, and additional arguments can be passed via ...
#' @param gof_fun Function that takes a fitted model object and a test data.frame and
#' returns a list of goodness-of-fit metrics. The model argument is passed as the first
#' argument and the test_data argument is passed as the second argument.
#' @param sample_frac Numeric between 0 and 1 indicating
#' the fraction of data to use for training the partial models. The rest is used for
#' testing and calculating goodness-of-fit metrics. Default is 0.7 (70% training, 30%
#' testing).
#' @param seed Optional integer seed for reproducible subsampling.
#' @param cluster An optional cluster object created by [parallel::makeCluster()] or
#' [mirai::make_cluster()].
fit_partial_models <- function(
  self,
  fit_fun,
  gof_fun,
  sample_frac = 0.7,
  seed = NULL,
  cluster = NULL,
  ...
) {
  trans_preds_nested <-
    data.table::as.data.table(self$trans_preds_t)[,
      .(id_pred = list(id_pred)),
      by = .(id_run, id_trans)
    ]

  viable_trans <-
    self$trans_meta_t[
      is_viable == TRUE,
      .(id_trans, id_lulc_anterior, id_lulc_posterior)
    ][
      trans_preds_nested,
      on = "id_trans"
    ]

  stopifnot(
    "No viable transitions" = nrow(viable_trans) > 0L,
    "fit_fun must be a function" = is.function(fit_fun),
    "gof_fun must be a function" = is.function(gof_fun),
    "sample_frac must be between 0 and 1" = sample_frac > 0 && sample_frac < 1
  )

  n_trans <- data.table::uniqueN(viable_trans, by = c("id_run", "id_trans"))
  message(glue::glue(
    "Fitting partial models for {n_trans} transitions..."
  ))

  viable_trans |>
    split(by = c("id_run", "id_trans")) |>
    run_parallel_evoland(
      items = _,
      worker_fun = fit_partial_model_worker,
      parent_db = self,
      cluster = cluster,
      fit_fun = fit_fun,
      gof_fun = gof_fun,
      seed = seed,
      sample_frac = sample_frac,
      ...
    ) |>
    data.table::rbindlist() |>
    as_trans_models_t()
}

#' @describeIn trans_models_t Fit full models for each transition based on the best
#' partial model according to a specified goodness-of-fit criterion.
#' @param self, [evoland_db] instance to query for transitions and predictor data
#' @param partial_models A trans_models_t table containing the fitted partial models and
#' their goodness-of-fit metrics.
#' @param gof_criterion Character string specifying which goodness-of-fit metric to use for
#' selecting the best partial model for each transition (e.g., "roc_auc", "rmse").
#' @param maximize Logical indicating whether to select the model with the maximum
#' (TRUE) or minimum (FALSE) value of the specified goodness-of-fit criterion. Default
#' is TRUE.
#' @param cluster An optional cluster object created by [parallel::makeCluster()] or
#' [mirai::make_cluster()].
fit_full_models <- function(
  self,
  partial_models,
  gof_criterion,
  maximize = TRUE,
  cluster = NULL
) {
  stopifnot(
    "partial_models must be a trans_models_t" = inherits(partial_models, "trans_models_t"),
    "partial_models is empty" = nrow(partial_models) > 0L,
    "gof_criterion must be a character string" = is.character(gof_criterion) &&
      length(gof_criterion) == 1L
  )

  trans_preds_nested <-
    data.table::as.data.table(self$trans_preds_t)[,
      .(id_pred = list(id_pred)),
      by = .(id_run, id_trans)
    ]

  partial_models_dt <-
    merge(
      data.table::as.data.table(partial_models),
      trans_preds_nested,
      by = c("id_run", "id_trans")
    )

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

  # Select best model per transition based on GOF criterion
  if (maximize) {
    best_models <- partial_models_dt[, .SD[which.max(gof_value)], by = .(id_run, id_trans)]
  } else {
    best_models <- partial_models_dt[, .SD[which.min(gof_value)], by = .(id_run, id_trans)]
  }

  message(glue::glue(
    "Fitting full models for {nrow(best_models)} transitions..."
  ))

  results_list <-
    best_models[, .(id_run, id_trans, id_pred, fit_call)] |>
    split(by = c("id_run", "id_trans")) |>
    run_parallel_evoland(
      items = _,
      worker_fun = fit_full_model_worker,
      parent_db = self,
      cluster = cluster,
    ) |>
    data.table::rbindlist()

  merge(
    best_models[, -c("model_obj_full", "gof_value", "id_pred")],
    results_list,
    by = c("id_run", "id_trans")
  ) |>
    as_trans_models_t()
}


#' @export
validate.trans_models_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_run",
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
    is.integer(x[["id_run"]]),
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
#' @describeIn trans_models_t Print a trans_models_t object as yaml-style list
print.trans_models_t <- function(x) {
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
  print_rowwise_yaml(x)
  invisible(x)
}
