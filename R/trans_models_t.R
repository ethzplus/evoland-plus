#' Create Transition Models Table
#'
#' Creates a trans_models_t table for storing transition model metadata and
#' serialized model objects. This function creates an empty table with proper
#' structure for storing fitted models via the mlr3 interface.
#'
#' @name trans_models_t
#'
#' @param x A list or data.frame coercible to a data.table
#'
#' @return A data.table of class "trans_models_t" with columns:
#'   - `id_run`: Foreign key to runs_t
#'   - `id_trans`: Foreign key to trans_meta_t
#'   - `learner_id`: mlr3 learner key, e.g. `"classif.ranger"`
#'   - `learner_params`: MAP of atomic scalar learner hyperparameters for
#'     querying; complete hyperparameters are captured by `learner_spec`
#'   - `learner_spec`: BLOB of serialized untrained mlr3 `Learner`; for
#'     AutoTuners, this is the optimal inner learner after tuning
#'   - `crossval_score`: MAP of cross-validation performance scores
#'     (from `prediction$score(measures)`)
#'   - `crossval_predictions`: BLOB of serialized mlr3 `PredictionClassif`
#'     on the held-out test split
#'   - `learner_full`: BLOB of serialized trained mlr3 `Learner` fitted on
#'     the full dataset, used for extrapolation
#' @export
as_trans_models_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_run = integer(0),
      id_trans = integer(0),
      learner_id = character(0),
      learner_params = list(),
      learner_spec = list(),
      crossval_score = list(),
      crossval_predictions = list(),
      learner_full = list()
    )
  }

  data.table::setDT(x) |>
    cast_dt_col("id_run", "int") |>
    cast_dt_col("id_trans", "int")

  as_parquet_db_t(
    x,
    "trans_models_t",
    key_cols = c("id_run", "id_trans", "learner_id"),
    map_cols = c("learner_params", "crossval_score"),
    partition_cols = "id_run"
  )
}

# Worker function for partial model fitting
# Not exported; used internally by fit_partial_models
fit_partial_model_worker <- function(
  item,
  db,
  learner,
  measures,
  seed = NULL,
  sample_frac = 0.7
) {
  if (!requireNamespace("mlr3", quietly = TRUE)) {
    stop("Package 'mlr3' is required. Install with: install.packages('mlr3')")
  }

  id_run_orig <- db$id_run
  on.exit(db$id_run <- id_run_orig, add = TRUE)
  db$id_run <- item[["id_run"]]

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

      # Stratified sampling by did_transition
      idx_true <- which(trans_pred_data_full[["did_transition"]])
      idx_false <- which(!trans_pred_data_full[["did_transition"]])

      n_train_true <- ceiling(length(idx_true) * sample_frac)
      n_train_false <- ceiling(length(idx_false) * sample_frac)

      if (!is.null(seed)) {
        set.seed(seed)
      }

      train_idx <- c(
        sample(idx_true, n_train_true),
        sample(idx_false, n_train_false)
      )

      # Subset to task columns (did_transition + predictors) and coerce target
      task_cols <- c("did_transition", pred_cols)
      train_data <- trans_pred_data_full[train_idx, .SD, .SDcols = task_cols]
      test_data <- trans_pred_data_full[-train_idx, .SD, .SDcols = task_cols]

      train_data[, did_transition := factor(did_transition, levels = c("FALSE", "TRUE"))]
      test_data[, did_transition := factor(did_transition, levels = c("FALSE", "TRUE"))]

      # Build mlr3 task and train a fresh clone of the learner
      train_task <- mlr3::as_task_classif(
        train_data,
        target = "did_transition",
        positive = "TRUE"
      )
      trained_learner <- learner$clone(deep = TRUE)
      trained_learner$train(train_task)

      # Predict on test data; test_data includes did_transition as truth
      prediction <- trained_learner$predict_newdata(test_data)

      # Score with supplied measures
      scores <- as.list(prediction$score(measures))

      # For AutoTuner: extract optimal inner learner; otherwise use trained learner
      extract_from <-
        if (inherits(trained_learner, "AutoTuner") && !is.null(trained_learner$learner$model)) {
          trained_learner$learner
        } else {
          trained_learner
        }

      learner_id_val <- extract_from$id
      learner_params_val <- Filter(
        function(v) is.atomic(v) && length(v) == 1L,
        extract_from$param_set$values
      )
      learner_params_val <- if (length(learner_params_val) == 0L) NULL else learner_params_val
      learner_spec_blob <- qs2::qs_serialize(extract_from$clone(deep = TRUE)$reset())

      data.table::data.table(
        id_run = item[["id_run"]],
        id_trans = item[["id_trans"]],
        learner_id = learner_id_val,
        learner_params = list(learner_params_val),
        learner_spec = list(learner_spec_blob),
        crossval_score = list(scores),
        crossval_predictions = list(qs2::qs_serialize(prediction)),
        learner_full = list(NULL)
      )
    },
    error = function(e) {
      warning(glue::glue(
        "Error fitting model for transition {item[['id_trans']]}: {e$message}"
      ))
      data.table::data.table(
        id_run = item[["id_run"]],
        id_trans = item[["id_trans"]],
        learner_id = "error",
        learner_params = list(NULL),
        learner_spec = list(NULL),
        crossval_score = list(NULL),
        crossval_predictions = list(NULL),
        learner_full = list(NULL)
      )
    }
  )
}

# Worker function for full model fitting
# Not exported; used internally by fit_full_models
fit_full_model_worker <- function(item, db, ...) {
  if (!requireNamespace("mlr3", quietly = TRUE)) {
    stop("Package 'mlr3' is required. Install with: install.packages('mlr3')")
  }

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

      pred_cols <- grep("^id_pred_", names(trans_pred_data_full), value = TRUE)
      task_cols <- c("did_transition", pred_cols)
      task_data <- trans_pred_data_full[, .SD, .SDcols = task_cols]
      task_data[, did_transition := factor(did_transition, levels = c("FALSE", "TRUE"))]

      full_task <- mlr3::as_task_classif(
        task_data,
        target = "did_transition",
        positive = "TRUE"
      )

      # Reconstruct learner: try learner_spec first, fall back to do.call(lrn, ...)
      learner_spec_raw <- item[["learner_spec"]][[1L]]
      learner_id_val <- item[["learner_id"]]
      learner_params_val <- item[["learner_params"]][[1L]]

      trained_learner <- tryCatch(
        qs2::qs_deserialize(learner_spec_raw),
        error = function(e) {
          fallback <- do.call(mlr3::lrn, c(list(learner_id_val), as.list(learner_params_val)))
          warning(glue::glue(
            "learner_spec deserialization failed for {learner_id_val}: {e$message}; ",
            "falling back to reconstructed learner: {paste(fallback$format(), collapse = ' ')}"
          ))
          fallback
        }
      )

      trained_learner$train(full_task)

      list(
        id_run = item[["id_run"]],
        id_trans = item[["id_trans"]],
        learner_id = learner_id_val,
        learner_full = list(qs2::qs_serialize(trained_learner))
      )
    },
    error = function(e) {
      warning(glue::glue(
        "Error fitting full model for transition {item[['id_trans']]}: {e$message}"
      ))
      list(
        id_run = item[["id_run"]],
        id_trans = item[["id_trans"]],
        learner_id = item[["learner_id"]],
        learner_full = list(NULL)
      )
    }
  )
}


#' @describeIn trans_models_t Fit partial (cross-validation) models for each viable
#' transition and store results in a trans_models_t table.
#' @param self [evoland_db] instance to query for transitions and predictor data
#' @param learner An mlr3 `Learner` or `AutoTuner` object. A deep clone is trained
#'   for each transition; the original object is not modified. For `AutoTuner`,
#'   the optimal inner learner is extracted after tuning.
#' @param measures Either a character vector of mlr3 measure IDs
#'   (e.g. `c("classif.auc", "classif.acc")`) or a list of instantiated mlr3
#'   `Measure` objects (e.g. `list(mlr3::msr("classif.auc"))`). Character IDs are
#'   converted via `mlr3::msrs()` internally. Results are written to `crossval_score`.
#' @param sample_frac Numeric between 0 and 1 indicating
#' the fraction of data to use for training the partial models. The rest is used for
#' testing and calculating goodness-of-fit metrics. Default is 0.7 (70% training, 30%
#' testing).
#' @param seed Optional integer seed for reproducible subsampling.
#' @param cluster An optional cluster object created by [parallel::makeCluster()] or
#' [mirai::make_cluster()].
#' @return A [trans_models_t] table with one row per viable transition, containing
#'   the learner identity, serialized spec, cross-validation scores (`crossval_score`),
#'   and serialized held-out predictions (`crossval_predictions`).
fit_partial_models <- function(
  self,
  learner,
  measures,
  sample_frac = 0.7,
  seed = NULL,
  cluster = NULL
) {
  # Accept either a character vector of measure IDs or a list of Measure objects
  if (is.character(measures)) {
    measures <- mlr3::msrs(measures)
  }

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
    "learner must be an mlr3 Learner or AutoTuner" = inherits(learner, "Learner"),
    "measures must be a non-empty character vector or list of Measure objects" = (
      (is.character(measures) || (is.list(measures) && all(vapply(measures, inherits, logical(1), "Measure")))) &&
        length(measures) > 0L
    ),
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
      learner = learner,
      measures = measures,
      seed = seed,
      sample_frac = sample_frac
    ) |>
    data.table::rbindlist() |>
    as_trans_models_t()
}

#' @describeIn trans_models_t Fit full models for each transition based on the best
#' partial model according to a specified cross-validation criterion.
#' @param self [evoland_db] instance to query for transitions and predictor data
#' @param learner An mlr3 `Learner` or `AutoTuner` object; kept for API consistency and
#'   used as a last-resort fallback if both `learner_spec` deserialization and
#'   `do.call(mlr3::lrn, ...)` reconstruction fail.
#' @param measures Either a character vector of mlr3 measure IDs or a list of `Measure`
#'   objects; kept for API consistency.
#' @param gof_criterion Character string specifying which cross-validation score to use
#'   for selecting the best partial model per transition (must match a key in
#'   `crossval_score`, e.g. `"classif.auc"`).
#' @param gof_maximize Logical; select the model with the maximum (`TRUE`) or minimum
#'   (`FALSE`) value of `gof_criterion`. Default is `TRUE`.
#' @param cluster An optional cluster object created by [parallel::makeCluster()] or
#' [mirai::make_cluster()].
#' @return A [trans_models_t] table with one row per transition, containing the columns
#'   from the best partial model plus `learner_full` with the serialized fully-trained
#'   learner.
fit_full_models <- function(
  self,
  learner,
  measures,
  gof_criterion,
  gof_maximize,
  cluster = NULL
) {
  stopifnot(
    "gof_criterion must be a character string" = is.character(gof_criterion) &&
      length(gof_criterion) == 1L,
    "gof_maximize must be set to TRUE or FALSE" = isTRUE(gof_maximize) || isFALSE(gof_maximize),
    "trans_models_t is missing" = file.exists(self$get_table_path("trans_models_t"))
  )

  # Get the best partial model per transition (scalar columns only; MAP/BLOB via fetch below)
  best_model_ids <- self$get_query(glue::glue(
    r"[
    with preds_nested as (
      select
        id_run,
        id_trans,
        list(id_pred) as id_pred
      from
        {self$get_read_expr("trans_preds_t")}
      group by
        id_run, id_trans
    )
    select
      tm.id_run,
      tm.id_trans,
      tm.learner_id,
      pn.id_pred,
    from
      {self$get_read_expr("trans_models_t")} tm,
      preds_nested pn
    where
      pn.id_run = tm.id_run
      and pn.id_trans = tm.id_trans
    qualify row_number() over (
        partition by tm.id_run, tm.id_trans
        order by tm.crossval_score['{gof_criterion}'] {ifelse(gof_maximize, "desc", "asc")}
    ) = 1;
    ]"
  ))

  # Fetch learner_spec (BLOB) and learner_params (MAP) for the best rows via fetch()
  # so that MAP columns are properly deserialized to named lists.
  learner_id_csv <- paste0("'", best_model_ids$learner_id, "'", collapse = ", ")
  best_specs <- self$fetch(
    "trans_models_t",
    cols = c("id_run", "id_trans", "learner_id", "learner_spec", "learner_params"),
    where = glue::glue(
      "id_run in ({toString(best_model_ids$id_run)}) and ",
      "id_trans in ({toString(best_model_ids$id_trans)}) and ",
      "learner_id in ({learner_id_csv})"
    )
  )

  # Join to add id_pred and build complete item list for workers
  best_models <- best_model_ids[best_specs, on = c("id_run", "id_trans", "learner_id")]

  message(glue::glue(
    "Fitting full models for {nrow(best_models)} transitions..."
  ))

  full_models <-
    best_models |>
    split(by = c("id_run", "id_trans")) |>
    run_parallel_evoland(
      items = _,
      worker_fun = fit_full_model_worker,
      parent_db = self,
      cluster = cluster,
    ) |>
    data.table::rbindlist()

  # Fetch remaining columns from the best partial models and join
  partial_models <- self$fetch(
    "trans_models_t",
    cols = c(
      "id_run",
      "id_trans",
      "learner_id",
      "learner_params",
      "learner_spec",
      "crossval_score",
      "crossval_predictions"
    ),
    where = glue::glue(
      "id_run in ({toString(full_models$id_run)}) and ",
      "id_trans in ({toString(full_models$id_trans)}) and ",
      "learner_id in ({paste0(\"'\", full_models$learner_id, \"'\", collapse = \", \")})"
    )
  )

  full_models[
    partial_models,
    on = c("id_run", "id_trans", "learner_id"),
    `:=`(
      learner_params = i.learner_params,
      learner_spec = i.learner_spec,
      crossval_score = i.crossval_score,
      crossval_predictions = i.crossval_predictions
    )
  ] |>
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
      "learner_id",
      "learner_params",
      "learner_spec",
      "crossval_score",
      "crossval_predictions",
      "learner_full"
    )
  )

  # Skip soft checks if empty
  if (nrow(x) == 0L) {
    return(x)
  }

  stopifnot(
    is.integer(x[["id_run"]]),
    is.integer(x[["id_trans"]]),
    is.character(x[["learner_id"]]),
    is.list(x[["learner_params"]]),
    is.list(x[["learner_spec"]]),
    is.list(x[["crossval_score"]]),
    is.list(x[["crossval_predictions"]]),
    is.list(x[["learner_full"]]),
    all(x[["id_trans"]] > 0),
    !any(x[["learner_id"]] == "")
  )

  return(x)
}

#' @export
#' @describeIn trans_models_t Print a trans_models_t object as yaml-style list
print.trans_models_t <- function(x) {
  if (nrow(x) > 0) {
    n_trans <- data.table::uniqueN(x[["id_trans"]])
    learner_ids <- unique(x[["learner_id"]])
    n_with_crossval <- sum(!vapply(x[["crossval_predictions"]], is.null, logical(1)))
    n_with_full <- sum(!vapply(x[["learner_full"]], is.null, logical(1)))

    cat(glue::glue(
      "Transition Models Table\n",
      "Total models: {nrow(x)}\n",
      "Transitions: {n_trans}\n",
      "Learners: {paste(learner_ids, collapse = ', ')}\n",
      "With cross-val predictions: {n_with_crossval}, With full models: {n_with_full}\n\n"
    ))
  } else {
    cat("Transition Models Table (empty)\n")
  }
  print_rowwise_yaml(x)
  invisible(x)
}

#' @describeIn trans_models_t Deserialize cross-validation predictions and return
#' plots via `mlr3viz::autoplot()`. Requires the `mlr3viz` package.
#' @param self [evoland_db] instance
#' @param id_run Optional integer; filter by run ID.
#' @param id_trans Optional integer; filter by transition ID.
get_crossval_plots <- function(self, id_run = NULL, id_trans = NULL) {
  if (!requireNamespace("mlr3viz", quietly = TRUE)) {
    stop("Package 'mlr3viz' is required. Install with: install.packages('mlr3viz')")
  }

  where_clauses <- c()
  if (!is.null(id_run)) {
    where_clauses <- c(where_clauses, glue::glue("id_run = {id_run}"))
  }
  if (!is.null(id_trans)) {
    where_clauses <- c(where_clauses, glue::glue("id_trans = {id_trans}"))
  }
  where <- if (length(where_clauses) > 0L) paste(where_clauses, collapse = " and ") else NULL

  models <- self$fetch(
    "trans_models_t",
    cols = c("id_run", "id_trans", "learner_id", "crossval_predictions"),
    where = where
  )

  plots <- lapply(seq_len(nrow(models)), function(i) {
    pred_blob <- models$crossval_predictions[[i]]
    if (is.null(pred_blob)) {
      return(NULL)
    }
    prediction <- qs2::qs_deserialize(pred_blob)
    mlr3viz::autoplot(prediction)
  })

  names(plots) <- paste0(
    "id_run=", models$id_run,
    "_id_trans=", models$id_trans,
    "_", models$learner_id
  )

  plots
}
