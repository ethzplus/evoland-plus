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
#'   - `learner_id`: mlr3 twoclass [LearnerClassif](https://mlr3.mlr-org.com/reference/LearnerClassif.html)
#'     key, e.g. `"classif.ranger"`
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
      )[, -c("id_coord", "id_period_anterior")]

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

      # Split
      train_data <- trans_pred_data_full[train_idx]
      test_data <- trans_pred_data_full[-train_idx]

      # Coerce target; mlr3 uses factors internally also for twoclass classification
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
        learner_params = list(list()),
        learner_spec = list(NULL),
        crossval_score = list(list()),
        crossval_predictions = list(NULL),
        learner_full = list(NULL)
      )
    }
  )
}

# Worker function for full model fitting
# Not exported; used internally by fit_full_models.
# Operates in two modes depending on whether `learner` is NULL:
#  - direct mode (learner != NULL): train the passed learner clone on full data
#  - score-select mode (learner == NULL): reconstruct from item$learner_spec and retrain
fit_full_model_worker <- function(item, db, learner = NULL, ...) {
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

      if (!is.null(learner)) {
        # Direct mode: use the passed learner directly
        trained_learner <- learner$clone(deep = TRUE)
        learner_id_val <- trained_learner$id
        learner_params_val <- Filter(
          function(v) is.atomic(v) && length(v) == 1L,
          trained_learner$param_set$values
        )
        learner_params_val <- if (length(learner_params_val) == 0L) NULL else learner_params_val
        learner_spec_blob <- qs2::qs_serialize(trained_learner$clone(deep = TRUE)$reset())
        crossval_score_val <- list(list())
        crossval_predictions_val <- list(NULL)
      } else {
        # Score-select mode: reconstruct from learner_spec; fall back to do.call
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
        learner_spec_blob <- learner_spec_raw
        # Pass through cross-validation results from the partial model
        crossval_score_val <- item[["crossval_score"]]
        crossval_predictions_val <- item[["crossval_predictions"]]
      }

      trained_learner$train(full_task)

      list(
        id_run = item[["id_run"]],
        id_trans = item[["id_trans"]],
        learner_id = learner_id_val,
        learner_params = list(learner_params_val),
        learner_spec = list(learner_spec_blob),
        crossval_score = crossval_score_val,
        crossval_predictions = crossval_predictions_val,
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
        learner_id = if (!is.null(learner)) learner$id else item[["learner_id"]],
        learner_params = list(list()),
        learner_spec = list(NULL),
        crossval_score = list(list()),
        crossval_predictions = list(NULL),
        learner_full = list(NULL)
      )
    }
  )
}


#' @describeIn trans_models_t Fit partial (cross-validation) models for each viable
#' transition; returns a [trans_models_t] object with one row per viable transition,
#' containing the learner identity, serialized spec, cross-validation scores
#' (`crossval_score`), and serialized held-out predictions (`crossval_predictions`).
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
    "measures must be a non-empty character vector or list of Measure objects" = ((is.character(
      measures
    ) ||
      (is.list(measures) && all(vapply(measures, inherits, logical(1), "Measure")))) &&
      length(measures) > 0L),
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

#' @describeIn trans_models_t Fit full models (trained on the complete dataset) for each
#' viable transition and return a [trans_models_t] object with `learner_full` populated.
#' Two mutually exclusive modes are supported:
#' - **Direct-learner mode** (`learner` provided, `select_score` omitted): a fresh clone of
#'   `learner` is trained on the full data for each transition. `crossval_score` and
#'   `crossval_predictions` will be `NULL` in the result. Does not require a prior
#'   call to [fit_partial_models()].
#' - **Score-select mode** (`select_score` provided, `learner` omitted): selects the best
#'   partial model per transition by `select_score`, reconstructs its learner from
#'   `learner_spec`, and retrains on the full data. Requires [fit_partial_models()] to
#'   have been run first.
#' @param self [evoland_db] instance to query for transitions and predictor data
#' @param learner An mlr3 `Learner` or `AutoTuner` object for direct-learner mode.
#'   Must be `NULL` when `select_score` is provided.
#' @param select_score Character string; mlr3 measure ID (e.g. `"classif.auc"`) used to
#'   rank partial models in score-select mode. Must be `NULL` when `learner` is provided.
#' @param select_maximize Logical; if `TRUE` (default) the model with the highest
#'   `select_score` is selected; if `FALSE`, the lowest. Only used in score-select mode.
#' @param cluster An optional cluster object created by [parallel::makeCluster()] or
#' [mirai::make_cluster()].
fit_full_models <- function(
  self,
  learner = NULL,
  select_score = NULL,
  select_maximize = TRUE,
  cluster = NULL
) {
  has_learner <- !is.null(learner)
  has_score <- !is.null(select_score)

  stopifnot(
    "Provide exactly one of 'learner' or 'select_score'" = xor(has_learner, has_score)
  )

  if (has_learner) {
    stopifnot(
      "learner must be an mlr3 Learner or AutoTuner" = inherits(learner, "Learner")
    )

    # Direct mode: get viable transitions with their predictor lists
    trans_preds_nested <-
      data.table::as.data.table(self$trans_preds_t)[,
        .(id_pred = list(id_pred)),
        by = .(id_run, id_trans)
      ]

    viable_trans <-
      self$trans_meta_t[
        is_viable == TRUE,
        .(id_trans)
      ][
        trans_preds_nested,
        on = "id_trans"
      ]

    message(glue::glue(
      "Fitting full models for {nrow(viable_trans)} transitions..."
    ))

    viable_trans |>
      split(by = c("id_run", "id_trans")) |>
      run_parallel_evoland(
        items = _,
        worker_fun = fit_full_model_worker,
        parent_db = self,
        cluster = cluster,
        learner = learner,
      ) |>
      data.table::rbindlist() |>
      as_trans_models_t()
  } else {
    # Score-select mode
    stopifnot(
      "select_score must be a character string" = is.character(select_score) &&
        length(select_score) == 1L,
      "select_maximize must be TRUE or FALSE" = isTRUE(select_maximize) || isFALSE(select_maximize),
      "trans_models_t is missing" = file.exists(self$get_table_path("trans_models_t"))
    )

    # Identify the best partial model per transition (using QUALIFY window function)
    # and get the predictor ID lists from trans_preds_t in the same query
    best_models <-
      self$get_query(glue::glue(
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
        tm.learner_spec,
        tm.learner_params,
        tm.crossval_score,
        tm.crossval_predictions,
      from
        {self$get_read_expr("trans_models_t")} tm,
        preds_nested pn
      where
        pn.id_run = tm.id_run
        and pn.id_trans = tm.id_trans
      qualify row_number() over (
          partition by tm.id_run, tm.id_trans
          order by tm.crossval_score['{select_score}'] {ifelse(select_maximize, "desc", "asc")}
      ) = 1;
        ]"
      )) |>
      convert_list_cols(
        c("learner_params", "crossval_score"),
        kv_df_to_list
      )

    message(glue::glue(
      "Fitting full models for {nrow(best_models)} transitions..."
    ))

    best_models |>
      split(by = c("id_run", "id_trans")) |>
      run_parallel_evoland(
        items = _,
        worker_fun = fit_full_model_worker,
        parent_db = self,
        cluster = cluster,
      ) |>
      data.table::rbindlist() |>
      as_trans_models_t()
  }
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
    "id_run=",
    models$id_run,
    "_id_trans=",
    models$id_trans,
    "_",
    models$learner_id
  )

  plots
}
