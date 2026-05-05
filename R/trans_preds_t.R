#' Create Transition-Predictor Relationship Table
#'
#' Creates a trans_preds_t table based on the relationships between transitions
#' and predictors. This function establishes which predictors are useful for
#' modelling each transition type.
#'
#' @name trans_preds_t
#'
#' @param db An [evoland_db] instance with populated trans_meta_t and pred_meta_t tables
#'
#' @return A data.table of class "trans_preds_t" with columns:
#'   - `id_run`: Foreign key to runs_t
#'   - `id_pred`: Foreign key to pred_meta_t
#'   - `id_trans`: Foreign key to trans_meta_t
#' @export
as_trans_preds_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_run = integer(0),
      id_pred = integer(0),
      id_trans = integer(0)
    )
  }

  data.table::setDT(x) |>
    cast_dt_col("id_run", "int") |>
    cast_dt_col("id_pred", "int") |>
    cast_dt_col("id_trans", "int")

  as_parquet_db_t(
    x,
    "trans_preds_t",
    key_cols = c("id_run", "id_pred", "id_trans")
  )
}

#' @export
validate.trans_preds_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_run",
      "id_pred",
      "id_trans"
    )
  )

  # Skip soft checks if empty
  if (nrow(x) == 0L) {
    return(x)
  }

  stopifnot(
    is.integer(x[["id_run"]]),
    is.integer(x[["id_pred"]]),
    is.integer(x[["id_trans"]]),
    all(x[["id_pred"]] > 0),
    all(x[["id_trans"]] > 0)
  )

  return(x)
}

#' @export
#' @describeIn trans_preds_t Print a trans_preds_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.trans_preds_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_preds <- data.table::uniqueN(x[["id_pred"]])
    n_trans <- data.table::uniqueN(x[["id_trans"]])
    avg_preds_per_trans <- nrow(x) / n_trans
    avg_trans_per_pred <- nrow(x) / n_preds

    cat(glue::glue(
      "Transition-Predictor Relationships Table\n",
      "Total relationships: {nrow(x)}\n",
      "Predictors: {n_preds}, Transitions: {n_trans}\n",
      "Avg predictors per transition: {round(avg_preds_per_trans, 1)}\n",
      "Avg transitions per predictor: {round(avg_trans_per_pred, 1)}\n\n"
    ))
  } else {
    cat("Transition-Predictor Relationships Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}

#' @describeIn trans_preds_t Set an initial full set of transition / predictor relations
#' @param overwrite Bool, should a potentially existing table be overwritten?
set_full_trans_preds <- function(self, overwrite = FALSE) {
  p <- self$pred_meta_t
  t <- self$trans_meta_t[is_viable == TRUE]

  full <- expand.grid(
    id_run = 0L,
    id_pred = p[["id_pred"]],
    id_trans = t[["id_trans"]],
    KEEP.OUT.ATTRS = FALSE
  )

  self$commit(
    as_trans_preds_t(full),
    "trans_preds_t",
    method = "overwrite"
  )
}

# Worker function for parallel mlr3filter::Filter
# Not exported; used internally by get_pred_filter_score
pred_filter_worker <- function(item, db, filter, ordered_pred_data = FALSE) {
  stopifnot(inherits(filter, "Filter"), inherits(item, "data.frame"))
  # item has constant id_run and id_trans; extract first value into scalar
  id_run <- item[["id_run"]][1L]
  id_trans <- item[["id_trans"]][1L]
  id_pred <- item[["id_pred"]]
  filter_id <- filter$id

  tryCatch(
    {
      # Get wide transition-predictor data
      trans_pred_data_v <- db$trans_pred_data_v(
        id_trans = id_trans,
        id_pred = id_pred,
        ordered = ordered_pred_data
      )[, -c("id_coord", "id_period_anterior")]

      if (nrow(trans_pred_data_v) == 0L) {
        stop(glue::glue(
          "No data for transition {id_trans}; not pruning"
        ))
      }

      # Coerce target; mlr3 uses factors internally also for twoclass classification
      trans_pred_data_v[, did_transition := factor(did_transition, levels = c("FALSE", "TRUE"))]

      filter_task <- mlr3::as_task_classif(
        trans_pred_data_v,
        target = "did_transition",
        positive = "TRUE"
      )

      # this _will_ error if the filter is incompatible with the data. should we
      # hard error?
      filter$calculate(filter_task)

      scores_dt <-
        data.table::as.data.table(filter) |>
        setNames(c("id_pred", filter_id))

      scores_dt[, id_pred := as.integer(sub("^id_pred_", "", id_pred))]
      scores_dt[, id_run := id_run]
      scores_dt[, id_trans := id_trans]

      return(scores_dt)
    },
    error = function(e) {
      warning(glue::glue("Error processing id_trans?={id_trans}: {e$message}"))
      item[[filter_id]] <- NA_real_
      return(item)
    }
  )
}

#' @describeIn trans_preds_t Get a filter score for all transition-predictor
#' relationships based on mlr3filters. Returns trans_preds_t with an additional
#' column named after the filter$id. The filter score can be used for feature
#' selection: simply subset according to the score and overwrite trans_preds_t
#' in the database using `db$trans_preds_t <- trans_preds_t[score > threshold]`
#' or similar.
#' @param filter An [mlr3filters::Filter] object or a character string
#' specifying the filter method, retrieved via [mlr3filters::flt]. Note that your
#' filter must be compatible with the feature data types; compare your
#' `pred_meta_t` table to <https://mlr3filters.mlr-org.com> for filter compatibility.
#' @param cluster An optional cluster object, see [run_parallel_evoland]
#' @param ordered_pred_data Bool, should the predictor data be ordered? Needed
#' for fully deterministic behavior
#' @param ... Additional arguments passed to `flt` if `filter` is a character string
get_pred_filter_score <- function(
  self,
  filter,
  cluster = NULL,
  ordered_pred_data = FALSE,
  ...
) {
  # Accept either a character vector of measure IDs or a list of Measure objects
  if (is.character(filter)) {
    filter <- mlr3filters::flt(filter, ...)
  }
  if (self$row_count("trans_preds_t") == 0) {
    self$set_full_trans_preds()
  }
  items <- split(self$trans_preds_t, by = c("id_run", "id_trans"))

  message(glue::glue("Processing {length(items)} transitions..."))

  run_parallel_evoland(
    items = items,
    worker_fun = pred_filter_worker,
    parent_db = self,
    cluster = cluster,
    filter = filter,
    ordered_pred_data = ordered_pred_data
  ) |>
    data.table::rbindlist() |>
    as_trans_preds_t()
}
