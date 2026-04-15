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

# Worker function for parallel transition pruning
# Not exported; used internally by get_pruned_trans_preds_t
prune_trans_worker <- function(item, db, filter_fun, ordered_pred_data = FALSE, ...) {
  # item is just a data.table slice. expecting scalar id_run and id_trans
  id_run <- item[["id_run"]][1L]
  id_trans <- item[["id_trans"]][1L]
  id_pred <- item[["id_pred"]]

  tryCatch(
    {
      # Get wide transition-predictor data
      trans_pred_data <- db$trans_pred_data_v(
        id_trans = id_trans,
        id_pred = id_pred,
        ordered = ordered_pred_data
      )

      # Check if we have any data
      pred_cols <- grep("^id_pred_", names(trans_pred_data), value = TRUE)
      if (nrow(trans_pred_data) == 0L || length(pred_cols) == 0L) {
        stop(glue::glue(
          "No data for transition {id_trans}; not pruning"
        ))
      }

      # Return ranked + filtered predictor names as id_pred_{n}
      filtered_preds <- filter_fun(
        # drop vars that are irrelevant to the filtering
        data = trans_pred_data[, .SD, .SDcols = !c("id_coord", "id_period_anterior")],
        ...
      )

      if (length(filtered_preds) == 0L) {
        stop(glue::glue(
          "Filter dropped all predictors for {id_trans}; not pruning"
        ))
      }
      # Parse id_pred values from column names (e.g., "id_pred_1" -> 1)
      selected_ids <- as.integer(sub("^id_pred_", "", filtered_preds))

      # Create result rows
      return(data.table::data.table(
        id_run = id_run,
        id_pred = selected_ids,
        id_trans = id_trans
      ))
    },
    error = function(e) {
      # do not prune on error
      warning(glue::glue(
        "Error processing transition {id_trans}: {e$message}"
      ))
      return(data.table::data.table(
        id_run = id_run,
        id_pred = id_pred,
        id_trans = id_trans
      ))
    }
  )
}

#' @describeIn trans_preds_t Get a pruned set of transition-predictor relationships
#' based on a filtering function
#' @param filter_fun A function that takes a transition-predictor data (cf. [trans_pred_data_v]) and
#' returns a character vector of column names to keep, see e.g. [covariance_filter]
#' @param na_value Value to use for missing data when retrieving predictor data
#' @param cluster An optional cluster object, see [run_parallel_evoland]
#' @param ordered_pred_data Bool, should the predictor data be ordered? needed
#' for fully deterministic behavior
get_pruned_trans_preds_t <- function(
  self,
  filter_fun = covariance_filter,
  cluster = NULL,
  ordered_pred_data = FALSE,
  ...
) {
  if (self$row_count("trans_preds_t") == 0) {
    self$set_full_trans_preds()
  }
  items <- split(self$trans_preds_t, by = c("id_run", "id_trans"))

  message(glue::glue("Processing {length(items)} transitions..."))

  run_parallel_evoland(
    items = items,
    worker_fun = prune_trans_worker,
    parent_db = self,
    cluster = cluster,
    filter_fun = filter_fun,
    ordered_pred_data = ordered_pred_data,
    ...
  ) |>
    data.table::rbindlist() |>
    as_trans_preds_t()
}
