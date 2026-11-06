#' Create Transition-Predictor Relationship Table
#'
#' Creates a trans_preds_t table, which lists the transition / predictor pairs a run
#' considers. `is_selected` says which of them the transition models use: feature
#' selection sets it to `FALSE` for the predictors it prunes. Further columns, such as the
#' scores of [get_pred_filter_score()], are added to the table as they are written; they
#' are for reporting only. When writing a selection back, assign the scored table with its
#' flags rather than a subset of it: rows left out of the assignment keep the flags they had.
#'
#' @name trans_preds_t
#'
#' @param self An [evoland_db] instance with populated trans_meta_t and pred_meta_t tables
#' @param x A list or data.frame coercible to a trans_preds_t object. If missing, an
#' empty table will be created. If it has no `is_selected` column, every pair is selected.
#'
#' @return A data.table of class "trans_preds_t" with columns:
#'   - `id_run`: Foreign key to runs_t
#'   - `id_pred`: Foreign key to pred_meta_t
#'   - `id_trans`: Foreign key to trans_meta_t
#'   - `is_selected`: Logical, whether the models of the transition use the predictor
#'   - any further columns, e.g. filter scores
#' @examples
#' \dontrun{
#' # a typical selection step
#' scores <- db$get_pred_filter_score("importance")
#' scores[, is_selected := importance > 100]
#' db$trans_preds_t <- scores
#' }
#' @export
as_trans_preds_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_run = integer(0),
      id_pred = integer(0),
      id_trans = integer(0),
      is_selected = logical(0)
    )
  }

  data.table::setDT(x)
  if (!"is_selected" %in% names(x)) {
    x[, is_selected := TRUE]
  }

  x |>
    cast_dt_col("id_run", "int") |>
    cast_dt_col("id_pred", "int") |>
    cast_dt_col("id_trans", "int") |>
    cast_dt_col("is_selected", "bool")

  as_ducklake_db_t(
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
      "id_trans",
      "is_selected"
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
    is.logical(x[["is_selected"]]),
    all(x[["id_pred"]] > 0),
    all(x[["id_trans"]] > 0),
    "is_selected must be TRUE or FALSE, not NA" = !anyNA(x[["is_selected"]])
  )

  return(x)
}

#' @export
#' @describeIn trans_preds_t Print a trans_preds_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.trans_preds_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    selected <- x[x[["is_selected"]]]
    n_preds <- data.table::uniqueN(selected[["id_pred"]])
    n_trans <- data.table::uniqueN(selected[["id_trans"]])
    avg_preds_per_trans <- nrow(selected) / n_trans
    avg_trans_per_pred <- nrow(selected) / n_preds

    cat(glue::glue(
      "Transition-Predictor Relationships Table\n",
      "Total relationships: {nrow(x)}, selected: {nrow(selected)}\n",
      "Distinct selected predictors: {n_preds}, distinct transitions: {n_trans}\n",
      "Avg predictors per transition: {round(avg_preds_per_trans, 1)}\n",
      "Avg transitions per predictor: {round(avg_trans_per_pred, 1)}\n\n"
    ))
  } else {
    cat("Transition-Predictor Relationships Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}

#' @describeIn trans_preds_t Set an initial full set of transition / predictor relations for
#' the active run: every predictor for every viable transition, all selected
#' @param reset Bool, start the active run's selection over? This removes the active run's
#' rows, including their score columns' values, before writing the full set.
set_full_trans_preds <- function(self, reset = FALSE) {
  exists <- self$row_count("trans_preds_t", where = glue::glue("id_run = {self$id_run}")) > 0L
  if (exists && !reset) {
    stop("`trans_preds_t` already holds predictor sets for this run; use reset = TRUE")
  }
  p <- self$pred_meta_t
  t <- viable_trans_meta(self)

  full <- expand.grid(
    id_run = self$id_run,
    id_pred = p[["id_pred"]],
    id_trans = t[["id_trans"]],
    is_selected = TRUE,
    KEEP.OUT.ATTRS = FALSE
  )

  self$transaction({
    if (exists) {
      self$delete_from("trans_preds_t", where = glue::glue("id_run = {self$id_run}"))
    }
    self$commit(as_trans_preds_t(full), "trans_preds_t", method = "upsert")
  })
}

# Worker function for parallel mlr3filter::Filter
# Not exported; used internally by get_pred_filter_score
pred_filter_worker <- function(item, db, filter) {
  stopifnot(inherits(filter, "Filter"), inherits(item, "data.frame"))
  # item has constant id_run and id_trans; extract first value into scalar
  id_run <- item[["id_run"]][1L]
  id_trans <- item[["id_trans"]][1L]
  id_pred <- item[["id_pred"]]
  filter_id <- filter$id

  tryCatch(
    {
      # Get wide transition-predictor data
      pred_wide_train <- db$pred_wide_train_v(
        id_trans = id_trans,
        id_pred = id_pred
      )[, -c("id_coord", "id_period_anterior")]

      if (nrow(pred_wide_train) == 0L) {
        stop(glue::glue(
          "No data for transition {id_trans}; not pruning"
        ))
      }

      # Coerce target; mlr3 uses factors internally also for twoclass classification
      pred_wide_train[, did_transition := factor(did_transition, levels = c("FALSE", "TRUE"))]

      filter_task <- mlr3::as_task_classif(
        pred_wide_train,
        target = "did_transition",
        positive = "TRUE"
      )

      # this _will_ error if the filter is incompatible with the data. should we
      # hard error?
      filter$calculate(filter_task)

      scores_dt <-
        data.table::as.data.table(filter) |>
        stats::setNames(c("id_pred", filter_id))

      scores_dt[, id_pred := as.integer(sub("^id_pred_", "", id_pred))]
      scores_dt[, id_run := id_run]
      scores_dt[, id_trans := id_trans]
      scores_dt[, is_selected := TRUE]

      return(scores_dt)
    },
    error = function(e) {
      warning(glue::glue("Error processing id_trans={id_trans}: {e$message}"))
      item[[filter_id]] <- NA_real_
      return(item)
    }
  )
}

#' @describeIn trans_preds_t Get a filter score for the selected transition-predictor
#' relationships based on mlr3filters. Returns trans_preds_t with an additional
#' column named after the filter$id. The filter score can be used for feature
#' selection: set `is_selected` according to the score and write the table back, e.g.
#' `scores[, is_selected := importance > 100]; db$trans_preds_t <- scores`. Writing it
#' back also stores the scores. Only the selected pairs are scored, so a second filter
#' refines the selection of the first.
#' @param filter An [mlr3filters::Filter] object or a character string
#' specifying the filter method, retrieved via [mlr3filters::flt]. Note that your
#' filter must be compatible with the feature data types; compare your
#' `pred_meta_t` table to <https://mlr3filters.mlr-org.com> for filter compatibility.
#' @param id_trans Optional integer vector of transition IDs; only these transitions are
#' scored. `NULL` (default) scores every transition with selected predictors.
#' @param cluster An optional cluster object, see [run_parallel_evoland]
#' @param ... Additional arguments passed to `flt` if `filter` is a character string
get_pred_filter_score <- function(
  self,
  filter = "correlation",
  id_trans = NULL,
  cluster = NULL,
  ...
) {
  # Accept either a character vector of measure IDs or a list of Measure objects
  if (is.character(filter)) {
    filter <- mlr3filters::flt(filter, ...)
  }
  stopifnot("id_trans must be NULL or integers" = is.null(id_trans) || is_integerish(id_trans))
  if (self$row_count("trans_preds_t") == 0) {
    self$set_full_trans_preds()
  }
  # the scores are for the active run; the predictor sets may be inherited from an ancestor,
  # so they are labelled with the active run here. Only the key columns: scores of earlier
  # filters are not passed on to the workers
  items <-
    self$get_query(
      "select {self$id_run}::integer as id_run, id_pred, id_trans, is_selected
       from {self$get_read_expr('trans_preds_t')}
       where is_selected {id_trans_filter}",
      id_trans_filter = if (is.null(id_trans)) {
        DBI::SQL("")
      } else {
        glue::glue_sql("and id_trans in ({id_trans*})", .con = self$connection)
      }
    ) |>
    split(by = "id_trans")

  message(glue::glue("Processing {length(items)} transitions..."))

  run_parallel_evoland(
    items = items,
    worker_fun = pred_filter_worker,
    parent_db = self,
    cluster = cluster,
    filter = filter
  ) |>
    data.table::rbindlist(use.names = TRUE) |>
    as_trans_preds_t()
}
