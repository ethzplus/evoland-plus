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
#'   - `id_pred`: Foreign key to pred_meta_t
#'   - `id_trans`: Foreign key to trans_meta_t
#' @export
create_trans_preds_t <- function(db) {
  if (!inherits(db, "evoland_db")) {
    stop("db must be an evoland_db instance")
  }

  # For now, create an empty table with proper structure
  # Full implementation would perform feature selection to determine
  # which predictors are useful for each transition

  x <- data.table::data.table(
    id_pred = integer(0),
    id_trans = integer(0)
  )

  data.table::setkeyv(x, c("id_pred", "id_trans"))

  new_evoland_table(x, "trans_preds_t")
}

#' @export
validate.trans_preds_t <- function(x, ...) {
  # Check that it's a data.table
  if (!inherits(x, "data.table")) {
    stop("trans_preds_t must inherit from data.table")
  }

  # Check required columns
  check_missing_names(x, c("id_pred", "id_trans"))

  # Check column types
  if (!is.integer(x[["id_pred"]])) {
    id_preds <- as.integer(x[["id_pred"]])
    if (anyNA(id_preds)) {
      stop("id_pred must be int or coercible to it")
    }
    data.table::set(x, j = "id_pred", value = id_preds)
  }

  if (!is.integer(x[["id_trans"]])) {
    id_trans <- as.integer(x[["id_trans"]])
    if (anyNA(id_trans)) {
      stop("id_trans must be int or coercible to it")
    }
    data.table::set(x, j = "id_trans", value = id_trans)
  }

  # if empty, don't run soft checks
  if (nrow(x) == 0L) {
    return(x)
  }

  # Check for unique combinations of id_pred, id_trans (composite primary key)
  if (anyDuplicated(x, by = c("id_pred", "id_trans"))) {
    stop("combinations of id_pred, id_trans must be unique")
  }

  # Check for positive IDs
  if (any(x[["id_pred"]] <= 0) || any(x[["id_trans"]] <= 0)) {
    stop("id_pred and id_trans must be positive integers")
  }

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
