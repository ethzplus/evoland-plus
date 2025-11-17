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
as_trans_preds_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_pred = integer(0),
      id_trans = integer(0)
    )
  }
  new_evoland_table(
    x,
    "trans_preds_t",
    c("id_pred", "id_trans")
  )
}

#' @export
create_trans_preds_t <- function() {
  # For now, create an empty table with proper structure
  # Full implementation would perform feature selection to determine
  # which predictors are useful for each transition

  x <- data.table::data.table(
    id_pred = integer(0),
    id_trans = integer(0)
  )

  data.table::setkeyv(x, c("id_pred", "id_trans"))

  as_trans_preds_t(x)
}

#' @export
validate.trans_preds_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_pred",
      "id_trans"
    )
  )

  # Skip soft checks if empty
  if (nrow(x) == 0L) {
    return(x)
  }

  stopifnot(
    is.integer(x[["id_pred"]]),
    is.integer(x[["id_trans"]]),
    !anyDuplicated(x, by = c("id_pred", "id_trans")),
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
