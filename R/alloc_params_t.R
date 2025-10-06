#' Create Allocation Parameters Table
#'
#' Creates a alloc_params_t table for storing transition model metadata and
#' serialized model objects. This function creates an empty table with proper
#' structure for storing fitted models.
#'
#' @name alloc_params_t
#'
#' @param x A list or data.frame coercible to a data.table
#'
#' @return A data.table of class "alloc_params_t" with columns:
#'   - `id_trans`: Foreign key to trans_meta_t
#'   - `id_period`: Foreign key to periods_t
#'   - `alloc_params`: Map of model (hyper) parameters
#'   - `goodness_of_fit`: Map of various measures of fit (e.g., ROC AUC)
#' @export
as_alloc_params_t <- function(x) {
  data.table::setDT(x)
  new_evoland_table(x, "alloc_params_t")
}

#' @export
validate.alloc_params_t <- function(x, ...) {
  # Check that it's a data.table
  if (!inherits(x, "data.table")) {
    stop("alloc_params_t must inherit from data.table")
  }
  # Check required columns
  check_missing_names(
    x,
    c(
      "id_trans",
      "id_period",
      "alloc_params",
      "goodness_of_fit"
    )
  )

  # Check column types
  if (!is.integer(x[["id_trans"]])) {
    id_trans <- as.integer(x[["id_trans"]])
    if (anyNA(id_trans)) {
      stop("id_trans must be int or coercible to it")
    }
    data.table::set(x, j = "id_trans", value = id_trans)
  }

  if (!is.integer(x[["id_period"]])) {
    id_periods <- as.integer(x[["id_period"]])
    if (anyNA(id_periods)) {
      stop("id_period must be int or coercible to it")
    }
    data.table::set(x, j = "id_period", value = id_periods)
  }

  data.table::setkeyv(x, c("id_trans", "id_period"))

  if (!is.list(x[["alloc_params"]])) {
    stop("alloc_params must be a list")
  }

  if (!is.list(x[["goodness_of_fit"]])) {
    stop("goodness_of_fit must be a list")
  }

  # if empty, don't run soft checks
  if (nrow(x) == 0L) {
    return(x)
  }

  # Check for positive IDs
  if (any(x[["id_trans"]] <= 0) || any(x[["id_period"]] <= 0)) {
    stop("id_trans and id_period must be positive integers")
  }

  return(x)
}

#' @export
#' @describeIn alloc_params_t Print a alloc_params_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.alloc_params_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_trans <- data.table::uniqueN(x[["id_trans"]])
    n_periods <- data.table::uniqueN(x[["id_period"]])

    cat(glue::glue(
      "Allocation Parameters Table\n",
      "Rows: {nrow(x)}\n",
      "Transitions: {n_trans}, Periods: {n_periods}\n"
    ))
  } else {
    cat("Allocation Parameters Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
