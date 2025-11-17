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
  if (missing(x)) {
    x <- data.table::data.table(
      id_trans = integer(0),
      id_period = integer(0),
      alloc_params = list(),
      goodness_of_fit = list()
    )
  }
  new_evoland_table(
    x,
    "alloc_params_t",
    c("id_trans", "id_period")
  )
}

#' @export
validate.alloc_params_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_trans",
      "id_period",
      "alloc_params",
      "goodness_of_fit"
    )
  )

  stopifnot(
    is.integer(x[["id_trans"]]),
    is.integer(x[["id_period"]]),
    is.list(x[["alloc_params"]]),
    is.list(x[["goodness_of_fit"]]),
    !anyDuplicated(x, by = c("id_trans", "id_period"))
  )

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
