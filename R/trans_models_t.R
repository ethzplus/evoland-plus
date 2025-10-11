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
#'   - `id_trans`: Foreign key to trans_meta_t
#'   - `id_period`: Foreign key to periods_t
#'   - `model_family`: Model family (e.g., "rf", "glm", "bayesian")
#'   - `model_params`: Map of model (hyper) parameters
#'   - `goodness_of_fit`: Map of various measures of fit (e.g., ROC AUC)
#'   - `model_obj_part`: BLOB of serialized model object for validation
#'   - `model_obj_full`: BLOB of serialized model object for extrapolation
#' @export
as_trans_models_t <- function(x) {
  new_evoland_table(
    x,
    "trans_models_t",
    c("id_trans", "id_period")
  )
}

#' @export
validate.trans_models_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_trans",
      "id_period",
      "model_family",
      "model_params",
      "goodness_of_fit",
      "model_obj_part",
      "model_obj_full"
    )
  )

  stopifnot(
    is.integer(x[["id_trans"]]),
    is.integer(x[["id_period"]]),
    is.character(x[["model_family"]]),
    is.list(x[["model_params"]]),
    is.list(x[["goodness_of_fit"]]),
    is.list(x[["model_obj_part"]]),
    is.list(x[["model_obj_full"]]),
    all(x[["id_trans"]] > 0),
    all(x[["id_period"]] > 0),
    !any(x[["model_family"]] == "")
  )

  return(x)
}

#' @export
#' @describeIn trans_models_t Print a trans_models_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.trans_models_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_trans <- data.table::uniqueN(x[["id_trans"]])
    n_periods <- data.table::uniqueN(x[["id_period"]])
    model_families <- unique(x[["model_family"]])
    n_with_part_models <- sum(!vapply(x[["model_obj_part"]], is.null, logical(1)))
    n_with_full_models <- sum(!vapply(x[["model_obj_full"]], is.null, logical(1)))

    cat(glue::glue(
      "Transition Models Table\n",
      "Total models: {nrow(x)}\n",
      "Transitions: {n_trans}, Periods: {n_periods}\n",
      "Model families: {paste(model_families, collapse = ', ')}\n",
      "With partial models: {n_with_part_models}, With full models: {n_with_full_models}\n\n"
    ))
  } else {
    cat("Transition Models Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
