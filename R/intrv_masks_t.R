#' Create Intervention Masks Table from Configuration
#'
#' Creates an intrv_masks_t table based on the intervention data specification in an
#' evoland_config object. This function creates empty tables with proper structure
#' for intervention masks.
#'
#' @name intrv_masks_t
NULL

#' @export
validate.intrv_masks_t <- function(x, ...) {
  # Check that it's a data.table
  if (!inherits(x, "data.table")) {
    stop("intrv_masks_t must inherit from data.table")
  }

  # Check required columns
  data.table::setcolorder(x, c("id_intrv", "id_coord"))
  data.table::setkeyv(x, c("id_intrv", "id_coord"))

  # Check column types
  if (!is.integer(x[["id_intrv"]])) {
    id_intrvs <- as.integer(x[["id_intrv"]])
    if (anyNA(id_intrvs)) {
      stop("id_intrv must be int or coercible to it")
    }
    data.table::set(x, j = "id_intrv", value = id_intrvs)
  }

  if (!is.integer(x[["id_coord"]])) {
    id_coords <- as.integer(x[["id_coord"]])
    if (anyNA(id_coords)) {
      stop("id_coord must be int or coercible to it")
    }
    data.table::set(x, j = "id_coord", value = id_coords)
  }

  # if empty, don't run soft checks
  if (nrow(x) == 0L) {
    return(x)
  }

  # Check for unique combinations of id_intrv, id_coord (composite primary key)
  if (anyDuplicated(x, by = c("id_intrv", "id_coord"))) {
    stop("combinations of id_intrv, id_coord must be unique")
  }

  # Check for positive IDs
  if (any(x[["id_intrv"]] <= 0) || any(x[["id_coord"]] <= 0)) {
    stop("id_intrv and id_coord must be positive integers")
  }

  return(x)
}

#' @export
#' @describeIn intrv_masks_t Print an intrv_masks_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.intrv_masks_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_intrvs <- data.table::uniqueN(x[["id_intrv"]])
    n_coords <- data.table::uniqueN(x[["id_coord"]])

    cat(glue::glue(
      "Intervention Masks Table\n",
      "Masks: {nrow(x)}\n",
      "Interventions: {n_intrvs}, Coordinates: {n_coords}\n\n"
    ))
  } else {
    cat("Intervention Masks Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
