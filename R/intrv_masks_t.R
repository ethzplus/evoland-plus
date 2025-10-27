#' Create Intervention Masks Table
#'
#' Construct and validate (part of) an `intrv_masks_t` object. This is used to
#' positively link interventions []
#'
#' @name intrv_masks_t
#'
#' @param x An object that is accepted by [data.table::setDT()]
#'
#' @return A data.table-inheriting object with the columns `id_intrv` and `id_coord`,
#' creating a relation between coordinates to apply a
#'
#' @export
as_intrv_masks_t <- function(x) {
  new_evoland_table(
    x,
    "intrv_masks_t",
    c("id_intrv", "id_coord")
  )
}

#' @export
validate.intrv_masks_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c("id_intrv", "id_coord")
  )

  stopifnot(
    is.integer(x[["id_intrv"]]),
    is.integer(x[["id_coord"]]),
    !anyDuplicated(x, by = c("id_intrv", "id_coord"))
  )

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
