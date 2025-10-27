#' Create LULC Data Table
#'
#' Creates an `lulc_data_t` table and validates that it matches the schema.
#'
#' @name lulc_data_t
#'
#' @param x An object that can be passed to [data.table::setDT()]
#'
#' @return A data.table of class "lulc_data_t" with columns:
#'   - `id_coord`: Foreign key to coords_t
#'   - `id_lulc`: Foreign key to lulc_meta_t
#'   - `id_period`: Foreign key to periods_t
#' @export
as_lulc_data_t <- function(x) {
  new_evoland_table(
    x,
    "lulc_data_t",
    c("id_coord", "id_lulc", "id_period")
  )
}

#' @export
validate.lulc_data_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_coord",
      "id_lulc",
      "id_period"
    )
  )

  stopifnot(
    is.integer(x[["id_coord"]]),
    is.integer(x[["id_lulc"]]),
    is.integer(x[["id_period"]]),
    !anyDuplicated(x, by = c("id_coord", "id_lulc", "id_period"))
  )

  return(x)
}

#' @export
#' @describeIn lulc_data_t Print an lulc_data_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.lulc_data_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_coords <- data.table::uniqueN(x[["id_coord"]])
    n_lulc <- data.table::uniqueN(x[["id_lulc"]])
    n_periods <- data.table::uniqueN(x[["id_period"]])

    cat(glue::glue(
      "LULC Data Table\n",
      "Observations: {nrow(x)}\n",
      "Coordinates: {n_coords}, LULC classes: {n_lulc}, Periods: {n_periods}\n\n"
    ))
  } else {
    cat("LULC Data Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
