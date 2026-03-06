#' Create LULC Data Table
#'
#' Creates an `lulc_data_t` table and validates that it matches the schema.
#'
#' @name lulc_data_t
#'
#' @param x An object that can be passed to [data.table::setDT()]
#'
#' @return A data.table of class "lulc_data_t" with columns:
#'   - `id_run`: Foreign key to runs_t
#'   - `id_period`: Foreign key to periods_t
#'   - `id_lulc`: Foreign key to lulc_meta_t
#'   - `id_coord`: Foreign key to coords_t
#'
#' @export
as_lulc_data_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_run = integer(0),
      id_period = integer(0),
      id_lulc = integer(0),
      id_coord = integer(0)
    )
  }

  data.table::setDT(x) |>
    cast_dt_col("id_run", "int") |>
    cast_dt_col("id_period", "int") |>
    cast_dt_col("id_lulc", "int") |>
    cast_dt_col("id_coord", "int")

  as_parquet_db_t(
    x = x,
    class_name = "lulc_data_t",
    key_cols = c("id_run", "id_coord", "id_period"),
    partition_cols = "id_run"
  )
}

#' @export
validate.lulc_data_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_run",
      "id_coord",
      "id_lulc",
      "id_period"
    )
  )

  stopifnot(
    is.integer(x[["id_run"]]),
    is.integer(x[["id_coord"]]),
    is.integer(x[["id_lulc"]]),
    is.integer(x[["id_period"]]),
    !anyDuplicated(x)
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

#' @describeIn lulc_data_t Retrieve LULC data as SpatRaster
#' @param id_period Integer or vector of integers specifying which periods to
#' retrieve; returned as layers.
#' @keywords internal
lulc_data_as_rast <- function(self, id_period = NULL) {
  # Build query to join lulc_data_t with coords_t
  where_clause <- NULL
  if (!is.null(id_period)) {
    where_clause <- glue::glue("id_period in ({toString(id_period)})")
  }

  data <- self$fetch("lulc_data_t", where = where_clause)

  if (nrow(data) == 0L) {
    stop("No LULC data found for the specified periods")
  }

  tabular_to_raster(
    data = data,
    coords = self$coords_minimal,
    value_col = "id_lulc"
  )
}
