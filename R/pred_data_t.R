#' Create Predictor Data Table
#'
#' Construct and validate a `pred_data_t` objects, which are used to store predictor
#' data. Different data types (float, int, bool) have different subclasses
#' (pred_data_t_float, ...)
#'
#' @name pred_data_t
#'
#' @param x Coercible to data.table
#' @param type Character string specifying the data type: "float", "int", or "bool"
#'
#' @return A data.table of class "pred_data_t_<type>" and "pred_data_t" with columns:
#'   - `id_run`: Foreign key to runs_t
#'   - `id_pred`: Foreign key to pred_meta_t
#'   - `id_coord`: Foreign key to coords_t
#'   - `id_period`: Foreign key to periods_t
#'   - `value`: Predictor value (type depends on subclass)
#' @export
as_pred_data_t <- function(x, type) {
  stopifnot(type %in% c("float", "int", "bool"))

  if (missing(x)) {
    x <- data.table::data.table(
      id_run = integer(0),
      id_period = integer(0),
      id_pred = integer(0),
      id_coord = integer(0),
      value = integer(0)
    )
  }

  data.table::setDT(x) |>
    cast_dt_col("id_run", "int") |>
    cast_dt_col("id_period", "int") |>
    cast_dt_col("id_pred", "int") |>
    cast_dt_col("id_coord", "int") |>
    cast_dt_col("value", type)

  class_name <- paste0("pred_data_t_", type)
  new_evoland_table(
    x,
    class_name = c(class_name, "pred_data_t"),
    key_cols = c("id_run", "id_period", "id_pred"),
    partition_cols = c("id_run", "id_period")
  )
}

#' @export
validate.pred_data_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_run",
      "id_period",
      "id_pred",
      "id_coord",
      "value"
    )
  )

  stopifnot(
    is.integer(x[["id_run"]]),
    is.integer(x[["id_period"]]),
    is.integer(x[["id_pred"]]),
    is.integer(x[["id_coord"]]),
    !anyDuplicated(x, by = c("id_run", "id_pred", "id_coord", "id_period"))
  )

  return(x)
}

#' @export
validate.pred_data_t_float <- function(x, ...) {
  x <- NextMethod()

  stopifnot(is.numeric(x[["value"]]))

  return(x)
}

#' @export
validate.pred_data_t_int <- function(x, ...) {
  x <- NextMethod()

  stopifnot(is.integer(x[["value"]]))

  return(x)
}

#' @export
validate.pred_data_t_bool <- function(x, ...) {
  x <- NextMethod()

  stopifnot(is.logical(x[["value"]]))

  return(x)
}

#' @export
#' @describeIn pred_data_t Print a pred_data_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.pred_data_t <- function(x, nrow = 10, ...) {
  # Determine subtype
  subtype <- if (inherits(x, "pred_data_t_float")) {
    "float"
  } else if (inherits(x, "pred_data_t_int")) {
    "int"
  } else if (inherits(x, "pred_data_t_bool")) {
    "bool"
  } else {
    "unknown"
  }

  if (nrow(x) > 0) {
    n_runs <- data.table::uniqueN(x[["id_run"]])
    n_periods <- data.table::uniqueN(x[["id_period"]])
    n_preds <- data.table::uniqueN(x[["id_pred"]])
    n_coords <- data.table::uniqueN(x[["id_coord"]])

    cat(glue::glue(
      "Predictor Data Table ({subtype})\n",
      "Observations: {nrow(x)}\n",
      "Runs: {n_runs}, Periods: {n_periods}, Predictors: {n_preds}, Coordinates: {n_coords}\n\n"
    ))
  } else {
    cat(glue::glue("Predictor Data Table ({subtype}) (empty)\n"))
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
