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
as_pred_data_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_run = integer(0),
      id_period = integer(0),
      id_pred = integer(0),
      id_coord = integer(0),
      value = double(0)
    )
  }

  data.table::setDT(x) |>
    cast_dt_col("id_run", "int") |>
    cast_dt_col("id_period", "int") |>
    cast_dt_col("id_pred", "int") |>
    cast_dt_col("id_coord", "int")

  as_parquet_db_t(
    x,
    class_name = "pred_data_t",
    key_cols = c("id_run", "id_period", "id_pred", "id_coord"),
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

#' @describeIn pred_data_t Check if predictor data is complete, i.e. each entry in
#' [pred_meta_t] is either present in period 0 or for all other periods for a given run.
pred_data_available_v <- function(self) {
  system.file("pred_data_present.sql", package = "evoland") |>
    readLines() |>
    paste(collapse = "\n") |>
    glue::glue() |>
    self$get_query(
      pred_data_read_expr = self$get_read_expr("pred_data_t"),
      periods_read_expr = self$get_read_expr("periods_t"),
      pred_meta_read_expr = self$get_read_expr("pred_meta_t"),
      runs_read_expr = self$get_read_expr("runs_t")
    )
}

#' @describeIn pred_data_t Get transitions along with their predictor data in a wide data.table
#' @param self, [evoland_db] instance to query
#' @param id_trans Integer transition ID, see [trans_meta_t]
#' @param id_pred Optional integer vector of predictor IDs to include; if
#'        missing, use all predictor IDs from [pred_meta_t]
#' @param ordered - if TRUE, order output by id_period then id_coord (otherwise no
#'        guaranteed order; need ordering for reproducible subsampling)
#' @return data.table with columns id_coord, id_period, result (bool), and one column
#' per predictor (id_pred_{n})
trans_pred_data_v <- function(
  self,
  id_trans,
  id_pred,
  ordered = FALSE
) {
  stopifnot(
    "id_trans must be a single integer" = {
      length(id_trans) == 1L && as.integer(id_trans) == id_trans
    },
    "id_pred must be missing or a numeric vector" = {
      missing(id_pred) || all(as.integer(id_pred) == id_pred)
    },
    "run_id must be set" = !is.null(self$run_id)
  )

  pred_meta_t <- self$pred_meta_t
  if (missing(id_pred)) {
    id_pred <- pred_meta_t[["id_pred"]]
  }

  result <-
    system.file("trans_pred_data.sql", package = "evoland") |>
    readLines() |>
    paste(collapse = "\n") |>
    glue::glue(
      lulc_data_read_expr = self$get_read_expr("lulc_data_t"),
      period_read_expr = self$get_read_expr("periods_t"),
      pred_data_read_expr = self$get_read_expr("pred_data_t"),
      trans_meta_read_expr = self$get_read_expr("trans_meta_t"),
      id_trans = id_trans,
      id_pred = id_pred
    ) |>
    self$get_query()

  set_pred_coltypes(result, pred_meta_t)

  if (ordered) {
    data.table::setkeys(result, c("id_coord", "id_period"))
  }

  result
}


#' @describeIn pred_data_t In-place casting of predictor columns to their
#' correct data types based on pred_meta_t; also fills NA values with fill_value
#' from pred_meta_t if specified
#' @param result data.table with predictor columns named id_pred_{n}
#' @param pred_meta_t see [pred_meta_t]
#' @keywords internal
set_pred_coltypes <- function(result, pred_meta_t) {
  # look up data type for each predictor and cast accordingly; also convert factors to R
  # factors with correct levels
  for (col in grep("^id_pred_", names(result), value = TRUE)) {
    meta_row <- pred_meta_t[paste0("id_pred_", id_pred) == col]
    if (nrow(meta_row) != 1L) {
      # should never happen, but just in case
      next
    }
    cast_type <- dtype <- as.character(meta_row$data_type)
    cast_type <- if (dtype == "factor") "integer" # cast to int, then add attrs

    cast_dt_col(result, col, cast_type)
    if (dtype == "factor") {
      lvls <- meta_row$factor_levels[[1L]]
      data.table::setattr(result[[col]], "levels", lvls)
      data.table::setattr(result[[col]], "class", "factor")
    }

    fill_value <- meta_row$fill_value |> type.convert()
    if (!is.na(fill_value)) {
      data.table::set(
        result,
        i = which(is.na(result[[col]])),
        j = col,
        value = meta_row$fill_value
      )
    }
  }
}
