#' Create Predictor Data Table
#'
#' Construct and validate a `pred_data_t` objects, which are used to store predictor
#' data. Different data types (float, int, bool) have different subclasses
#' (pred_data_t_float, ...)
#'
#' @name pred_data_t
#'
#' @param x Coercible to data.table
#'
#' @return A data.table of class "pred_data_t" with columns:
#'   - `id_run`: Foreign key to runs_t
#'   - `id_pred`: Foreign key to pred_meta_t
#'   - `id_coord`: Foreign key to coords_t
#'   - `id_period`: Foreign key to periods_t
#'   - `value`: Predictor value (coerced to double, recoverable through [pred_meta_t])
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
    cast_dt_col("id_coord", "int") |>
    cast_dt_col("value", "float")

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
    is.integer(x[["id_coord"]])
  )

  return(x)
}

#' @export
#' @describeIn pred_data_t Print a pred_data_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.pred_data_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_runs <- data.table::uniqueN(x[["id_run"]])
    n_periods <- data.table::uniqueN(x[["id_period"]])
    n_preds <- data.table::uniqueN(x[["id_pred"]])
    n_coords <- data.table::uniqueN(x[["id_coord"]])

    cat(glue::glue(
      "Raw Predictor Data Table (values as floats)\n",
      "Recover original values through [pred_meta_t]\n",
      "Observations: {nrow(x)}\n",
      "Runs: {n_runs}, Periods: {n_periods}, Predictors: {n_preds}, Coordinates: {n_coords}\n\n"
    ))
  } else {
    cat(glue::glue("Raw Predictor Data Table (empty)\n"))
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
    glue::glue(
      pred_data_read_expr = self$get_read_expr("pred_data_t"),
      periods_read_expr = self$get_read_expr("periods_t"),
      pred_meta_read_expr = self$get_read_expr("pred_meta_t"),
      runs_read_expr = self$get_read_expr("runs_t")
    ) |>
    self$get_query()
}

#' @describeIn pred_data_t Get transitions along with their predictor data in a wide data.table
#' @param self, [evoland_db] instance to query
#' @param id_trans Integer transition ID, see [trans_meta_t]
#' @param id_pred Optional integer vector of predictor IDs to include; if
#'        missing, use all predictor IDs from [pred_meta_t]
#' @param ordered - if TRUE, order output by id_period then id_coord (otherwise no
#'        guaranteed order; need ordering for reproducible subsampling)
#' @return data.table with columns id_coord, id_period, did_transition (bool),
#'         and one column per predictor (`id_pred_{n}`)
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
    "id_run must be set" = !is.null(self$id_run)
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
    data.table::setkeyv(result, c("id_coord", "id_period_anterior"))
  }

  result
}

#' @describeIn pred_data_t Get predictor data in a wide data.table for transition potential
#' prediction (cols `id_coord, id_pred_{n}`)
#' @param id_period_anterior Scalar integerish of anterior period ID, i.e. we
#' get the predictors that explain the processes over the next N years
pred_data_wide_v <- function(
  self,
  id_trans,
  id_period_anterior
) {
  stopifnot(
    "id_trans must be a single integer" = {
      length(id_trans) == 1L && as.integer(id_trans) == id_trans
    },
    "id_period_anterior must be a single integer" = {
      length(id_period_anterior) == 1L && as.integer(id_period_anterior) == id_period_anterior
    },
    "id_run must be set" = !is.null(self$id_run)
  )

  result <-
    system.file("pred_data_wide.sql", package = "evoland") |>
    readLines() |>
    paste(collapse = "\n") |>
    glue::glue(
      trans_meta_read_expr = self$get_read_expr("trans_meta_t"),
      trans_preds_read_expr = self$get_read_expr("trans_preds_t"),
      lulc_data_read_expr = self$get_read_expr("lulc_data_t"),
      pred_data_read_expr = self$get_read_expr("pred_data_t"),
      id_trans = id_trans,
      id_period_anterior = id_period_anterior
    ) |>
    self$get_query()

  set_pred_coltypes(result, self$pred_meta_t)

  result
}

#' @describeIn pred_data_t In-place casting of predictor columns to their
#' correct data types based on pred_meta_t; also fills NA values with fill_value
#' from pred_meta_t if specified
#' @param result data.table with predictor columns named `id_pred_{n}`
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

    # manually reconstructing factors: cast to int, then add attrs
    cast_type <- ifelse(dtype == "factor", "int", cast_type)

    cast_dt_col(result, col, cast_type)
    if (dtype == "factor") {
      lvls <- meta_row$factor_levels[[1L]]
      data.table::setattr(result[[col]], "levels", lvls)
      data.table::setattr(result[[col]], "class", "factor")
    }

    # if col is factor, fill_value being a character is safe
    # dt set() can add a new level if it's not already present
    fill_value <- meta_row$fill_value |> type.convert(as.is = TRUE)
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

#' @describeIn pred_data_t Add a predictor to the database, given a data.table with
#' columns `id_coord`, `id_period`, and `value` (predictor value). Uses the current
#' `id_run`.
#' @param self an [evoland_db] instance
#' @param pred_data_raw data.table with columns id_coord, id_period, and value
#' (predictor value); the data type of the value is stored in [pred_meta_t]
#' @param name Character scalar, unique name of predictor. If already present in
#' `pred_meta_t`, other parameters are ignored and the operation turns into an upsert.
#' @param fill_value Value to use for coordinates registered in [coords_t] but not in
#' the provided `pred_data_raw`. e.g. where no known population is registered,
#' assume pop. 0. For a factor variable, this could be a base case, e.g. for
#' different nature reserve types, this could be the "not in a reserve" type.
#' @param pretty_name opt. Character scalar, friendly name for plots/output
#' @param description opt. Character scalar. Long description / operationalisation
#' @param orig_format opt. Character scalar. Original format description
#' @param sources opt. list of lists: each list containing one `url` and a
#' `md5sum` field, see [create_pred_meta_t()] and [download_and_verify()]
#' @param unit opt. Character scalar. SI unit for physical properties, or more complex
#' descriptors like "bed nights/year" as a proxy for touristic activity
add_predictor <- function(
  self,
  pred_data_raw,
  name,
  fill_value,
  pretty_name = name,
  description = NA_character_,
  orig_format = NA_character_,
  sources = list(),
  unit = NA_character_
) {
  id_pred <- self$column_max("pred_meta_t", "id_pred") + 1L
  if (id_pred > 1L) {
    # if id_pred == 1, this is the first entry in pred_meta_t
    # if higher, we check if this predictor is already in DB
    existing_pred <- self$fetch("pred_meta_t", where = glue::glue("name = '{name}'"))
    if (nrow(existing_pred) > 0L) {
      # use pre-existing id_pred if already exists
      id_pred <- existing_pred[["id_pred"]][1L]
    }
  }

  new_meta_row <- data.table::data.table(
    id_pred = id_pred,
    name = name,
    pretty_name = pretty_name,
    description = description,
    orig_format = orig_format,
    sources = list(sources),
    unit = unit,
    data_type = switch(
      class(pred_data_raw[["value"]]),
      integer = "int",
      numeric = "float",
      logical = "bool",
      factor = "factor",
      Date = "date",
      stop("Unsupported data type for value column")
    ),
    fill_value = fill_value,
    factor_levels = {
      if (is.factor(pred_data_raw[["value"]])) {
        list(levels(pred_data_raw[["value"]]))
      } else {
        list(character(0))
      }
    }
  )

  # upsert
  self$pred_meta_t <- as_pred_meta_t(new_meta_row)

  # construct valid pred data
  pred_data_to_add <- data.table::copy(pred_data_raw)
  pred_data_to_add[, id_run := self$id_run]
  pred_data_to_add[, id_pred := id_pred]

  # upsert
  self$pred_data_t <- as_pred_data_t(pred_data_to_add)
}
