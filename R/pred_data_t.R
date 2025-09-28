#' Create Predictor Data Table from Configuration
#'
#' Creates pred_data_t tables based on the predictor data specification in an
#' evoland_config object. This function creates empty tables with proper structure
#' for different data types (float, int, bool).
#'
#' @name pred_data_t
#'
#' @param x Coercible to data.table
#' @param type Character string specifying the data type: "float", "int", or "bool"
#'
#' @return A data.table of class "pred_data_t_<type>" and "pred_data_t" with columns:
#'   - `id_pred`: Foreign key to pred_meta_t
#'   - `id_coord`: Foreign key to coords_t
#'   - `id_period`: Foreign key to periods_t
#'   - `value`: Predictor value (type depends on subclass)
#' @export
as_pred_data_t <- function(x, type) {
  stopifnot(type %in% c("float", "int", "bool"))

  # Create empty table with proper value type
  coercion_fn <- switch(
    type,
    float = as.numeric,
    int = as.integer,
    bool = as.logical
  )

  data.table::setDT(x, key = c("id_pred", "id_coord", "id_period"))
  data.table::set(x, j = "value", value = coercion_fn(x$value))

  class_name <- paste0("pred_data_t_", type)
  new_evoland_table(x, c(class_name, "pred_data_t"))
}

#' @export
validate.pred_data_t <- function(x, ...) {
  # Check that it's a data.table
  if (!inherits(x, "data.table")) {
    stop("pred_data_t must inherit from data.table")
  }

  # Check required columns
  check_missing_names(x, c("id_pred", "id_coord", "id_period", "value"))

  # Check column types
  if (!is.integer(x[["id_pred"]])) {
    id_preds <- as.integer(x[["id_pred"]])
    if (anyNA(id_preds)) {
      stop("id_pred must be int or coercible to it")
    }
    data.table::set(x, j = "id_pred", value = id_preds)
  }

  if (!is.integer(x[["id_coord"]])) {
    id_coords <- as.integer(x[["id_coord"]])
    if (anyNA(id_coords)) {
      stop("id_coord must be int or coercible to it")
    }
    data.table::set(x, j = "id_coord", value = id_coords)
  }

  if (!is.integer(x[["id_period"]])) {
    id_periods <- as.integer(x[["id_period"]])
    if (anyNA(id_periods)) {
      stop("id_period must be int or coercible to it")
    }
    data.table::set(x, j = "id_period", value = id_periods)
  }

  # if empty, don't run soft checks
  if (nrow(x) == 0L) {
    return(x)
  }

  # Check for unique combinations of id_pred, id_coord, id_period
  if (anyDuplicated(x, by = c("id_pred", "id_coord", "id_period"))) {
    stop("combinations of id_pred, id_coord, id_period must be unique")
  }

  # Check for positive IDs
  if (any(x[["id_pred"]] <= 0) || any(x[["id_coord"]] <= 0) || any(x[["id_period"]] <= 0)) {
    stop("id_pred, id_coord, and id_period must be positive integers")
  }

  return(x)
}

#' @export
validate.pred_data_t_float <- function(x, ...) {
  x <- NextMethod()

  if (!is.numeric(x[["value"]])) {
    stop("value must be numeric for pred_data_t_float")
  }

  return(x)
}

#' @export
validate.pred_data_t_int <- function(x, ...) {
  x <- NextMethod()

  if (!is.integer(x[["value"]])) {
    value_ints <- as.integer(x[["value"]])
    if (anyNA(value_ints)) {
      stop("value must be int or coercible to it for pred_data_t_int")
    }
    data.table::set(x, j = "value", value = value_ints)
  }

  return(x)
}

#' @export
validate.pred_data_t_bool <- function(x, ...) {
  x <- NextMethod()

  if (!is.logical(x[["value"]])) {
    stop("value must be logical for pred_data_t_bool")
  }

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
    n_preds <- data.table::uniqueN(x[["id_pred"]])
    n_coords <- data.table::uniqueN(x[["id_coord"]])
    n_periods <- data.table::uniqueN(x[["id_period"]])

    cat(glue::glue(
      "Predictor Data Table ({subtype})\n",
      "Observations: {nrow(x)}\n",
      "Predictors: {n_preds}, Coordinates: {n_coords}, Periods: {n_periods}\n\n"
    ))
  } else {
    cat(glue::glue("Predictor Data Table ({subtype}) (empty)\n"))
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
