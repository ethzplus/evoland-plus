#' Create Transition Metadata Table
#'
#' Creates a trans_meta_t table based on observed land use transitions in the
#' LULC data. This function analyzes transition patterns and creates metadata
#' entries for each viable transition type.
#'
#' @name trans_meta_t
#'
#' @param db An [evoland_db] instance with populated lulc_data_t and lulc_meta_t tables
#'
#' @return A data.table of class "trans_meta_t" with columns:
#'   - `id_trans`: Unique ID for each transition
#'   - `id_lulc_anterior`: Foreign key to lulc_meta_t (before transition)
#'   - `id_lulc_posterior`: Foreign key to lulc_meta_t (after transition)
#'   - `cardinality`: How many times this transition occurred
#'   - `frequency_rel`: Frequency relative to all transitions in this timestep
#'   - `frequency_abs`: Frequency relative to all coordinate pairs with data in this timestep
#'   - `is_viable`: Whether this transition is viable for modelling
#' @export
as_trans_meta_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_trans = integer(0),
      id_lulc_anterior = integer(0),
      id_lulc_posterior = integer(0),
      cardinality = integer(0),
      frequency_rel = numeric(0),
      frequency_abs = numeric(0),
      is_viable = logical(0)
    )
  }
  new_evoland_table(
    x,
    "trans_meta_t",
    "id_trans"
  )
}

#' @export
create_trans_meta_t <- function() {
  # For now, create an empty table with proper structure
  # Full implementation would analyze transitions in lulc_data_t
  # and compute frequency statistics

  x <- data.table::data.table(
    id_trans = integer(0),
    id_lulc_anterior = integer(0),
    id_lulc_posterior = integer(0),
    cardinality = integer(0),
    frequency_rel = numeric(0),
    frequency_abs = numeric(0),
    is_viable = logical(0)
  )

  as_trans_meta_t(x)
}

#' @export
validate.trans_meta_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_trans",
      "id_lulc_anterior",
      "id_lulc_posterior",
      "cardinality",
      "frequency_rel",
      "frequency_abs",
      "is_viable"
    )
  )

  stopifnot(
    is.integer(x[["id_trans"]]),
    is.integer(x[["id_lulc_anterior"]]),
    is.integer(x[["id_lulc_posterior"]]),
    is.integer(x[["cardinality"]]),
    is.numeric(x[["frequency_rel"]]),
    is.numeric(x[["frequency_abs"]]),
    is.logical(x[["is_viable"]]),
    !anyDuplicated(x[["id_trans"]]),
    !anyDuplicated(x, by = c("id_lulc_anterior", "id_lulc_posterior")),
    all(x[["cardinality"]] >= 0),
    all(x[["frequency_rel"]] >= 0 & x[["frequency_rel"]] <= 1),
    all(x[["frequency_abs"]] >= 0 & x[["frequency_abs"]] <= 1)
  )

  return(x)
}

#' @export
#' @describeIn trans_meta_t Print a trans_meta_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.trans_meta_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_viable <- sum(x[["is_viable"]])
    n_total <- nrow(x)
    total_transitions <- sum(x[["cardinality"]])

    cat(glue::glue(
      "Transition Metadata Table\n",
      "Total transition types: {n_total}\n",
      "Viable for modelling: {n_viable}\n",
      "Total observed transitions: {total_transitions}\n\n"
    ))
  } else {
    cat("Transition Metadata Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
