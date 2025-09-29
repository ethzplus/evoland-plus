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
create_trans_meta_t <- function(db) {
  if (!inherits(db, "evoland_db")) {
    stop("db must be an evoland_db instance")
  }

  # Call internal implementation
  .create_trans_meta_t(
    db$config,
    db$lulc_data_t
  )
}

# Internal implementation uses R objects, but for consistency, this should only
# be called on a database object
.create_trans_meta_t <- function(config, lulc_data_t) {
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

  data.table::setkey(x, "id_trans")

  new_evoland_table(x, "trans_meta_t")
}

#' @export
validate.trans_meta_t <- function(x, ...) {
  # Check that it's a data.table
  if (!inherits(x, "data.table")) {
    stop("trans_meta_t must inherit from data.table")
  }

  # Check required columns
  check_missing_names(
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

  # Check column types
  if (!is.integer(x[["id_trans"]])) {
    id_ints <- as.integer(x[["id_trans"]])
    if (anyNA(id_ints)) {
      stop("id_trans must be int or coercible to it")
    }
    data.table::set(x, j = "id_trans", value = id_ints)
  }

  if (!is.integer(x[["id_lulc_anterior"]])) {
    id_ants <- as.integer(x[["id_lulc_anterior"]])
    if (anyNA(id_ants)) {
      stop("id_lulc_anterior must be int or coercible to it")
    }
    data.table::set(x, j = "id_lulc_anterior", value = id_ants)
  }

  if (!is.integer(x[["id_lulc_posterior"]])) {
    id_posts <- as.integer(x[["id_lulc_posterior"]])
    if (anyNA(id_posts)) {
      stop("id_lulc_posterior must be int or coercible to it")
    }
    data.table::set(x, j = "id_lulc_posterior", value = id_posts)
  }

  if (!is.integer(x[["cardinality"]])) {
    cards <- as.integer(x[["cardinality"]])
    if (anyNA(cards)) {
      stop("cardinality must be int or coercible to it")
    }
    data.table::set(x, j = "cardinality", value = cards)
  }

  if (!is.numeric(x[["frequency_rel"]])) {
    stop("frequency_rel must be numeric")
  }

  if (!is.numeric(x[["frequency_abs"]])) {
    stop("frequency_abs must be numeric")
  }

  if (!is.logical(x[["is_viable"]])) {
    stop("is_viable must be logical")
  }

  # if empty, don't run soft checks
  if (nrow(x) == 0L) {
    return(x)
  }

  # Check for unique id_trans values
  if (anyDuplicated(x[["id_trans"]])) {
    stop("id_trans values must be unique")
  }

  # Check for unique combinations of anterior/posterior
  if (anyDuplicated(x, by = c("id_lulc_anterior", "id_lulc_posterior"))) {
    stop("combinations of id_lulc_anterior, id_lulc_posterior must be unique")
  }

  # Check for non-negative cardinality
  if (any(x[["cardinality"]] < 0)) {
    stop("cardinality must be non-negative")
  }

  # Check frequency bounds
  if (any(x[["frequency_rel"]] < 0) || any(x[["frequency_rel"]] > 1)) {
    stop("frequency_rel must be between 0 and 1")
  }

  if (any(x[["frequency_abs"]] < 0) || any(x[["frequency_abs"]] > 1)) {
    stop("frequency_abs must be between 0 and 1")
  }

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
