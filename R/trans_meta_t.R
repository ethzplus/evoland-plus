#' Create Transition Metadata Table
#'
#' Creates a trans_meta_t table based on observed land use transitions in the
#' LULC data. This function analyzes transition patterns and creates metadata
#' entries for each viable transition type.
#'
#' @name trans_meta_t
#'
#' @param lulc_data A [lulc_data_t] table with land use observations
#' @param min_cardinality_abs Minimum absolute number of transitions for viability (optional)
#' @param min_frequency_rel Minimum relative frequency of transitions for viability (optional)
#' @param exclude_anterior Vector of id_lulc values to exclude as anterior (source) classes
#' @param exclude_posterior Vector of id_lulc values to exclude as posterior (target) classes
#'
#' @return A data.table of class "trans_meta_t" with columns:
#'   - `id_trans`: Unique ID for each transition
#'   - `id_lulc_anterior`: Foreign key to lulc_meta_t (before transition)
#'   - `id_lulc_posterior`: Foreign key to lulc_meta_t (after transition)
#'   - `cardinality`: How many times this transition occurred
#'   - `frequency_rel`: Frequency relative to all transitions
#'   - `frequency_abs`: Frequency relative to all coordinate pairs
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

#' @describeIn trans_meta_t Calculate the transition metadata and mark for modelling feasibility
#' @export
create_trans_meta_t <- function(
  lulc_data,
  min_cardinality_abs = NULL,
  min_frequency_rel = NULL,
  exclude_anterior = NULL,
  exclude_posterior = NULL
) {
  # Return empty table if no data provided
  if (missing(lulc_data) || nrow(lulc_data) == 0L) {
    return(as_trans_meta_t())
  }

  # Ensure lulc_data is a data.table
  lulc_data <- data.table::as.data.table(lulc_data)
  data.table::setkey(lulc_data, id_coord, id_period)

  # Create transitions by self-joining on id_coord to find consecutive periods
  # Rename columns to distinguish anterior (i) from posterior (x) states
  transitions <-
    lulc_data[,
      .(id_period = id_period + 1L, id_lulc_posterior = id_lulc, id_coord)
    ][
      lulc_data,
      .(
        id_lulc_anterior = i.id_lulc,
        id_lulc_posterior,
        id_period
      ),
      on = .(id_coord, id_period),
      # exclude e.g. any transition to period 1, or where data point only shows up later
      nomatch = NULL
    ]

  # Count total coordinate-period pairs that could have transitions
  n_total_pairs <- nrow(transitions)

  # Aggregate transitions by type and calculate frequencies
  trans_summary <-
    transitions[
      id_lulc_anterior != id_lulc_posterior
    ][,
      .(cardinality = .N),
      by = .(id_lulc_anterior, id_lulc_posterior)
    ][,
      `:=`(
        frequency_rel = cardinality / sum(cardinality),
        frequency_abs = cardinality / n_total_pairs
      )
    ]

  # Determine viability
  trans_summary[, is_viable := TRUE]

  # Apply exclusions
  if (!is.null(exclude_anterior) && length(exclude_anterior) > 0) {
    trans_summary[id_lulc_anterior %in% exclude_anterior, is_viable := FALSE]
  }

  if (!is.null(exclude_posterior) && length(exclude_posterior) > 0) {
    trans_summary[id_lulc_posterior %in% exclude_posterior, is_viable := FALSE]
  }

  # Apply minimum thresholds (OR logic - satisfy at least one if both specified)
  if (!is.null(min_cardinality_abs) || !is.null(min_frequency_rel)) {
    if (!is.null(min_cardinality_abs) && !is.null(min_frequency_rel)) {
      # Both specified: viable if either threshold is met
      trans_summary[
        is_viable == TRUE &
          cardinality < min_cardinality_abs &
          frequency_rel < min_frequency_rel,
        is_viable := FALSE
      ]
    } else if (!is.null(min_cardinality_abs)) {
      # Only absolute threshold specified
      trans_summary[
        is_viable == TRUE & cardinality < min_cardinality_abs,
        is_viable := FALSE
      ]
    } else {
      # Only relative threshold specified
      trans_summary[
        is_viable == TRUE & frequency_rel < min_frequency_rel,
        is_viable := FALSE
      ]
    }
  }

  # Add id_trans
  trans_summary[, id_trans := .I]

  as_trans_meta_t(trans_summary)
}

#' @export
validate.trans_meta_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_lulc_anterior",
      "id_lulc_posterior",
      "cardinality",
      "frequency_rel",
      "frequency_abs",
      "is_viable"
    )
  )

  # we don't know if there's an id_trans
  data.table::setcolorder(x, "id_trans", before = "id_lulc_anterior", skip_absent = TRUE)

  stopifnot(
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
