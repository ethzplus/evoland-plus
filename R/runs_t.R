#' Create Runs Table
#'
#' Creates a runs_t table that stores the identities, hierarchy, and description of runs.
#' Defaults to a table with one row with the base `id_run := 0`
#'
#' @name runs_t
#'
#' @param x A list or data.frame coercible to a data.table
#'
#' @return A data.table of class "runs_t" with columns:
#'   - `id_run`: Foreign key to runs_t
#'   - `id_period`: Foreign key to periods_t
#'   - `id_trans`: Foreign key to trans_meta_t
#'   - `rate`: Transition rate (0 to 1)
#' @export
as_runs_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_run = 0L,
      parent_id_run = NA_integer_,
      description = "Base"
    )
  }

  data.table::setDT(x) |>
    cast_dt_col("id_run", "int") |>
    cast_dt_col("parent_id_run", "int")

  as_parquet_db_t(
    x,
    class_name = "runs_t",
    key_cols = "id_run"
  )
}

#' @export
validate.runs_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_run",
      "parent_id_run",
      "description"
    )
  )

  stopifnot(
    "id_run is not integer" = is.integer(x[["id_run"]]),
    "parent_id_run is not integer" = is.integer(x[["parent_id_run"]]),
    "duplicated id_run" = !anyDuplicated(x, by = "id_run"),
    "no base (0) id_run" = 0L %in% x[["id_run"]]
  )

  return(x)
}

#' @describeIn runs_t Print a runs_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
#' @export
print.runs_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    # could add a small recursive function to look up max hierarchy depth
    cat(glue::glue(
      "Run metadata table\n",
      "Number of runs: {nrow(x)}\n\n"
    ))
  } else {
    cat("Runs Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}

#' @describeIn runs_t Get or set the active run ID; error if no lineage is found
db_active_id_run <- function(self, private, x) {
  if (missing(x)) {
    return(private$active_id_run)
  }
  stopifnot(
    "id_run must be scalar integerish or NULL" = {
      is.null(x) || length(x) == 1L && as.integer(x) == x
    }
  )

  x <- as.integer(x)
  lineage <- get_lineage(self$runs_t, x)

  if (length(lineage) == 0L) {
    stop(glue::glue("Requested run (id_run = {x}) not found in runs_t"), call. = FALSE)
  }

  private$active_id_run <- x
  private$active_run_lineage <- lineage

  invisible(x)
}

# lineage is ordered from most recent to oldest (i.e. id_run, parent_id_run,
# grandparent_id_run, etc.)
get_lineage <- function(runs_t, id_run) {
  lineage <- integer(0)
  current_id <- id_run

  repeat {
    parent_id <- runs_t[id_run == current_id, parent_id_run]

    if (length(parent_id) == 0L) {
      break
    }

    # append to lineage, even if parent_id is NA (base case)
    lineage <- c(lineage, current_id)
    current_id <- parent_id
  }

  lineage
}
