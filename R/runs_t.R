#' Create Runs Table
#'
#' Creates a runs_t table that stores the identities, hierarchy, and description of
#' runs. Defaults to a table with one row with the base `id_run := 0`. Runs are a
#' feature that allows one to label data that is either overriding base data (e.g.
#' predictors being used for forecasting) or produced by a specific model experiment
#' (e.g. a given allocation of land use change given a set of transition potential
#' models and allocation parameters).
#'
#' A run's `seed` belongs to that run alone and is not inherited: [alloc_clumpy()] seeds R's
#' random number generator with it before allocating, so that a run replays from the
#' database. Runs without a seed (`NA`) draw from the generator's current state.
#'
#' @name runs_t
#'
#' @param x A list or data.frame coercible to a data.table
#'
#' @return A data.table of class "runs_t" with columns:
#'   - `id_run`: Unique ID for each run; 0 is the base run
#'   - `parent_id_run`: The run this run inherits data from; `NA` for the base run
#'   - `description`: Human-readable description of the run
#'   - `seed`: Seed for the run's random number generator; `NA` for none
#' @export
as_runs_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_run = 0L,
      parent_id_run = NA_integer_,
      description = "Base",
      seed = NA_integer_
    )
  }

  data.table::setDT(x)
  # runs registered before the seed column existed have none
  if (!"seed" %in% names(x)) {
    x[, seed := NA_integer_]
  }

  x |>
    cast_dt_col("id_run", "int") |>
    cast_dt_col("parent_id_run", "int") |>
    cast_dt_col("seed", "int")

  as_ducklake_db_t(
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
      "description",
      "seed"
    )
  )

  stopifnot(
    "id_run is not integer" = is.integer(x[["id_run"]]),
    "parent_id_run is not integer" = is.integer(x[["parent_id_run"]]),
    "seed is not integer" = is.integer(x[["seed"]]),
    "all parent_id_run must be in id_run or NA" = all(
      is.na(x[["parent_id_run"]]) | x[["parent_id_run"]] %in% x[["id_run"]]
    ),
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
#' @param self an evoland_db instance
#' @param private an evoland_db private environment
#' @param y (optional) scalar integerish or NULL; if provided, sets the active run ID and lineage
db_active_id_run <- function(self, private, y) {
  if (missing(y)) {
    return(private$active_id_run)
  }
  if (is.null(y)) {
    private$active_id_run <- NULL
    private$active_run_lineage <- NULL
    return(invisible(NULL))
  }
  stopifnot(
    "id_run must be scalar integerish or NULL" = is_scalar_integerish(y)
  )

  y <- as.integer(y)
  lineage <- get_lineage(self$runs_t, y)

  private$active_id_run <- y
  private$active_run_lineage <- lineage

  invisible(y)
}

# lineage is ordered from most recent to oldest (i.e. id_run, parent_id_run,
# grandparent_id_run, etc.)
get_lineage <- function(runs_t, id_run) {
  lineage <- integer(0)
  current_id <- id_run

  if (nrow(runs_t[id_run == current_id]) == 0L) {
    stop(
      glue::glue(
        "Requested run (id_run = {id_run}) not found in runs_t"
      ),
      call. = FALSE
    )
  }

  repeat {
    parent_id <- runs_t[id_run == current_id, parent_id_run]

    if (length(parent_id) == 0L) {
      stop("parent not found, should never happen with valid runs_t")
    }

    # append to lineage
    lineage <- c(lineage, current_id)
    if (is.na(parent_id)) {
      # encountered root
      break
    }
    current_id <- parent_id
  }

  lineage
}

# Create runs_t with the base run, or add what an older database lacks: the seed column
# and the base run itself. Never upserts the base run, which would reset its description
# and seed each time the database is opened.
ensure_base_run <- function(self) {
  if (!"runs_t" %in% self$list_tables()) {
    return(self$commit(as_runs_t(), "runs_t", method = "overwrite"))
  }

  columns <- self$get_query(
    "select column_name from (describe dl_db.runs_t)",
    as_atomic = TRUE
  )
  if (!"seed" %in% columns) {
    self$execute("alter table dl_db.runs_t add column seed integer")
  }

  has_base <- self$get_query(
    "select exists (select 1 from dl_db.runs_t where id_run = 0)",
    as_atomic = TRUE
  )
  if (!has_base) {
    self$commit(as_runs_t(), "runs_t", method = "upsert")
  }

  invisible(NULL)
}

# The seed registered for `id_run` in runs_t, NA if it has none
get_run_seed <- function(self, id_run = self$id_run) {
  seed <- self$get_query(
    "select seed from {self$get_read_expr('runs_t')} where id_run = {id_run}",
    as_atomic = TRUE
  )
  if (length(seed) == 0L) NA_integer_ else as.integer(seed)
}

# Evaluate `expr` with R's random number generator seeded from the active run's seed, and
# restore the generator's previous state afterwards, so that seeding one run does not fix
# the draws of whatever the caller does next. Without a seed, `expr` draws from the current
# state, as before the seed column existed.
with_run_seed <- function(self, expr) {
  seed <- get_run_seed(self)
  if (is.na(seed)) {
    return(expr)
  }

  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  previous_seed <- if (had_seed) get(".Random.seed", envir = globalenv())
  on.exit(
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = globalenv())
    } else {
      rm(".Random.seed", envir = globalenv())
    },
    add = TRUE
  )

  set.seed(seed)
  expr
}
