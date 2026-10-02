#' Create Runs Table
#'
#' Creates a runs_t table that stores the identities, hierarchy, and description of
#' runs. Defaults to a table with one row with the base `id_run := 0`. Runs are a
#' feature that allows one to label data that is either overriding base data (e.g.
#' predictors being used for forecasting) or produced by a specific model experiment
#' (e.g. a given allocation of land use change given a set of transition potential
#' models and allocation parameters).
#'
#' Runs form a tree: every run except the base names a parent, and reading any table
#' under a run resolves each data slice to the nearest ancestor that has it. A run
#' therefore only stores what it changes.
#'
#' Besides the tree, `runs_t` carries a fixed set of columns that experiments need
#' again and again (seeds, the role of a run, its index in an ensemble, provenance),
#' and a map of free-form `attributes` for anything specific to one study, such as a
#' scenario label. Further ad hoc columns are kept but not validated; prefer
#' `attributes`, which keeps the schema fixed so that runs can be upserted, and which
#' [run_attributes_v()] resolves along the lineage.
#'
#' @name runs_t
#'
#' @param x A list or data.frame coercible to a data.table. Standard columns that are
#'   missing are added as `NA`.
#'
#' @return A data.table of class "runs_t" with columns:
#'   - `id_run`: Unique ID of the run
#'   - `parent_id_run`: ID of the parent run; `NA` for the base run
#'   - `description`: Free text
#'   - `kind`: Role of the run. Suggested values: `"base"`, `"scenario"`,
#'     `"parameters"`, `"reference"` (e.g. held-out observations), `"realisation"`
#'     (a member of a stochastic ensemble). Free text; not validated.
#'   - `member`: Index of the run within its parent's ensemble, `>= 1`, or `NA`
#'   - `seed`: Integer seed for the run's stochastic steps, or `NA`. The CLUMPY
#'     allocators seed R's random number generator from it, see [alloc_clumpy()].
#'     Dinamica EGO allocates with its own generator, which this seed does not control.
#'   - `created_by`: Who or what registered the run, e.g. the experiment step; `NA` if
#'     unknown. [add_runs()] fills it from the option `evoland.created_by`.
#'   - `created_at`: When the run was registered (UTC)
#'   - `evoland_version`: evoland version that registered the run
#'   - `attributes`: MAP of further run attributes (string keys and values), inherited
#'     along the lineage, see [run_attributes_v()]
#' @export
as_runs_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_run = 0L,
      parent_id_run = NA_integer_,
      description = "Base",
      kind = "base"
    )
  }

  data.table::setDT(x)
  add_missing_runs_cols(x)

  x |>
    cast_dt_col("id_run", "int") |>
    cast_dt_col("parent_id_run", "int") |>
    cast_dt_col("description", "char") |>
    cast_dt_col("kind", "char") |>
    cast_dt_col("member", "int") |>
    cast_dt_col("seed", "int") |>
    cast_dt_col("created_by", "char") |>
    cast_dt_col("evoland_version", "char")
  if (!inherits(x[["created_at"]], "POSIXct")) {
    data.table::set(
      x,
      j = "created_at",
      value = as.POSIXct(x[["created_at"]], tz = "UTC")
    )
  }

  as_ducklake_db_t(
    x,
    class_name = "runs_t",
    key_cols = "id_run",
    map_cols = "attributes"
  )
}

# The standard columns and their missing values, in schema order
runs_t_standard_cols <- function() {
  list(
    id_run = NA_integer_,
    parent_id_run = NA_integer_,
    description = NA_character_,
    kind = NA_character_,
    member = NA_integer_,
    seed = NA_integer_,
    created_by = NA_character_,
    created_at = as.POSIXct(NA, tz = "UTC"),
    evoland_version = NA_character_,
    attributes = list()
  )
}

# Add any missing standard column, by reference
add_missing_runs_cols <- function(x) {
  std <- runs_t_standard_cols()
  for (col in setdiff(names(std), names(x))) {
    # a list column has to be wrapped once more, or set() reads it as one value per column
    value <- if (col == "attributes") {
      list(rep(list(NULL), nrow(x)))
    } else {
      rep(std[[col]], nrow(x))
    }
    data.table::set(x, j = col, value = value)
  }
  invisible(x)
}

#' @export
validate.runs_t <- function(x, ...) {
  NextMethod()

  std_cols <- names(runs_t_standard_cols())
  data.table::setcolorder(x, c(std_cols, setdiff(names(x), std_cols)))

  member <- x[["member"]]
  stopifnot(
    "id_run is not integer" = is.integer(x[["id_run"]]),
    "parent_id_run is not integer" = is.integer(x[["parent_id_run"]]),
    "all parent_id_run must be in id_run or NA" = all(
      is.na(x[["parent_id_run"]]) | x[["parent_id_run"]] %in% x[["id_run"]]
    ),
    "no base (0) id_run" = 0L %in% x[["id_run"]],
    "kind is not character" = is.character(x[["kind"]]),
    "member is not integer" = is.integer(member),
    "member must be >= 1 or NA" = all(is.na(member) | member >= 1L),
    "seed is not integer" = is.integer(x[["seed"]]),
    "created_at is not POSIXct" = inherits(x[["created_at"]], "POSIXct"),
    "attributes is not a list column" = is.list(x[["attributes"]])
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
    "id_run must be scalar integerish or NULL" = {
      length(y) == 1L && as.integer(y) == y
    }
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

#' Register runs
#'
#' Adds runs below existing parents, with ids allocated by the database rather than by
#' the caller, and fills in the provenance columns. Allocation happens in one
#' transaction, so concurrent processes registering runs on the same database cannot
#' receive the same ids.
#'
#' All arguments except `self` are recycled to a common length, one element per new
#' run.
#'
#' Ids continue from the highest `id_run` in the table. Do not mix allocated ids with
#' ids picked by hand in the same database: a run committed later under a hand-picked id
#' can collide with one allocated here.
#'
#' @param self An [evoland_db] instance.
#' @param parent_id_run Integer, the parent of each new run; must exist in `runs_t`.
#' @param description Character, free text.
#' @param kind Character, the role of each run; see [runs_t] for suggested values.
#' @param member Integer, index within the parent's ensemble, or `NA`.
#' @param seed Integer, seed for the run's stochastic steps, or `NA`.
#' @param attributes `NULL`, a named list (the same attributes for every run), or a list
#'   of named lists (one per run). Values are stored as strings.
#' @param created_by Character, who or what registers the runs; defaults to the option
#'   `evoland.created_by`, which an experiment step can set once at its top.
#' @return The new rows as a [runs_t] object, in the order given (ids are allocated in
#'   that order).
#' @name add_runs
add_runs <- function(
  self,
  parent_id_run,
  description,
  kind = NA_character_,
  member = NA_integer_,
  seed = NA_integer_,
  attributes = NULL,
  created_by = getOption("evoland.created_by", NA_character_)
) {
  if (length(attributes) == 0L || !is.null(names(attributes))) {
    # NULL, or one named list shared by every run
    attributes <- list(attributes)
  }
  n <- max(
    length(parent_id_run),
    length(description),
    length(kind),
    length(member),
    length(seed),
    length(attributes)
  )
  recycle <- function(v) {
    stopifnot(
      "arguments must have length 1 or a common length" = length(v) %in% c(1L, n)
    )
    rep_len(v, n)
  }

  existing <- self$runs_t
  stopifnot(
    "every parent_id_run must exist in runs_t" = all(parent_id_run %in% existing$id_run)
  )

  new_runs <- NULL
  self$transaction({
    ids <- self$next_id("runs_t", "id_run", n = n)
    new_runs <- as_runs_t(rbind(
      existing,
      data.table::data.table(
        id_run = ids,
        parent_id_run = recycle(as.integer(parent_id_run)),
        description = recycle(as.character(description)),
        kind = recycle(as.character(kind)),
        member = recycle(as.integer(member)),
        seed = recycle(as.integer(seed)),
        created_by = recycle(as.character(created_by)),
        created_at = as.POSIXct(Sys.time(), tz = "UTC"),
        evoland_version = as.character(utils::packageVersion("evoland")),
        attributes = recycle(attributes)
      ),
      fill = TRUE
    ))[id_run %in% ids]
    self$commit(new_runs, "runs_t", method = "upsert")
  })
  new_runs
}

#' Run attributes, resolved along the lineage
#'
#' Each run inherits the attributes of its ancestors; where an attribute is set on
#' more than one run of a lineage, the nearest one wins, the same rule that resolves
#' data reads. A scenario label set once on a scenario run therefore applies to every
#' run below it, and realisations need not copy it.
#'
#' @param self An [evoland_db] instance.
#' @param wide Logical; `TRUE` (default) returns one row per run and one character
#'   column per attribute key, `FALSE` a long table with the run the value came from.
#' @return With `wide = TRUE`, a data.table with `id_run` and one column per attribute
#'   key (`NA` where a run has no value). With `wide = FALSE`, a data.table with
#'   `id_run`, `key`, `value` and `from_id_run`.
#' @name run_attributes_v
run_attributes_v <- function(self, wide = TRUE) {
  runs <- self$runs_t
  own <- data.table::rbindlist(lapply(seq_len(nrow(runs)), function(i) {
    a <- runs$attributes[[i]]
    if (length(a) == 0L) {
      return(NULL)
    }
    # not data.table(key = ...): `key` is data.table()'s argument for setting a sort key
    data.table::setDT(list(
      from_id_run = rep(runs$id_run[i], length(a)),
      key = names(a),
      value = as.character(unlist(a, use.names = FALSE))
    ))
  }))
  if (nrow(own) == 0L) {
    own <- data.table::data.table(
      from_id_run = integer(0),
      key = character(0),
      value = character(0)
    )
  }

  lineages <- data.table::rbindlist(lapply(runs$id_run, function(id) {
    lin <- get_lineage(runs, id)
    data.table::data.table(id_run = id, from_id_run = lin, depth = seq_along(lin))
  }))
  resolved <- own[lineages, on = "from_id_run", nomatch = NULL, allow.cartesian = TRUE][
    order(id_run, key, depth)
  ][, .SD[1L], by = .(id_run, key)][, .(id_run, key, value, from_id_run)]

  if (!wide) {
    return(resolved[])
  }
  out <- data.table::data.table(id_run = runs$id_run)
  if (nrow(resolved) > 0L) {
    out <- merge(
      out,
      data.table::dcast(resolved, id_run ~ key, value.var = "value"),
      by = "id_run",
      all.x = TRUE
    )
  }
  data.table::setkeyv(out, "id_run")
  out[]
}

# Seed for one period of a run: a draw from the stream of the run's own seed, so that
# runs with consecutive seeds do not share streams across periods (seed + period would
# give run s at period p + 1 the stream of run s + 1 at period p).
run_period_seed <- function(seed, id_period) {
  stopifnot(
    "seed must be a single integer" = length(seed) == 1L && !is.na(seed),
    "id_period must be a single positive integer" = length(id_period) == 1L &&
      id_period >= 1L
  )
  restore <- preserve_rng_state()
  on.exit(restore())
  set.seed(seed)
  sample.int(.Machine$integer.max, as.integer(id_period))[[id_period]]
}

# Snapshot R's RNG state; the returned function restores it (or removes the seed if
# none existed before).
preserve_rng_state <- function() {
  env <- globalenv()
  had_seed <- exists(".Random.seed", envir = env, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = env, inherits = FALSE)
  function() {
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = env)
    } else if (exists(".Random.seed", envir = env, inherits = FALSE)) {
      rm(".Random.seed", envir = env)
    }
  }
}

# Seed R's RNG for one allocation period from the active run's own seed (not
# inherited: a seed belongs to one realisation). Returns a function that restores the
# caller's RNG state, or NULL if the run has no seed.
seed_rng_from_active_run <- function(db, id_period_post) {
  seed <- db$runs_t[id_run == db$id_run, seed]
  if (length(seed) != 1L || is.na(seed)) {
    return(NULL)
  }
  period_seed <- run_period_seed(seed, id_period_post)
  restore <- preserve_rng_state()
  set.seed(period_seed)
  restore
}

# Bring an existing runs_t up to the standard schema. Older databases carry only
# id_run, parent_id_run and description, plus whatever columns experiments added;
# upserts into them fail as soon as a new standard column is committed. Rewrites the
# (small) table once, keeping every row and every ad hoc column.
migrate_runs_t <- function(self) {
  if (!"runs_t" %in% self$list_tables()) {
    return(invisible(FALSE))
  }
  stored_cols <- self$get_query(
    "select column_name from (describe dl_db.runs_t)",
    as_atomic = TRUE
  )
  if (all(names(runs_t_standard_cols()) %in% stored_cols)) {
    return(invisible(FALSE))
  }
  runs <- as_runs_t(self$fetch("runs_t"))
  self$commit(runs, "runs_t", method = "overwrite")
  invisible(TRUE)
}
