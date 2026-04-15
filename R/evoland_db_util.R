#' Utility functions and methods for `evoland_db`
#'
#' Factored out utility functions and methods specifically used in conjunction with
#' `evoland_db` and its subclasses.
#'
#' @name evoland_db_util
NULL

#' @describeIn evoland_db_util Gets the read expression for a given table, taking into
#' account the run lineage and distinctness columns. If the table has no `id_run` column
#' or is the `runs_t` table itself, falls back to the default read expression.
#' Otherwise, constructs a read expression that filters for the active `id_run` and its lineage,
#' using the distinctness columns to determine which slices of data to include.
#' @param self The `evoland_db` instance
#' @param table_name The name of the table to read from
#' @return A SQL expression string to read most specific data for slice
get_evoland_db_read_expr <- function(self, super, table_name) {
  table_path <- super$get_table_path(table_name)
  all_cols <- self$get_query(
    glue::glue("select column_name from (describe '{table_path}')")
  )[[1]]

  if (
    is.null(self$id_run) || # no active id_run
      !("id_run" %in% all_cols) || # no id_run in table
      table_name == "runs_t" # runs_t table itself
  ) {
    return(super$get_read_expr(table_name))
  }

  # Hardcode the set of columns to probe for distinctness; i.e. if any value is present
  # for a given distinctness tuple, the corresponding slice of data will be used. This
  # is a tradeoff: we could also choose to fall through on each coordinate point, but
  # that would blow up the query time.
  theoretical_distinctness_cols <- c("id_run", "id_period", "id_pred")
  distinctness_cols <- intersect(all_cols, theoretical_distinctness_cols)
  inheritance_key_cols <- setdiff(distinctness_cols, "id_run")

  # Single run in lineage: just filter for active id_run
  if (length(self$run_lineage) == 1L) {
    return(glue::glue("(select * from '{table_path}' where id_run = {self$id_run})"))
  }

  # map each id_run in lineage to its distance from the active run; used to
  # find the minimum distance. e.g. if run_lineage is (3, 2, 0) we get
  # case id_run when 3 then 1 when 2 then 2 when 0 then 3 else 999999 end
  run_case <- glue::glue(
    "case b.id_run ",
    paste(
      glue::glue("when {self$run_lineage} then {seq_along(self$run_lineage)}"),
      collapse = " "
    ),
    " else 999999 end"
  )

  ctes <- list()

  # return one row per tuple of available data within lineage
  ctes[["data_present"]] <- glue::glue(
    r"[
    select distinct
      {cols_to_select_expr(distinctness_cols)}
    from
      '{table_path}'
    where
      id_run in ({toString(self$run_lineage)})
    ]"
  )

  # Special case for id_period: need self-join on id_period AND id_period=0;
  # currently only relevant for pred_data_t, but let's generalize just in case
  if ("id_period" %in% inheritance_key_cols) {
    join_conditions <- vapply(
      inheritance_key_cols,
      function(col) {
        if (col == "id_period") {
          # special case for id_period: self-join data_present to allow for
          # id_period=0 fallback
          return(glue::glue("(a.{col} = b.{col} or b.{col} = 0)"))
        }
        glue::glue("a.{col} = b.{col}")
      },
      character(1)
    )
    # reduce data_present to one row per most specific id_run
    ctes[["best_run"]] <- glue::glue(
      r"[
      select
        {paste0("b.", inheritance_key_cols, collapse = ", ")},
        -- arg_min returns id_run for the single row where run_case is minimal
        arg_min(b.id_run, {run_case}) as id_run
      from
        data_present a,
        data_present b
      where
        {paste(join_conditions, collapse = " and ")}
      group by
        {paste0("b.", inheritance_key_cols, collapse = ", ")}
      ]"
    )
  } else {
    # general case: just find minimum distance id_run for each tuple of distinctness cols
    ctes[["best_run"]] <- glue::glue(
      r"[
      select
        {paste0("b.", distinctness_cols, collapse = ", ")},
        arg_min(b.id_run, {run_case}) as id_run
      from
        data_present b
      group by
        {paste0("b.", distinctness_cols, collapse = ", ")}
      ]"
    )
  }

  # return read expression: use semi join to filter table_path using best_run
  glue::glue(
    r"[(
    with
      data_present as (
        {ctes[["data_present"]]}
      ),
      best_run as (
        {ctes[["best_run"]]}
      )
    from
      '{table_path}' c
    semi join
      best_run b
      using ({cols_to_select_expr(distinctness_cols)})
    where
      c.id_run in ({toString(self$run_lineage)})
    )]"
  )
}

#' @describeIn evoland_db_util Run a worker function in parallel with an `evoland_db`
#' instance; takes an (optional) worker cluster, a worker function, a set of
#' items, and a parent `evoland_db` instance. If **no** cluster is passed, apply the
#' worker serially. **If** a cluster is passed, each worker gets its own database
#' instance derived from the parent database.
#'
#' @param worker_fun A function to apply to each item, with signature `worker_fun(item,
#' db, ...)`. The `db` argument will be a new `evoland_db` instance for each worker.
#' @param items A list of items to iterate over
#' @param parent_db An [evoland_db] instance; used for its `path` and `id_run`; or
#' passed directly to the worker for serial case.
#' @param cluster An optional cluster object created by [parallel::makeCluster()] or
#' [mirai::make_cluster()].
#' @param ... Additional arguments passed to `worker_fun`.
#'
#' @return A list of results
#' @export
run_parallel_evoland <- function(
  items,
  worker_fun,
  parent_db,
  cluster = NULL,
  ...
) {
  if (is.null(cluster)) {
    return(
      lapply(
        X = items,
        FUN = worker_fun,
        db = parent_db,
        ...
      )
    )
  }

  # Wrapper function to manage DB connection inside the worker
  wrapper <- function(item, worker_fun_inner, db_path, id_run, ...) {
    if (!exists("evoland_db")) {
      stop("evoland_db class not found on worker. Ensure package is installed.")
    }

    worker_db <- evoland_db$new(
      path = db_path,
      id_run = id_run,
      read_only = TRUE
    )
    worker_db$execute("set threads to 1")

    # Call the actual worker function
    worker_fun_inner(item = item, db = worker_db, ...)
  }

  # Run parallel lapply
  parallel::parLapply(
    cl = cluster,
    X = items,
    fun = wrapper,
    worker_fun_inner = worker_fun,
    db_path = parent_db$path,
    id_run = parent_db$id_run,
    ...
  )
}
