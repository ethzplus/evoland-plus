#' Run parallel tasks on cloned `evoland_db` instances
#'
#' This wrapper takes an (optional) worker cluster, a worker function, a set of items,
#' and a parent `evoland_db` instance. If **no** cluster is passed, apply the worker
#' serially. **If** a cluster is passed, each worker gets its own database instance
#' derived from the parent database.
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
      update_reporting = FALSE
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
    id_run = parent_db$get_active_run(),
    ...
  )
}
