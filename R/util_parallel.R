#' Parallel Processing Utilities
#'
#' Utilities for parallel execution within the evoland package.
#'
#' @name util_parallel
#' @include evoland_db.R
NULL

#' Run a function in parallel over a list of items
#'
#' @description
#' A wrapper around [parallel::parLapply()] that handles `evoland_db` connection management.
#' It uses a PSOCK cluster (via [parallel::makeCluster()]) to support Windows and ensures a clean
#' environment for each worker.
#'
#' @param items A list or vector of items to iterate over.
#' @param worker_fun A function to apply to each item. Must accept `item` as the first argument
#'   and `db` (the database instance) as the second argument.
#' @param db The current [evoland_db] instance (typically `self`).
#' @param cores Integer. Number of cores to use.
#' @param ... Additional arguments passed to `worker_fun`.
#'
#' @return A list of results, same as [lapply()].
#' @export
run_parallel_task <- function(items, worker_fun, db, cores = 1L, ...) {
  stopifnot(inherits(db, "evoland_db"))

  # Capture the path to re-initialize DB in workers
  db_path <- db$path

  if (cores > 1L) {
    # Create PSOCK cluster (works on Windows and Unix)
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl))

    # Setup workers
    # Ensure the package is loaded so that evoland_db and other objects are available.
    parallel::clusterEvalQ(cl, {
      if (requireNamespace("evoland", quietly = TRUE)) {
        library(evoland)
      }
    })

    # Explicitly export evoland_db to workers to handle development scenarios (load_all)
    # where the package is not installed in the system library
    parallel::clusterExport(cl, "evoland_db", envir = topenv())

    # Wrapper function to manage DB connection inside the worker
    wrapper <- function(item, db_path, worker_fun_inner, ...) {
      # Initialize DB for this worker
      # We rely on evoland_db being available (either via library loading
      # or explicit export)
      if (!exists("evoland_db")) {
        stop("evoland_db class not found on worker. Ensure package is installed.")
      }

      worker_db <- evoland_db$new(db_path, update_reporting = FALSE)

      # Call the actual worker function
      worker_fun_inner(item, worker_db, ...)
    }

    # Run parallel lapply
    parallel::parLapply(
      cl = cl,
      X = items,
      fun = wrapper,
      db_path = db_path,
      worker_fun_inner = worker_fun,
      ...
    )
  } else {
    # Serial execution
    # Reuse existing DB connection for efficiency
    wrapper_serial <- function(item, ...) {
      worker_fun(item, db, ...)
    }

    lapply(
      X = items,
      FUN = wrapper_serial,
      ...
    )
  }
}
