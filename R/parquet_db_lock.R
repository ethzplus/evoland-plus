#' Cross-process table locking for [parquet_db]
#'
#' @description
#' A minimal, dependency-free advisory lock used to serialise the
#' read-modify-write cycles that [parquet_db] performs on a table's parquet
#' file(s). Upserts (and non-partitioned appends and deletes) read the existing
#' data, merge, and rewrite; without a lock two processes doing that
#' concurrently silently lose each other's rows.
#'
#' The lock is a directory next to the table file, e.g.
#' `<db path>/.trans_pot_t.lock`. `dir.create()` maps onto a single
#' `mkdir(2)` (`CreateDirectory()` on Windows), so exactly one process can win
#' the race, and it does not rely on advisory file locking, which network file
#' systems implement unevenly.
#'
#' Locks are per table and per process:
#'
#' - Waiters poll with a jittered backoff until the lock is free or
#'   `timeout` seconds have passed, at which point they error.
#' - Acquisition is reentrant. A lock already held by the calling process only
#'   bumps a counter, so nesting (e.g. `$commit()` inside a
#'   `$with_table_lock()` block) does not deadlock.
#' - A process that dies while holding a lock leaves the directory behind. On
#'   Linux the recorded owner is checked via `/proc` and a lock whose owner is
#'   gone is reclaimed; elsewhere the timeout message points at the directory
#'   to remove.
#'
#' @name parquet_db_lock
#' @keywords internal
NULL

# Locks held by this process, keyed by lock path, with a depth counter to make
# acquisition reentrant.
lock_registry <- new.env(parent = emptyenv())

#' @describeIn parquet_db_lock Path of the lock directory guarding `table_name`
#' @param db_path Character path to the database folder
#' @param table_name Character table name
#' @keywords internal
table_lock_path <- function(db_path, table_name) {
  # normalise so that the same table reached by different relative paths maps
  # onto the same registry key within a process
  file.path(
    normalizePath(db_path, mustWork = FALSE),
    paste0(".", table_name, ".lock")
  )
}

#' @describeIn parquet_db_lock Block until the lock at `lock_path` is held by
#' this process. Returns `TRUE` if the lock was newly taken, `FALSE` if this
#' process already held it (reentrant acquisition).
#' @param lock_path Character path of the lock directory, see [table_lock_path()]
#' @param timeout Numeric seconds to wait before erroring out
#' @param poll_interval Numeric seconds between the first retries; grows up to
#' one second as waiting continues
#' @keywords internal
acquire_table_lock <- function(
  lock_path,
  timeout = 3600,
  poll_interval = 0.05
) {
  depth <- get0(lock_path, envir = lock_registry, ifnotfound = 0L)
  if (depth > 0L) {
    assign(lock_path, depth + 1L, envir = lock_registry)
    return(invisible(FALSE))
  }

  deadline <- Sys.time() + timeout
  wait <- poll_interval

  repeat {
    if (dir.create(lock_path, showWarnings = FALSE, recursive = FALSE)) {
      write_lock_owner(lock_path)
      assign(lock_path, 1L, envir = lock_registry)
      return(invisible(TRUE))
    }

    if (break_dead_lock(lock_path)) {
      next
    }

    if (Sys.time() > deadline) {
      stop(glue::glue(
        "Timed out after {timeout}s waiting for lock '{lock_path}'\n",
        "  Held by: {format_lock_owner(lock_path)}\n",
        "  If that process is gone, delete the lock directory to recover."
      ))
    }

    # jitter keeps a crowd of waiters from retrying in lockstep
    Sys.sleep(stats::runif(1L, wait / 2, wait))
    wait <- min(wait * 1.5, 1)
  }
}

#' @describeIn parquet_db_lock Release a lock held by this process. Returns
#' `TRUE` if the lock directory was removed, `FALSE` if an outer acquisition
#' still holds it or this process never held it.
#' @keywords internal
release_table_lock <- function(lock_path) {
  depth <- get0(lock_path, envir = lock_registry, ifnotfound = 0L)

  if (depth == 0L) {
    # never ours to release; removing it would break the actual owner's lock
    return(invisible(FALSE))
  }
  if (depth > 1L) {
    assign(lock_path, depth - 1L, envir = lock_registry)
    return(invisible(FALSE))
  }

  rm(list = lock_path, envir = lock_registry)
  unlink(lock_path, recursive = TRUE)
  invisible(TRUE)
}

# record who holds the lock, so waiters can report it and detect a dead owner
write_lock_owner <- function(lock_path) {
  try(
    writeLines(
      c(
        paste0("pid: ", Sys.getpid()),
        paste0("host: ", Sys.info()[["nodename"]]),
        paste0("time: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
      ),
      file.path(lock_path, "owner")
    ),
    silent = TRUE
  )
  invisible(NULL)
}

# named list of the owner fields, or NULL if not (yet) readable
read_lock_owner <- function(lock_path) {
  owner_file <- file.path(lock_path, "owner")
  if (!file.exists(owner_file)) {
    return(NULL)
  }

  lines <- tryCatch(
    readLines(owner_file, warn = FALSE),
    error = function(e) character(0)
  )
  fields <- regmatches(lines, regexpr(": ", lines), invert = TRUE)
  fields <- Filter(function(f) length(f) == 2L, fields)
  if (length(fields) == 0L) {
    return(NULL)
  }

  out <- lapply(fields, `[[`, 2L)
  names(out) <- vapply(fields, `[[`, character(1), 1L)
  out
}

format_lock_owner <- function(lock_path) {
  owner <- read_lock_owner(lock_path)
  if (is.null(owner)) {
    return("unknown")
  }
  toString(paste(names(owner), unlist(owner), sep = "="))
}

# A process that is killed while holding a lock cannot clean up after itself.
# On Linux the owner's liveness is readable from /proc, so such a lock can be
# reclaimed instead of stalling every other worker until the timeout. Returns
# TRUE if a dead owner's lock was removed.
break_dead_lock <- function(lock_path, grace = 10) {
  if (!dir.exists("/proc")) {
    return(FALSE)
  }

  owner <- read_lock_owner(lock_path)
  if (is.null(owner) || !identical(owner[["host"]], Sys.info()[["nodename"]])) {
    return(FALSE)
  }

  # only question a lock that has been sitting still for a while: the owner
  # file is written just after the directory appears, and PIDs get reused
  age <- difftime(
    Sys.time(),
    file.mtime(file.path(lock_path, "owner")),
    units = "secs"
  )
  if (is.na(age) || age < grace) {
    return(FALSE)
  }

  if (dir.exists(file.path("/proc", owner[["pid"]]))) {
    return(FALSE)
  }

  warning(glue::glue(
    "Reclaiming lock '{lock_path}' from dead process {owner[['pid']]}"
  ))
  unlink(lock_path, recursive = TRUE)
  TRUE
}
