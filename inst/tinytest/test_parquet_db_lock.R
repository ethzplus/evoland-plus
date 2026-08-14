library(tinytest)

temp_dir <- tempfile("evoland_lock_")
db <- parquet_db$new(temp_dir)
lock_path <- evoland:::table_lock_path(temp_dir, "some_table")

# ---- acquire / release ----
expect_false(dir.exists(lock_path))
expect_true(evoland:::acquire_table_lock(lock_path))
expect_true(dir.exists(lock_path))

# owner file identifies this process, so waiters can report/reclaim it
owner <- evoland:::read_lock_owner(lock_path)
expect_equal(owner[["pid"]], as.character(Sys.getpid()))
expect_equal(owner[["host"]], Sys.info()[["nodename"]])

# ---- reentrant within a process: only the outermost release removes it ----
expect_false(evoland:::acquire_table_lock(lock_path))
expect_false(evoland:::release_table_lock(lock_path))
expect_true(dir.exists(lock_path))
expect_true(evoland:::release_table_lock(lock_path))
expect_false(dir.exists(lock_path))

# releasing a lock this process does not hold must not remove it
dir.create(lock_path)
expect_false(evoland:::release_table_lock(lock_path))
expect_true(dir.exists(lock_path))

# ---- timeout while another (live, remote) process holds the lock ----
writeLines(
  c("pid: 1", "host: some-other-machine", "time: 2020-01-01T00:00:00"),
  file.path(lock_path, "owner")
)
expect_error(
  evoland:::acquire_table_lock(lock_path, timeout = 0.2),
  "Timed out after 0.2s waiting for lock"
)
expect_error(
  evoland:::acquire_table_lock(lock_path, timeout = 0.2),
  "host=some-other-machine"
)
unlink(lock_path, recursive = TRUE)

# ---- $with_table_lock() ----
expect_equal(db$with_table_lock("some_table", 41 + 1), 42)
expect_false(dir.exists(lock_path)) # released on normal exit

expect_error(db$with_table_lock("some_table", stop("boom")), "boom")
expect_false(dir.exists(lock_path)) # released on error, too

# nesting a commit inside a held lock must not deadlock against itself
expect_silent(
  db$with_table_lock(
    "nested_t",
    db$commit(
      as_parquet_db_t(data.table::data.table(id = 1L), key_cols = "id"),
      "nested_t",
      "upsert"
    )
  )
)
expect_equal(db$row_count("nested_t"), 1L)
expect_false(dir.exists(evoland:::table_lock_path(temp_dir, "nested_t")))

# lock directories are not mistaken for tables
expect_false(any(grepl("lock", db$list_tables())))

# ---- reclaiming a lock whose owner died (Linux only) ----
if (dir.exists("/proc")) {
  dir.create(lock_path)

  # a live owner is never reclaimed, however old the lock
  evoland:::write_lock_owner(lock_path)
  expect_false(evoland:::break_dead_lock(lock_path, grace = 0))
  expect_true(dir.exists(lock_path))

  # ... nor is a fresh lock questioned, even with an owner that cannot exist
  dead_pid <- readLines("/proc/sys/kernel/pid_max", warn = FALSE)[1] # never a live pid
  writeLines(
    c(
      paste0("pid: ", dead_pid),
      paste0("host: ", Sys.info()[["nodename"]]),
      "time: 2020-01-01T00:00:00"
    ),
    file.path(lock_path, "owner")
  )
  expect_false(evoland:::break_dead_lock(lock_path, grace = 60))
  expect_true(dir.exists(lock_path))

  # a settled lock held by a process that is gone is reclaimed
  expect_warning(
    reclaimed <- evoland:::break_dead_lock(lock_path, grace = 0),
    "Reclaiming lock"
  )
  expect_true(reclaimed)
  expect_false(dir.exists(lock_path))
}

# ---- concurrent upserts from uncoordinated processes ----
# Upsert is a read-modify-write over the whole table, so without the lock the
# writers would silently drop each other's rows. Forked workers are used here
# because they need no installed copy of the package.
if (!at_home() || .Platform$OS.type != "unix") {
  unlink(temp_dir, recursive = TRUE)
  exit_file("Concurrency test skipped (needs at_home() on unix)")
}

make_row <- function(i) {
  as_parquet_db_t(
    data.table::data.table(id = as.integer(i), payload = paste0("worker-", i)),
    key_cols = "id"
  )
}

db$commit(make_row(0L), "concurrent_t", "overwrite")

n_workers <- 8L
results <- parallel::mclapply(
  seq_len(n_workers),
  function(i) {
    # each worker is a fresh, uncoordinated database handle onto the same folder
    worker_db <- parquet_db$new(temp_dir)
    worker_db$commit(make_row(i), "concurrent_t", "upsert")
    TRUE
  },
  mc.cores = 4L
)

expect_true(all(vapply(results, isTRUE, logical(1))))

concurrent_t <- db$fetch("concurrent_t")[order(id)]
expect_equal(nrow(concurrent_t), n_workers + 1L)
expect_equal(concurrent_t$id, 0L:n_workers)
expect_equal(concurrent_t$payload, paste0("worker-", 0L:n_workers))

# upserting the same keys again is idempotent, concurrently as well
parallel::mclapply(
  seq_len(n_workers),
  function(i) parquet_db$new(temp_dir)$commit(make_row(i), "concurrent_t", "upsert"),
  mc.cores = 4L
)
expect_equal(db$row_count("concurrent_t"), n_workers + 1L)

# no lock left behind
expect_false(dir.exists(evoland:::table_lock_path(temp_dir, "concurrent_t")))

unlink(temp_dir, recursive = TRUE)
