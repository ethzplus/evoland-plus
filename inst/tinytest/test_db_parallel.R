# Setup temporary DB
library(tinytest)
temp_dir <- tempfile()
db <- evoland_db$new(
  temp_dir,
  reporting_nonce = "vorpal sword"
)

# 1. Test Serial Execution
expect_silent(
  res <- run_parallel_evoland(
    items = list(1, 2, 3),
    worker_fun = function(item, db) {
      paste(db$path, item * 2)
    },
    parent_db = db
  )
)
expect_equal(res, as.list(paste(temp_dir, c(2, 4, 6))))

# 2. Test Parallel Execution (Requires installed package for workers)
# In development (pkgload load_all()), workers can't load the package via library()
# so we check if we can run a minimal task before proceeding.
if (!at_home() || !requireNamespace("evoland", quietly = TRUE)) {
  message(
    "\n  Skipping parallel tests: workers could not initialize (package likely not installed)"
  )
  tinytest::exit_file()
}

worker_db_check <- function(item, db) {
  db$reporting_t
}

# Each worker commits its own row; the trans_pot_t-style upsert is only
# parallel-safe because $commit() serialises on the table lock.
worker_db_write <- function(item, db) {
  db$commit(
    as_parquet_db_t(
      data.table::data.table(someitem = as.integer(item), payload = paste0("w", item)),
      key_cols = "someitem"
    ),
    "test_table",
    "upsert"
  )
  TRUE
}

can_run_parallel <- FALSE
tryCatch(
  {
    c <- parallel::makeCluster(2)

    res_parallel <- run_parallel_evoland(
      items = list(1, 2),
      worker_fun = worker_db_check,
      parent_db = db,
      cluster = c
    )
    can_run_parallel <- TRUE
  },
  error = function(e) {
    message(
      "\n  Skipping parallel tests: workers could not initialize (",
      conditionMessage(e),
      ")"
    )
  },
  warning = function(w) {
    message(
      "\n  Warning during parallel tests: ",
      conditionMessage(w)
    )
  },
  finally = {
    parallel::stopCluster(c)
  }
)


if (can_run_parallel) {
  # should be exactly the same
  expect_equal(db$reporting_t, res_parallel[[1]])
  expect_equal(db$reporting_t, res_parallel[[2]])

  c <- parallel::makeCluster(2)

  # workers are read-only unless asked for otherwise
  expect_error(
    run_parallel_evoland(
      items = list(1, 2),
      worker_fun = worker_db_write,
      parent_db = db,
      cluster = c
    ),
    "!self\\$read_only is not TRUE"
  )
  expect_equal(db$row_count("test_table"), 0L)

  # ... and their writes do not drop each other's rows once they are
  db$commit(
    as_parquet_db_t(
      data.table::data.table(someitem = 0L, payload = "w0"),
      key_cols = "someitem"
    ),
    "test_table",
    "overwrite"
  )
  run_parallel_evoland(
    items = as.list(1:6),
    worker_fun = worker_db_write,
    parent_db = db,
    cluster = c,
    worker_writable = TRUE
  )
  parallel::stopCluster(c)

  expect_equal(sort(db$fetch("test_table")[["someitem"]]), 0L:6L)
  expect_false(dir.exists(evoland:::table_lock_path(db$path, "test_table")))
}

# Cleanup
unlink(temp_dir, recursive = TRUE)
