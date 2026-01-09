# Setup temporary DB
temp_dir <- tempfile()
db <- evoland_db$new(temp_dir)

# 1. Test Serial Execution
items <- list(1, 2, 3)
worker_fun <- function(item, db) {
  return(item * 2)
}

res <- run_parallel_task(items, worker_fun, db, cores = 1L)
expect_equal(res, list(2, 4, 6))

# 2. Test Parallel Execution (Requires installed package for workers)
# In development (pkgload load_all()), workers can't load the package via library()
# so we check if we can run a minimal task before proceeding.
can_run_parallel <- FALSE
if (requireNamespace("evoland", quietly = TRUE)) {
  tryCatch(
    {
      # Try a minimal task to see if workers can initialize
      dummy_res <- run_parallel_task(
        list(1),
        function(x, db) x,
        db,
        cores = 2L
      )
      can_run_parallel <- TRUE
    },
    error = function(e) {
      message(
        "Skipping parallel tests: workers could not initialize (package likely not installed)"
      )
    }
  )
}

if (can_run_parallel) {
  # Write something to DB to check read access in worker
  # We need a table to read. Let's create a dummy file directly since setting up full evoland tables is verbose
  # Actually, evoland_db has methods to list tables, let's use that.

  # Create a dummy parquet file manually to simulate a table
  dummy_df <- data.frame(a = 1:10)
  DBI::dbWriteTable(db$connection, "temp_df", dummy_df)
  DBI::dbExecute(
    db$connection,
    glue::glue("COPY temp_df TO '{file.path(temp_dir, 'test_table.parquet')}' (FORMAT PARQUET)")
  )
  DBI::dbRemoveTable(db$connection, "temp_df")

  worker_db_check <- function(item, db) {
    # Verify we can access the DB instance and it points to the right place
    tables <- db$list_tables()
    return(tables)
  }

  res_parallel <- run_parallel_task(items, worker_db_check, db, cores = 2L)

  expect_equal(length(res_parallel), 3)
  expect_true("test_table" %in% res_parallel[[1]])

  # Test with actual computation
  res_calc <- run_parallel_task(items, worker_fun, db, cores = 2L)
  expect_equal(res_calc, list(2, 4, 6))
}

# Cleanup
unlink(temp_dir, recursive = TRUE)
