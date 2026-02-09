# Setup temporary DB
require(tinytest)
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
can_run_parallel <- FALSE
if (requireNamespace("evoland", quietly = TRUE)) {
  tryCatch(
    {
      c <- parallel::makeCluster(2)
      # Try a minimal task to see if workers can initialize
      dummy_res <- run_parallel_evoland(
        items = list(1),
        worker_fun = function(x, db) x,
        parent_db = db,
        cluster = c
      )
      can_run_parallel <- TRUE
    },
    error = function(e) {
      parallel::stopCluster(c)
      message(
        "\n  Skipping parallel tests: workers could not initialize (package likely not installed)"
      )
    }
  )
}

if (can_run_parallel) {
  # Write something to DB to check read access in worker
  worker_db_check <- function(item, db) {
    # Verify we can access the DB instance and it points to the right place
    db$reporting_t
  }

  res_parallel <- run_parallel_evoland(
    items = items,
    worker_fun = worker_db_check,
    parent_db = db,
    cluster = c
  )

  # all three should return the same
  expect_equal(
    db$reporting_t,
    res_parallel[[1]],
    res_parallel[[2]]
  )
}

# Cleanup
unlink(temp_dir, recursive = TRUE)
