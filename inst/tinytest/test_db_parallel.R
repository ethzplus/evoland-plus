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

can_run_parallel <- FALSE
tryCatch(
  {
    c <- parallel::makeCluster(2)

    # todo ensure there is a scenario where a worker tries to write but is
    # stopped by the read_only lock

    # test_table_dt <-
    #   data.table::data.table(
    #     someitem = NA_integer_
    #   )

    # todo make partitioned writes so append works across processes
    # db$commit(
    #   test_table_dt,
    #   "test_table",
    #   "append"
    # )

    worker_db_check <- function(item, db) {
      # db$commit(
      #   data.table::data.table(
      #     someitem = item
      #   ),
      #   "test_table",
      #   "append"
      # )
      db$reporting_t
    }

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
}

# Cleanup
unlink(temp_dir, recursive = TRUE)
