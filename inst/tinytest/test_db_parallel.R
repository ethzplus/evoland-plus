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

# 3. Uncoordinated concurrent upserts into a single table.
# Each worker is its own process with its own connection and no knowledge of the
# others; the DuckLake catalog plus ducklake_db's retry wrapper have to get all of
# them through without losing rows or duplicating keys. Four writers is enough to
# lose rows without the retry -- even two do -- and the retry logic itself is
# covered deterministically in test_db_ducklake.R.
conc_dir <- tempfile("evoland_conc_")
conc_seed <- data.table::data.table(id_worker = 0L, value = 0)
data.table::setattr(conc_seed, "key_cols", "id_worker")
ducklake_db$new(conc_dir)$commit(conc_seed, "conc_t", method = "overwrite")

n_workers <- 4L
conc_cluster <- parallel::makeCluster(n_workers)

conc_results <- parallel::parLapply(
  cl = conc_cluster,
  X = seq_len(n_workers),
  fun = function(id_worker, path) {
    library(evoland)
    db <- ducklake_db$new(path = path)
    row <- data.table::data.table(id_worker = as.integer(id_worker), value = id_worker * 1.0)
    data.table::setattr(row, "key_cols", "id_worker")
    tryCatch(
      {
        db$commit(row, "conc_t", method = "upsert")
        "ok"
      },
      error = function(e) conditionMessage(e)
    )
  },
  path = conc_dir
)

parallel::stopCluster(conc_cluster)

expect_equal(unique(unlist(conc_results)), "ok")

conc_final <- ducklake_db$new(conc_dir)$fetch("conc_t")
# every writer's row survived exactly once
expect_equal(sort(conc_final[["id_worker"]]), 0:n_workers)
expect_equal(conc_final[["value"]][order(conc_final[["id_worker"]])], as.numeric(0:n_workers))

unlink(conc_dir, recursive = TRUE)
