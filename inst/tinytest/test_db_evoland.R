# Domain-specific functionality; generic parquet_db tests are in test_parquet_db.R
library(tinytest)

# evoland_db initialization with reporting
source(file.path(system.file("tinytest", package = "evoland"), "helper_testdb.R"))
expect_silent(db <- make_test_db())
expect_inherits(db, c("evoland_db", "parquet_db"))
expect_stdout(print(db), "Active Run: 0")
expect_identical(
  db$list_tables(),
  c(
    "alloc_params_t",
    "coords_t",
    "lulc_data_t",
    "lulc_meta_t",
    "neighbors_t",
    "periods_t",
    "pred_data_t",
    "pred_meta_t",
    "reporting_t",
    "runs_t",
    "trans_meta_t",
    "trans_preds_t"
  )
)
expect_equal(db$reporting_t["report_name", value], "evoland_scenario")

expect_message(
  db$trans_models_t <-
    db$fit_partial_models(
      fit_fun = fit_glm,
      gof_fun = gof_glm,
      seed = 1244244
    ) |>
    db$fit_full_models(
      gof_criterion = "cor"
    ),
  "Fitting full models for 2 transitions..."
)

# active bindings without tables
active_bindings <-
  Filter(
    function(nm) bindingIsActive(nm, db$.__enclos_env__$self),
    names(db)
  ) |>
  grep(pattern = ".*_t$", x = _, value = TRUE, invert = TRUE)

for (binding in active_bindings) {
  # check that it can be accessed without error and prints something
  expect_stdout(print(db[[binding]]))
}

for (binding in db$list_tables()) {
  # the name of the table binding and the class is the same
  expect_inherits(db[[binding]], binding)
}

db$runs_t <- as_runs_t(list(
  id_run = c(0L, 1L, 2L),
  parent_id_run = c(NA_integer_, 0L, 1L),
  description = c("Base", "Child", "Grandchild")
))

expect_silent(db$id_run <- 2L)
expect_equal(db$id_run, 2L)
expect_equal(db$run_lineage, 2:0)

# fetch back as rast
expect_equal(
  db$lulc_data_as_rast()["id_period_1_id_run_0"],
  m <- db$lulc_data_as_rast(id_period = 1L)
)
expect_length(as.vector(m["id_period_1"]), 900L)
