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

# test that we can overwrite a slice of data within a run
expect_silent(db$id_run <- 0L)
pred_run_0 <- db$pred_data_t

expect_silent(db$id_run <- 2L)
expect_equal(db$id_run, 2L)
expect_equal(db$run_lineage, 2:0)

db$pred_data_t <- added_run_2 <- db$pred_data_t[
  id_pred == 1L,
  .(id_run = 2L, id_period, id_pred, id_coord, value = value + 100L)
]

pred_run_2 <- db$pred_data_t

expect_equal(nrow(pred_run_0), nrow(pred_run_2))

# cannot check equality because of weird class/attribute changes due to
# data.table operations, but we can check that the added rows in run 2 are
# exactly those not in run 0
expect_equivalent(
  added_run_2,
  pred_run_2[
    !pred_run_0, # anti-join to find rows in run 2 not in run 0
    on = c("id_run", "id_period", "id_pred", "id_coord")
  ]
)

# fetch back as rast
expect_equal(
  db$lulc_data_as_rast()["id_period_1_id_run_0"],
  m <- db$lulc_data_as_rast(id_period = 1L)
)
expect_length(as.vector(m["id_period_1"]), 900L)
expect_equal(
  unique(db$lulc_data_t$id_lulc),
  unique(as.vector(m["id_period_1"]))
)

# add predictor via sugar add_predictor()
somethingelse_data <- data.table::data.table(
  id_coord = db$coords_minimal[, id_coord],
  id_period = 1L,
  value = factor(
    sample(letters[1:5], size = nrow(db$coords_minimal), replace = TRUE),
    levels = letters[1:5]
  )
)

db$add_predictor(
  pred_data_raw = somethingelse_data,
  name = "somethingelse",
  fill_value = "a",
  unit = "letters"
)

expect_equivalent(
  as.list(db$pred_meta_t[name == "somethingelse"]),
  list(
    id_pred = 11L,
    name = "somethingelse",
    pretty_name = "somethingelse",
    description = NA_character_,
    orig_format = NA_character_,
    sources = list(data.table::data.table(url = character(), md5sum = character())),
    unit = "letters",
    factor_levels = list(letters[1:5]),
    data_type = factor("factor", levels = c("int", "float", "bool", "factor")),
    fill_value = "a"
  )
)

somethingelse_data_roundtrip <- db$pred_data_t[id_pred == 11L]
expect_equal(nrow(somethingelse_data_roundtrip), 900L)
expect_inherits(somethingelse_data_roundtrip$value, "numeric")
expect_length(unique(somethingelse_data_roundtrip$value), 5L)

# try adding predictor to DB without pred_meta_t
empty_db <- evoland_db$new(tempfile("empty_evolanddb_"))
empty_db$add_predictor(
  pred_data_raw = somethingelse_data,
  name = "somethingelse",
  fill_value = "b"
)
expect_equivalent(
  data.table::as.data.table(empty_db$pred_data_t),
  data.table::as.data.table(somethingelse_data[, .(
    id_run = 0L,
    id_period,
    id_pred = 1L,
    id_coord,
    value = as.numeric(value)
  )])
)
