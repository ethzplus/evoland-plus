# Domain-specific functionality; generic ducklake_db tests are in test_db_ducklake.R
library(tinytest)

# evoland_db initialization with reporting
source(file.path(system.file("tinytest", package = "evoland"), "helper_testdb.R"))
expect_silent(db <- make_test_db())
expect_inherits(db, c("evoland_db", "ducklake_db"))
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

# create_alloc_params_t() returns one best estimate per viable transition, on the active run
db$id_run <- 1L
expect_equal(
  sort(db$alloc_params_t[["id_trans"]]),
  sort(db$trans_meta_t[is_viable == TRUE, id_trans])
)
db$id_run <- 0L

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
# check that the total number of rows across all pred_data_t is increased by added_run_2
expect_equal(
  nrow(pred_run_2) + nrow(added_run_2),
  db$row_count("pred_data_t")
)
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

# trans_pot_t is keyed per run: the grandchild's write must not replace run 0's rows, and
# each run reads back the nearest slice in its lineage
pots <- function(id_run, value) {
  as_trans_pot_t(data.table::data.table(
    id_run = id_run,
    id_trans = 1L,
    id_period_post = 2L,
    id_coord = 1:3,
    value = value
  ))
}
db$id_run <- 0L
db$trans_pot_t <- pots(0L, 0.5)
db$id_run <- 2L
db$trans_pot_t <- pots(2L, 0)
expect_equal(db$trans_pot_t[["value"]], c(0, 0, 0))
db$id_run <- 1L
expect_equal(db$trans_pot_t[["value"]], c(0.5, 0.5, 0.5))
db$id_run <- 2L

# fetch back as rast
expect_equal(
  db$lulc_data_as_rast()["id_run_0_id_period_1"],
  m <- db$lulc_data_as_rast(id_period = 1L)
)
expect_length(as.vector(m["id_run_0_id_period_1"]), 900L)
expect_equal(
  unique(db$lulc_data_t$id_lulc),
  unique(as.vector(m["id_run_0_id_period_1"]))
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

# pred_data_wide_v and trans_pred_data_v both should return timed (selected
# id_period) data, if it is available for that id_pred id_period slice.
# otherwise, fall back to static (id_period=0)

precedence_db <- make_test_db(include_neighbors = FALSE, include_trans_preds = TRUE)

expect_equal(
  nrow(precedence_db$pred_data_wide_v(
    id_trans = 1L,
    id_period_anterior = 1L
  )[is.na(id_pred_1)]),
  0L # there should not be any rows with missing id_pred_1 in fixture
)
expect_equal(
  nrow(precedence_db$trans_pred_data_v(
    id_trans = 1L,
    id_pred = 1:2
  )[is.na(id_pred_1)]),
  0L # there should not be any rows with missing id_pred_1 in fixture
)


n_lulc_ant <-
  precedence_db$lulc_data_t[
    id_period == 1L
  ][
    precedence_db$trans_meta_t,
    .(id_trans, id_lulc),
    on = c(id_lulc = "id_lulc_anterior")
  ][,
    .N,
    by = "id_trans"
  ]


# elevation (id_pred=1) is static-only in the fixture; we only overwrite it for
# one coordinate point in period 1. all other locations should come back NA.
precedence_db$pred_data_t <- as_pred_data_t(data.table::data.table(
  id_run = 0L,
  id_period = 1L,
  id_pred = 1L,
  id_coord = 333L, # a coordinate with id_lulc=1 at id_period=1
  value = -999
))

# get predictor data for the transition starting at id_lulc=1
expect_equal(
  precedence_db$pred_data_wide_v(
    id_trans = 1L,
    id_period_anterior = 1L
  )[
    is.na(id_pred_1),
    .N
  ],
  n_lulc_ant[id_trans == 1L, N] - 1L # all rows but 1 should be NA
)
expect_equal(
  precedence_db$trans_pred_data_v(
    id_trans = 1L,
    id_pred = 1L
  )[
    is.na(id_pred_1),
    .N
  ],
  n_lulc_ant[id_trans == 1L, N] - 1L # all rows but 1 should be NA
)

# get predictor data for the transition starting at id_lulc=2
expect_equal(
  precedence_db$pred_data_wide_v(
    id_trans = 2L,
    id_period_anterior = 1L
  )[
    is.na(id_pred_1),
    .N
  ],
  n_lulc_ant[id_trans == 2L, N] # all rows should be NA
)
expect_equal(
  precedence_db$trans_pred_data_v(
    id_trans = 2L,
    id_pred = 1L
  )[
    is.na(id_pred_1),
    .N
  ],
  n_lulc_ant[id_trans == 2L, N] # all rows should be NA
)

# figure_of_merit_v: run 10 holds the initial (period 1) and observed (period 2) maps, runs 11
# and 12 inherit the initial map and hold a simulated one. Run 11 has one cell of each kind:
# 1: 1 -> 2 simulated as 2 (hit)           2: 1 -> 2 simulated as 1 (miss)
# 3: 1 -> 1 simulated as 2 (false alarm)   4: 1 -> 1 simulated as 1 (correct persistence)
# 5: 2 -> 1 simulated as 3 (wrong hit)     6: 2 -> 2 simulated as 2 (correct persistence)
# Run 12 simulates the observed map exactly.
db$runs_t <- as_runs_t(list(
  id_run = c(0L, 10L:12L),
  parent_id_run = c(NA_integer_, 0L, 10L, 10L),
  description = c("Base", "Observed", "Simulated", "Simulated exactly")
))
fom_lulc <- function(id_run, id_period, id_lulc) {
  as_lulc_data_t(data.table::data.table(id_run, id_period, id_coord = 1:6, id_lulc))
}
db$lulc_data_t <- fom_lulc(10L, 1L, c(1L, 1L, 1L, 1L, 2L, 2L))
db$lulc_data_t <- fom_lulc(10L, 2L, c(2L, 2L, 1L, 1L, 1L, 2L))
db$lulc_data_t <- fom_lulc(11L, 2L, c(2L, 1L, 2L, 1L, 3L, 2L))
db$lulc_data_t <- fom_lulc(12L, 2L, c(2L, 2L, 1L, 1L, 1L, 2L))

db$id_run <- 11L
fom <- db$figure_of_merit_v(1L, 2L, id_run_reference = 10L, id_run_simulated = 11:12)
expect_equal(db$id_run, 11L) # restored after reading each run's lineage
# random allocation within class 1 (4 cells, 2 observed and 2 simulated 1 -> 2) expects one
# hit over a union of 3; class 2 (2 cells, 2 -> 1 observed, 2 -> 3 simulated) no hit over 1.5.
# Run 12 expects 0.5 hits in class 2 (2 -> 1 observed and simulated once each) over 1.5.
expect_equal(
  fom,
  data.table::data.table(
    id_run = 11:12,
    hits = c(1L, 3L),
    wrong_hits = c(1L, 0L),
    misses = c(1L, 0L),
    false_alarms = c(1L, 0L),
    figure_of_merit = c(1 / 4, 1),
    producers_accuracy = c(1 / 3, 1),
    users_accuracy = c(1 / 3, 1),
    figure_of_merit_null = c(1 / 4.5, 1.5 / 4.5)
  )
)

fom_trans <- db$figure_of_merit_v(1L, 2L, id_run_reference = 10L, by_transition = TRUE)
expect_equal(
  fom_trans,
  data.table::data.table(
    id_run = 11L,
    id_lulc_anterior = c(1L, 2L, 2L),
    id_lulc_posterior = c(2L, 1L, 3L),
    observed = c(2L, 1L, 0L),
    simulated = c(2L, 0L, 1L),
    hits = c(1L, 0L, 0L),
    figure_of_merit = c(1 / 3, 0, 0),
    figure_of_merit_null = c(1 / 3, 0, 0)
  )
)
db$id_run <- 0L
