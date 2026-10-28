library(tinytest)

if (!at_home()) {
  # cannot easily mock the dinamica call; skip for now
  exit_file("Integration tests skipped (not at_home)")
}

source(file.path(system.file("tinytest", package = "evoland"), "helper_testdb.R"))
db <- make_test_db()

# without Dinamica, exercise the plumbing around it: the fallback copies the anterior map;
# tinytest resets the option after this file
options(evoland.dinamica_fallback = !nzchar(Sys.which("DinamicaConsole")))
db$trans_rates_t <- db$get_obs_trans_rates()
db$trans_rates_t <- extrapolate_trans_rates(
  db$trans_rates_t,
  db$periods_t,
  coord_count = nrow(db$coords_t)
)

test_learner <- mlr3::lrn("classif.featureless", predict_type = "prob")
test_measures <- c("classif.auc")

# test the package's featureless learner fit and append to disk
expect_message(
  db$trans_models_t <- db$fit_partial_models(
    learner = test_learner,
    measures = test_measures,
    seed = 1244244,
  ),
  "Fitting partial models for 2 transitions..."
)

# Score-select mode: pick best partial model by classif.auc and retrain on full data
expect_message(
  db$trans_models_t <- db$fit_full_models(
    select_score = "classif.auc",
    select_maximize = TRUE
  ),
  "Fitting full models for"
)

# helpers: the rows of a run's map in a period, read through the run's lineage
lulc_of <- function(id_run, id_period) {
  db$get_query(
    "select id_coord, id_lulc from {db$get_read_expr('lulc_data_t', id_run = id_run)}
     where id_period = {id_period} order by id_coord"
  )
}
viable <- db$trans_meta_t[is_viable == TRUE]
# the cells that changed between the observed period 3 and an allocated map, by transition
changes_of <- function(posterior) {
  lulc_of(0L, 3L)[posterior, on = "id_coord"][
    id_lulc != i.id_lulc,
    .N,
    by = .(id_lulc_anterior = id_lulc, id_lulc_posterior = i.id_lulc)
  ]
}

# --------------------------------------------------------------------------
# Dinamica (or the test fallback without Dinamica), run 1
# --------------------------------------------------------------------------
expect_equal(nrow(lulc_of(1L, 4L)), 0L)
expect_message(
  allocate(
    db,
    id_periods = 4L,
    allocator = alloc_dinamica_spec(),
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 1L
  ),
  "Allocating with Dinamica EGO"
)
expect_equal(nrow(lulc_of(1L, 4L)), 900L)
# the active run is left alone; the potentials were predicted under the allocated run
expect_equal(db$id_run, 0L)
expect_equal(db$row_count("trans_pot_t"), 900L)
expect_equal(
  db$get_query("select distinct id_run from dl_db.trans_pot_t", as_atomic = TRUE),
  1L
)

adj_pots <- db$adjusted_trans_pot_v(4L, id_run = 1L)
expect_equal(nrow(adj_pots), 900L)
expect_true(all(adj_pots$value >= 0 & adj_pots$value <= 1))

# --------------------------------------------------------------------------
# CLUMPY, runs 2 and 3: the test DB has multi-pixel patches, so both exercise uPAM; the
# mono-pixel uSAM path is covered by the unit tests in test_alloc_clumpy.R
# --------------------------------------------------------------------------
expect_message(
  allocate(
    db,
    id_periods = 4L,
    allocator = alloc_clumpy_spec(),
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 2L
  ),
  "CLUMPY method: uPAM"
)
expect_equal(nrow(lulc_of(2L, 4L)), 900L)
# only viable transitions happen
expect_true(all(
  changes_of(lulc_of(2L, 4L))[!viable, on = .(id_lulc_anterior, id_lulc_posterior), .N] == 0L
))

# a run with its own models does not take an ancestor's potentials; run 3 predicts its own
expect_message(
  allocate(
    db,
    id_periods = 4L,
    allocator = alloc_clumpy_spec(avoid_aggregation = FALSE),
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 3L
  ),
  "Predicting transition potential"
)
expect_equal(nrow(lulc_of(3L, 4L)), 900L)

options("evoland.use_prefetch_predict" = TRUE)
expect_message(
  allocate(
    db,
    id_periods = 4L,
    allocator = alloc_clumpy_spec(avoid_aggregation = FALSE),
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 3L,
    force_predict = TRUE
  ),
  "CLUMPY: period 3 -> 4",
  fixed = TRUE
)
options("evoland.use_prefetch_predict" = NULL)

# potentials_from: run 4 allocates on its parent's potentials without predicting again
db$runs_t <- as_runs_t(rbind(
  db$runs_t,
  list(id_run = 4, parent_id_run = 3, description = "stochastic alloc")
))
alloc_messages <- character()
withCallingHandlers(
  allocate(
    db,
    id_periods = 4L,
    allocator = alloc_clumpy_spec(avoid_aggregation = FALSE),
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 4L,
    potentials_from = 3L
  ),
  message = function(m) {
    alloc_messages <<- c(alloc_messages, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
)
expect_false(any(grepl("Predicting transition", alloc_messages)))
expect_equal(nrow(lulc_of(4L, 4L)), 900L)
expect_equal(
  db$get_query("select count(*) from dl_db.trans_pot_t where id_run = 4", as_atomic = TRUE),
  0
)

# --------------------------------------------------------------------------
# Greedy (rank-and-fill), deterministic: run 3's potentials, single period, not committed
# --------------------------------------------------------------------------
greedy_one <- function(...) {
  suppressMessages(allocate_one_period(
    db,
    id_period_post = 4L,
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 4L,
    potentials_from = 3L,
    ...
  ))
}
n_lulc_before <- db$row_count("lulc_data_t")
greedy_joint <- greedy_one(allocator = alloc_greedy_spec())
expect_inherits(greedy_joint, "lulc_data_t")
expect_equal(nrow(greedy_joint), 900L)
expect_equal(unique(greedy_joint[["id_run"]]), 4L)
expect_equal(db$row_count("lulc_data_t"), n_lulc_before)
# deterministic: the same map again
expect_identical(
  greedy_joint[order(id_coord), id_lulc],
  greedy_one(allocator = alloc_greedy_spec())[order(id_coord), id_lulc]
)
# no transition exceeds the quota every allocator gets
greedy_quota <- evoland:::allocation_context(db, 4L, 4L, 3L)$quota
greedy_changes <-
  changes_of(greedy_joint)[viable, on = .(id_lulc_anterior, id_lulc_posterior), nomatch = NULL]
expect_true(all(greedy_changes[greedy_quota, on = "id_trans", N <= quota, nomatch = NULL]))
# the cells it changes are the most probable ones: within each transition, the changed cells'
# adjusted potential is at least that of every candidate left unchanged
greedy_pairs <- lulc_of(0L, 3L)[greedy_joint, on = "id_coord"][,
  .(id_coord, id_lulc_anterior = id_lulc, id_lulc_posterior = i.id_lulc)
][
  viable[, .(id_trans, id_lulc_anterior, id_lulc_posterior)],
  on = .(id_lulc_anterior, id_lulc_posterior),
  nomatch = NULL
]
greedy_scored <- db$adjusted_trans_pot_v(4L, id_run = 4L, id_run_trans_pot = 3L)[
  lulc_of(0L, 3L)[, .(id_coord, id_lulc_anterior = id_lulc)],
  on = "id_coord",
  nomatch = NULL
][
  db$trans_meta_t[, .(id_trans, id_lulc_anterior)],
  on = .(id_trans, id_lulc_anterior),
  nomatch = NULL
]
greedy_scored[, changed := FALSE][greedy_pairs, on = .(id_trans, id_coord), changed := TRUE]
# joint ranking: a cell is skipped only when another transition claimed it first, so check the
# cells that did not change at all
unchanged_cells <- setdiff(greedy_scored$id_coord, greedy_pairs$id_coord)
expect_true(all(
  greedy_scored[,
    .(
      ok = !any(changed) ||
        min(value[changed]) >= max(c(-Inf, value[!changed & id_coord %in% unchanged_cells]))
    ),
    by = id_trans
  ]$ok
))
# ordered arbitration with a priority runs; an unknown transition in the priority errors
greedy_ordered <- greedy_one(
  allocator = alloc_greedy_spec("ordered", order = rev(viable[["id_trans"]]))
)
expect_equal(nrow(greedy_ordered), 900L)
expect_error(
  greedy_one(allocator = alloc_greedy_spec("ordered", order = 999L)),
  "order must list viable id_trans only"
)

# CLUMPY and Dinamica return their single-period map without committing it either; Dinamica
# works in a temporary directory it removes afterwards
dinamica_dirs_before <- list.files(tempdir(), pattern = "^dinamica_")
for (allocator in list(alloc_clumpy_spec(), alloc_dinamica_spec())) {
  lulc_single <- greedy_one(allocator = allocator)
  expect_inherits(lulc_single, "lulc_data_t")
  expect_equal(nrow(lulc_single), 900L)
  expect_equal(db$row_count("lulc_data_t"), n_lulc_before)
}
expect_equal(list.files(tempdir(), pattern = "^dinamica_"), dinamica_dirs_before)

# --------------------------------------------------------------------------
# update_neighbors = FALSE skips the neighbor predictors after the last requested period
# --------------------------------------------------------------------------
db$runs_t <- as_runs_t(rbind(
  db$runs_t,
  list(id_run = 5, parent_id_run = 3, description = "no neighbour update")
))
count_run_5_preds <- function() {
  db$get_query(
    "select count(*) from dl_db.pred_data_t where id_run = 5 and id_period = 4",
    as_atomic = TRUE
  )
}
alloc_run_5 <- function(update_neighbors) {
  allocate(
    db,
    id_periods = 4L,
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 5L,
    potentials_from = 3L,
    update_neighbors = update_neighbors
  )
}
suppressMessages(alloc_run_5(update_neighbors = FALSE))
expect_equal(count_run_5_preds(), 0)
suppressMessages(alloc_run_5(update_neighbors = TRUE))
expect_true(count_run_5_preds() > 0)

# --------------------------------------------------------------------------
# Argument checks
# --------------------------------------------------------------------------
expect_error(
  allocate(db, id_periods = c(1L, 3L), select_score = "classif.auc", select_maximize = TRUE),
  "id_periods must be contiguous"
)
expect_error(
  allocate(
    db,
    id_periods = 3:4,
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 4L,
    potentials_from = 3L
  ),
  "potentials_from can differ from id_run for a single period only"
)
expect_error(
  allocate(db, id_periods = 4L, allocator = "clumpy"),
  "allocator must be an allocator specification"
)

# --------------------------------------------------------------------------
# eval_alloc_params_t, with Dinamica by default
# --------------------------------------------------------------------------
expect_message(
  db$alloc_params_t <-
    evaluated_params <-
      db$eval_alloc_params_t(
        select_score = "classif.auc",
        select_maximize = TRUE
      ),
  "Computing per-transition fuzzy similarity"
)

expect_inherits(evaluated_params, "alloc_params_t")
expect_true(all(c("similarity", "frac_patcher") %in% names(evaluated_params)))
expect_true(all(
  evaluated_params$similarity <= 1 &
    evaluated_params$similarity >= 0
))
