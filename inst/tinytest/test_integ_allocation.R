library(tinytest)

if (!at_home()) {
  exit_file("Integration tests skipped (not at_home)")
}

# The allocation loop around the allocators: prediction, the allocation context, committing
# the map and the neighbor predictors, runs and their lineage. It is the same for every
# allocator, so the deterministic greedy allocator stands in for all of them; the allocators
# themselves are tested on hand-built contexts in test_alloc_*.R

source(file.path(system.file("tinytest", package = "evoland"), "helper_testdb.R"))
db <- make_test_db()

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
# allocate(): run 1 predicts its potentials, allocates and commits period 4
# --------------------------------------------------------------------------
expect_equal(nrow(lulc_of(1L, 4L)), 0L)
expect_message(
  allocate(
    db,
    id_periods = 4L,
    allocator = alloc_greedy_spec(),
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 1L
  ),
  "Allocating with greedy (joint): run 1",
  fixed = TRUE
)
expect_equal(nrow(lulc_of(1L, 4L)), 900L)
# only viable transitions happen
expect_true(all(
  changes_of(lulc_of(1L, 4L))[!viable, on = .(id_lulc_anterior, id_lulc_posterior), .N] == 0L
))
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

# a run with its own models does not take an ancestor's potentials; run 3 predicts its own
expect_message(
  allocate(
    db,
    id_periods = 4L,
    allocator = alloc_greedy_spec(),
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 3L
  ),
  "Predicting transition potential"
)
expect_equal(nrow(lulc_of(3L, 4L)), 900L)

# force_predict predicts again, also with prefetching
options("evoland.use_prefetch_predict" = TRUE)
expect_message(
  allocate(
    db,
    id_periods = 4L,
    allocator = alloc_greedy_spec(),
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 3L,
    force_predict = TRUE
  ),
  "Predicting transition potential"
)
options("evoland.use_prefetch_predict" = NULL)

# --------------------------------------------------------------------------
# potentials_ancestor = 1: run 4 allocates on its parent's potentials without predicting
# --------------------------------------------------------------------------
db$runs_t <- as_runs_t(rbind(
  db$runs_t,
  list(id_run = 4, parent_id_run = 3, description = "stochastic alloc")
))
alloc_messages <- character()
withCallingHandlers(
  allocate(
    db,
    id_periods = 4L,
    allocator = alloc_greedy_spec(),
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 4L,
    potentials_ancestor = 1L
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
# allocate_one_period(): returns the map without committing it
# --------------------------------------------------------------------------
n_lulc_before <- db$row_count("lulc_data_t")
expect_message(
  one_period <- allocate_one_period(
    db,
    id_period_post = 4L,
    allocator = alloc_greedy_spec(),
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 4L,
    potentials_ancestor = 1L
  ),
  "Changed [0-9]+ of 900 cells"
)
expect_inherits(one_period, "lulc_data_t")
expect_equal(nrow(one_period), 900L)
expect_equal(unique(one_period[["id_run"]]), 4L)
expect_equal(db$row_count("lulc_data_t"), n_lulc_before)
# no transition exceeds the quota the context gives every allocator
quota <- evoland:::allocation_context(db, 4L, 4L, 3L)$quota
expect_true(all(
  changes_of(one_period)[viable, on = .(id_lulc_anterior, id_lulc_posterior), nomatch = NULL][
    quota,
    on = "id_trans",
    N <= quota,
    nomatch = NULL
  ]
))

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
    allocator = alloc_greedy_spec(),
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 5L,
    potentials_ancestor = 1L,
    update_neighbors = update_neighbors
  )
}
expect_message(alloc_run_5(update_neighbors = FALSE), "Allocating with greedy")
expect_equal(count_run_5_preds(), 0)
expect_message(alloc_run_5(update_neighbors = TRUE), "Allocating with greedy")
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
    potentials_ancestor = 1L
  ),
  "potentials_ancestor above 0 works for a single period only"
)
expect_error(
  allocate_one_period(
    db,
    id_period_post = 4L,
    select_score = "classif.auc",
    select_maximize = TRUE,
    id_run = 4L,
    potentials_ancestor = 5L
  ),
  "Run 4 has no ancestor 5 generations up"
)
expect_error(
  allocate(db, id_periods = 4L, allocator = "clumpy"),
  "allocator must be an allocator specification"
)
expect_error(
  evoland:::new_allocator_spec("x", list(), one_period = function() NULL),
  "one_period must be a function of one argument"
)
