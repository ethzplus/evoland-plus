library(tinytest)

# Gate: skip during R CMD check; run with build_install_test()
if (!at_home()) {
  exit_file("Integration tests skipped (not at_home)")
}

# Fixture
source(file.path(
  system.file("tinytest", package = "evoland.plus"),
  "helper_testdb.R"
))
db <- make_test_db(
  include_neighbors = FALSE,
  include_trans_preds = FALSE,
  include_alloc_params = FALSE
)

# Test empty table
expect_stdout(print(as_trans_preds_t()), "Transition-Predictor Relationships")

# suppress info logs from mlr3 during testing
lgr::get_logger("mlr3")$set_threshold("warn")

set.seed(123)
# Test covariance filter
expect_message(
  perf_results <- db$get_pred_filter_score(
    # the default performance measure for a classification task is classif.ce
    # with minimize TRUE so we expect scores in [-1,0]
    filter = mlr3filters::FilterPerformance$new(resampling = mlr3::rsmp("cv", folds = 2))
  ),
  "Processing 2 transitions"
)

perf_expected <-
  as_trans_preds_t(data.table::rowwiseDT( # nolint start
      id_run=, id_pred=, id_trans=, performance=,
      0,       1,        1,         -0.4639329  ,
      0,       1,        2,         -0.4515716  ,
      0,       2,        1,         -0.4639329  ,
      0,       2,        2,         -0.4515716  ,
      0,       3,        1,         -0.4639329  ,
      0,       3,        2,         -0.4515716  ,
      0,       4,        1,         -0.4639329  ,
      0,       4,        2,         -0.4515716
  )) # nolint end
expect_equal(perf_results, perf_expected, tol = 1e-6)

# Test GRRF filter via FilterImportance
grrf_learner <- LearnerClassifGrrf$new()
grrf_learner$param_set$values <- list(gamma = 0.9, num.trees = 10L, max.depth = 100L)

set.seed(13233)
expect_message(
  importance_results <- db$get_pred_filter_score(
    filter = mlr3filters::FilterImportance$new(learner = grrf_learner)
  ),
  "Processing 2 transitions"
)

importance_expected <-
  as_trans_preds_t(data.table::rowwiseDT( # nolint start
      id_run=, id_pred=, id_trans=, importance=,
      0,       1,        1,         175.322855 ,
      0,       1,        2,         129.138887 ,
      0,       2,        1,         168.299098 ,
      0,       2,        2,         137.339063 ,
      0,       3,        1,          15.719198 ,
      0,       3,        2,          14.312763 ,
      0,       4,        1,          39.324345 ,
      0,       4,        2,          27.297686
  )) # nolint end
expect_equal(importance_results, importance_expected, tol = 1e-6)

# Test get_pred_filter_score with a manually supplied trans_preds argument
# Restrict to id_trans == 1 only; expect exactly 1 transition processed
db$set_full_trans_preds(overwrite = TRUE)
trans_preds_t1 <- db$trans_preds_t[id_trans == 1L]

set.seed(123)
expect_message(
  perf_results_manual <- db$get_pred_filter_score(
    filter = mlr3filters::FilterPerformance$new(resampling = mlr3::rsmp("cv", folds = 2)),
    trans_preds = trans_preds_t1
  ),
  "Processing 1 transitions"
)

# Result should contain only id_trans == 1 rows, identical to the full run
expect_equal(
  perf_results_manual,
  perf_results[id_trans == 1L],
  tol = 1e-7
)

# selecting by score: the assignment flips the flags and stores the scores, and the pruned
# pairs stay in the table (#97)
db$set_full_trans_preds(overwrite = TRUE)
importance_results[, is_selected := id_trans != 1L | importance > 100]
expect_message(db$trans_preds_t <- importance_results, "Added column\\(s\\).*importance")
stored <- db$trans_preds_t
expect_equal(nrow(stored), nrow(importance_results))
expect_equal(stored[is_selected == TRUE & id_trans == 1L, id_pred], 1:2)
expect_equal(stored[["importance"]], importance_results[["importance"]], tol = 1e-6)

# a second filter only scores what the first one kept
set.seed(123)
expect_message(
  perf_refined <- db$get_pred_filter_score(
    filter = mlr3filters::FilterPerformance$new(resampling = mlr3::rsmp("cv", folds = 2))
  ),
  "Processing 2 transitions"
)
expect_equal(perf_refined[id_trans == 1L, id_pred], 1:2)
db$trans_preds_t <- perf_refined
expect_true(all(c("importance", "performance") %in% names(db$trans_preds_t)))

# a child run's selection leaves the base run's alone
db$runs_t <- as_runs_t(rbind(
  db$runs_t,
  list(id_run = 4L, parent_id_run = 0L, description = "own predictor sets")
))
db$id_run <- 4L
db$set_full_trans_preds() # run 4 holds no predictor sets of its own yet
expect_true(all(db$trans_preds_t[["is_selected"]]))
db$id_run <- 0L
expect_equal(db$trans_preds_t[is_selected == TRUE & id_trans == 1L, id_pred], 1:2)
db$id_run <- 4L
expect_error(db$set_full_trans_preds(), "already holds predictor sets for this run")
db$id_run <- 0L

# starting over removes the run's scores along with its flags
db$set_full_trans_preds(overwrite = TRUE)
expect_true(all(db$trans_preds_t[["is_selected"]]))
expect_true(all(is.na(db$trans_preds_t[["importance"]])))
