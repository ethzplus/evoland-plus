library(tinytest)

# Gate: skip during R CMD check; run with build_install_test()
if (!at_home()) {
  exit_file("Integration tests skipped (not at_home)")
}

# Fixture
source(file.path(
  system.file("tinytest", package = "evoland"),
  "helper_testdb.R"
))
db <- make_test_db(include_neighbors = FALSE, include_trans_preds = FALSE)

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
    filter = mlr3filters::FilterPerformance$new(resampling = mlr3::rsmp("cv", folds = 2)),
    ordered_pred_data = TRUE # for deterministic behavior
  ),
  "Processing 2 transitions"
)

perf_expected <-
  as_trans_preds_t(data.table::rowwiseDT( # nolint start
      id_run=, id_pred=, id_trans=, performance=,
      0,       1,        1,         -0.4515679  ,
      0,       1,        2,         -0.4639171  ,
      0,       2,        1,         -0.4515679  ,
      0,       2,        2,         -0.4639171  ,
      0,       3,        1,         -0.4515679  ,
      0,       3,        2,         -0.4639171  ,
      0,       4,        1,         -0.4515679  ,
      0,       4,        2,         -0.4639171
  )) # nolint end
expect_equal(perf_results, perf_expected, tol = 1e-7)

# Test GRRF filter via FilterImportance
grrf_learner <- LearnerClassifGrrf$new()
grrf_learner$param_set$values <- list(gamma = 0.9, num.trees = 10L, max.depth = 100L)

set.seed(13233)
expect_message(
  importance_results <- db$get_pred_filter_score(
    filter = mlr3filters::FilterImportance$new(learner = grrf_learner),
    ordered_pred_data = TRUE # for deterministic behavior
  ),
  "Processing 2 transitions"
)

importance_expected <-
  as_trans_preds_t(data.table::rowwiseDT( # nolint start
      id_run=, id_pred=, id_trans=, importance=,
      0,       1,        1,         135.268208 ,
      0,       1,        2,         170.083786 ,
      0,       2,        1,         132.745360 ,
      0,       2,        2,         167.511848 ,
      0,       3,        1,           9.780672 ,
      0,       3,        2,          13.208952 ,
      0,       4,        1,          26.786576 ,
      0,       4,        2,          33.555883  
  )) # nolint end
expect_equal(importance_results, importance_expected, tol = 1e-7)
