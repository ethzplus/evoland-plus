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
  cov_results <- db$get_pred_filter_score(
    # the default performance measure for a classification task is classif.ce
    # with minimize TRUE so we expect scores in [-1,0]
    filter = mlr3filters::FilterPerformance$new(resampling = mlr3::rsmp("cv", folds = 2)),
    ordered_pred_data = TRUE # for deterministic behavior
  ),
  "Processing 2 transitions"
)

cov_expected <-
  as_trans_preds_t(data.table::rowwiseDT(
      id_run=, id_pred=, id_trans=, performance=,
      0,       1,        1,         -0.4515679,
      0,       1,        2,         -0.4639171,
      0,       2,        1,         -0.4515679,
      0,       2,        2,         -0.4639171,
      0,       3,        1,         -0.4515679,
      0,       3,        2,         -0.4639171,
      0,       4,        1,         -0.4515679,
      0,       4,        2,         -0.4639171
  ))
expect_equal(cov_results, cov_expected, tol = 1e-7)

exit_file("grrf filter test skipped, not implemented as mlr3 filter yet")
# test grrf filter with custom params
expect_silent(db$set_full_trans_preds(overwrite = TRUE))
expect_message(
  expect_stdout(
    grrf_results <- db$get_pruned_trans_preds_t(
      filter_fun = grrf_filter,
      num.trees = 10,
      gamma = 0.9
    ),
    "Split select weights used"
  ),
  "Processing 2 transitions"
)

# no effect due to synthetic data
grrf_expected <-
  as_trans_preds_t(data.table::rowwiseDT(
      id_run=, id_pred=, id_trans=,
      0,       1,        1,
      0,       1,        2,
      0,       2,        1,
      0,       2,        2,
      0,       3,        1,
      0,       3,        2,
      0,       4,        1,
      0,       4,        2
  ))
expect_equal(grrf_results, grrf_expected)
