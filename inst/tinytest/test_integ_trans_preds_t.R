require(tinytest)

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

# Test covariance filter
expect_message(
  cov_results <- db$get_pruned_trans_preds_t(
    filter_fun = covariance_filter,
    corcut = 0.2
  ),
  "Processing 2 transitions"
)
cov_expected <-
  as_trans_preds_t(data.table::rowwiseDT(
      id_run=,  id_pred=, id_trans=,
      0,        1,        1,
      0,        1,        2,
      0,        2,        1,
      0,        2,        2,
      0,        3,        1,
      0,        3,        2
  ))
expect_equal(cov_results, cov_expected)

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

grrf_expected <-
  as_trans_preds_t(data.table::rowwiseDT(
      id_run=,  id_pred=, id_trans=,
      0,        1,        1,
      0,        1,        2,
      0,        2,        1,
      0,        2,        2,
      0,        3,        1,
      0,        3,        2
  ))
expect_equal(grrf_results, grrf_expected)

# TODO add test for parallel worker function
