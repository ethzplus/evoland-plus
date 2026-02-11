# Shared test helper — provides make_test_db() for integration tests.
# Source this from any test file that needs a populated evoland_db.
#
# Usage:
#   source(file.path(system.file("tinytest", package = "evoland"), "helper_testdb.R"))
#   db <- make_test_db()

#' Create a fully-populated evoland_db in a temp directory
#'
#' Populates: coords_t, periods_t, lulc_meta_t, lulc_data_t,
#'   trans_meta_t, pred_meta_t, pred_data_t_float, neighbors,
#'   neighbor predictors, trans_preds_t.
#'
#' @param include_neighbors bool, run set_neighbors() + generate_neighbor_predictors()
#' @param include_trans_preds bool, run set_full_trans_preds()
#' @return An evoland_db object in a temporary directory. The temp directory path is in db$path.
make_test_db <- function(
  include_neighbors = TRUE,
  include_trans_preds = TRUE
) {
  test_dir <- tempfile("evoland_fixture_")
  db <- evoland_db$new(test_dir)

  # synthetic tables
  db$coords_t <- evoland:::test_coords_t
  db$periods_t <- evoland:::test_periods_t
  db$lulc_meta_t <- evoland:::test_lulc_meta_t
  db$lulc_data_t <- evoland:::test_lulc_data_t
  db$pred_meta_t <- evoland:::test_pred_meta_t
  db$pred_data_t_float <- evoland:::test_pred_data_t_float

  # derived tables
  db$trans_meta_t <- create_trans_meta_t(db$trans_v, min_cardinality_abs = 5L)

  if (include_neighbors) {
    capture.output(suppressMessages(db$set_neighbors()))
    capture.output(suppressMessages(db$generate_neighbor_predictors()))
  }

  if (include_trans_preds) {
    stopifnot(include_neighbors)
    db$set_full_trans_preds()
  }

  db
}
