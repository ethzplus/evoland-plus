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
#' @param include_alloc_params bool, derive the perturbed alloc_params_t. The
#'   runs_t lineage is always created, since most callers want it and it is
#'   cheap; this is only the ~1s of parameter perturbation on top.
#' @return An evoland_db object in a temporary directory.
make_test_db <- function(
  include_neighbors = TRUE,
  include_trans_preds = TRUE,
  include_alloc_params = TRUE
) {
  test_dir <- tempfile("evoland_fixture_")
  db <- evoland_db$new(test_dir)

  # synthetic tables
  db$coords_t <- evoland:::test_coords_t
  db$periods_t <- evoland:::test_periods_t
  db$lulc_meta_t <- evoland:::test_lulc_meta_t[order(id_lulc)]
  db$lulc_data_t <- evoland:::test_lulc_data_t
  db$pred_meta_t <- evoland:::test_pred_meta_t[order(id_pred)]
  db$pred_data_t <- evoland:::test_pred_data_t

  # derived tables
  db$trans_meta_t <- create_trans_meta_t(db$trans_v, min_cardinality_abs = 5L)

  if (include_neighbors) {
    suppressMessages(db$set_neighbors(quiet = TRUE))
    suppressMessages(db$generate_neighbor_predictors())
  }

  if (include_trans_preds) {
    db$set_full_trans_preds()
  }

  # the run lineage is what most callers are after and costs ~0.1s; deriving
  # the perturbed parameters costs ~1s, so only the allocation tests pay for it
  db$runs_t <-
    data.table::data.table(
      id_run = 0L:3L,
      parent_id_run = c(NA, 0L, 0L, 0L),
      description = c("base", "unperturbed", "perturbation 1", "perturbation 2")
    ) |>
    as_runs_t()

  if (include_alloc_params) {
    db$id_run <- 1L
    best_estimate <- suppressMessages(db$create_alloc_params_t())
    db$id_run <- 0L
    # runs 2 and 3 jitter frac_expander around the best estimate on run 1
    perturbed <- lapply(2:3, function(perturbed_run) {
      jittered <- best_estimate[["frac_expander"]] + stats::rnorm(nrow(best_estimate), sd = 0.1)
      jittered <- pmax(0, pmin(1, jittered))
      data.table::copy(best_estimate)[, `:=`(
        id_run = perturbed_run,
        frac_expander = jittered,
        frac_patcher = 1 - jittered
      )]
    })
    db$alloc_params_t <- as_alloc_params_t(data.table::rbindlist(
      c(list(best_estimate), perturbed),
      use.names = TRUE
    ))
  }

  db
}
