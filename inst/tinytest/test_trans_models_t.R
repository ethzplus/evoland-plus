library(tinytest)

# Fitting records a failure as a sentinel row instead of aborting the batch; this is what
# finds those rows again, so predict_trans_pot can replay the reason next to the error.

models <- as_trans_models_t(data.table::data.table(
  id_run = 0L,
  id_trans = 1:3,
  learner_id = c("classif.rpart", "error", "classif.rpart"),
  learner_params = list(
    list(cp = 0.01),
    list(error_message = "task has only one class"),
    list(error_message = "No data for transition 3, skipping")
  ),
  learner_spec = list(as.raw(1), NULL, NULL),
  crossval_score = list(list(classif.auc = 0.8), list(), list()),
  crossval_predictions = list(as.raw(1), NULL, NULL),
  learner_full = list(as.raw(1), NULL, NULL)
))

failed <- evoland:::failed_fits(models)
expect_equal(failed[["id_trans"]], 2:3)
expect_equal(
  vapply(failed[["learner_params"]], `[[`, character(1), "error_message"),
  c("task has only one class", "No data for transition 3, skipping")
)

# a batch that fitted cleanly has nothing to report
expect_equal(nrow(evoland:::failed_fits(models[1L])), 0L)

# an empty batch is not an error
expect_equal(nrow(evoland:::failed_fits(models[0L])), 0L)
