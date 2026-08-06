library(tinytest)

# Fitting records a failure as a sentinel row instead of aborting the batch. These helpers
# are what turns those rows back into a report; without them a failed fit is only visible
# several steps later, when allocation finds a viable transition that has no model.

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

# a successful batch reports nothing and passes the models through unchanged
expect_equal(nrow(evoland:::failed_fits(models[1L])), 0L)
expect_silent(evoland:::warn_failed_fits(models[1L]))
expect_equal(evoland:::warn_failed_fits(models[1L]), models[1L])

# an empty batch is not an error
expect_equal(nrow(evoland:::failed_fits(models[0L])), 0L)

# the warning names every failed transition and quotes the recorded reason
expect_warning(
  evoland:::warn_failed_fits(models),
  "No model was fitted for 2 of 3 transition\\(s\\)"
)
expect_warning(evoland:::warn_failed_fits(models), "id_trans 2: task has only one class")
expect_warning(evoland:::warn_failed_fits(models), "id_trans 3: No data for transition 3")
