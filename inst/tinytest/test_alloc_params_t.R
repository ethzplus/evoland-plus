library(tinytest)

alloc_params_t <- as_alloc_params_t(list(
  id_trans = 1L,
  id_period = 2L,
  alloc_params = list(
    list(
      mean_patch_size = 1.3,
      patch_size_variance = 1.4,
      patch_isometry = 0.2,
      share_expander = 0.8
    )
  ),
  goodness_of_fit = list(
    list(
      window_size = 11,
      fuzzy_similarity = 0.8
    )
  )
))

expect_silent(alloc_params_t)
expect_equal(nrow(alloc_params_t), 1L)
expect_silent(print(alloc_params_t))
expect_inherits(alloc_params_t, "alloc_params_t")

# Test expected column structure
expected_cols <- c("id_trans", "id_period", "alloc_params", "goodness_of_fit")
expect_true(all(expected_cols %in% names(alloc_params_t)))

# Test that the nested list structures are preserved
expect_true(is.list(alloc_params_t$alloc_params[[1]]))
expect_true(is.list(alloc_params_t$goodness_of_fit[[1]]))
expect_equal(alloc_params_t$alloc_params[[1]]$mean_patch_size, 1.3)
expect_equal(alloc_params_t$goodness_of_fit[[1]]$window_size, 11)
