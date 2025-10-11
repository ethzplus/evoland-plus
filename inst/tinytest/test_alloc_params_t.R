library(tinytest)

config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
db$config <- read_evoland_config(config_path)
db$coords_t <- create_coords_t(db$config)
db$periods_t <- create_periods_t(db$config)

# lulc meta and data
db$lulc_meta_t <- create_lulc_meta_t(db$config)
db$lulc_data_t <-
  data.table::data.table(
    id_coord = c(1L, 2L, 1L),
    id_lulc = c(1L, 2L, 3L),
    id_period = c(1L, 1L, 2L),
    date = as.Date(c("2000-01-01", "2000-01-01", "2010-01-01"))
  ) |>
  as_lulc_data_t()

db$trans_meta_t <-
  data.table::data.table(
    id_trans = 1:3,
    id_lulc_anterior = 1:3,
    id_lulc_posterior = 2:4,
    cardinality = c(100L, 2000L, 10L),
    frequency_rel = c(0.1, 0.1, 0.15),
    frequency_abs = c(0.1, 0.1, 0.15),
    is_viable = c(TRUE, FALSE, FALSE)
  ) |>
  as_trans_meta_t()

# Test creation and validation
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
expect_silent(db$alloc_params_t <- alloc_params_t)
expect_equal(db$alloc_params_t, alloc_params_t)
