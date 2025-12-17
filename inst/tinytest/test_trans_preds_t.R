library(tinytest)


# Test creation and validation
trans_preds_t <- as_trans_preds_t()
expect_silent(print(trans_preds_t))
expect_true(nrow(trans_preds_t) >= 0L)


#' Actually running the predictor selection logic is heavily dependent on previous
#' steps. Hence, this is a more heavy-handed test verging on integration testing.
test_dir_trans_preds <- tempfile("evoland_trans_preds_")
on.exit(unlink(test_dir_trans_preds, recursive = TRUE), add = TRUE)
db_tps <- evoland_db$new(test_dir_trans_preds)

# Set up minimal coords and periods
db_tps$coords_t <- create_coords_t_square(
  epsg = 2056,
  extent = terra::ext(c(xmin = 2697000, xmax = 2697500, ymin = 1252000, ymax = 1252500)),
  resolution = 100
)

db_tps$periods_t <- create_periods_t(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2005-01-01",
  end_extrapolated = "2015-01-01"
)

# Set up LULC classes
db_tps$lulc_meta_t <- create_lulc_meta_t(list(
  forest = list(pretty_name = "Forest", src_classes = 1L),
  urban = list(pretty_name = "Urban", src_classes = 2L),
  agriculture = list(pretty_name = "Agriculture", src_classes = 3L)
))

# Create synthetic LULC data with known transitions
set.seed(42)
n_coords <- nrow(db_tps$coords_t)
lulc_data <- data.table::rbindlist(list(
  data.table::data.table(
    id_coord = 1:n_coords,
    id_lulc = sample(1:2, n_coords, replace = TRUE, prob = c(0.7, 0.3)),
    id_period = 1L
  ),
  data.table::data.table(
    id_coord = 1:n_coords,
    id_lulc = sample(1:2, n_coords, replace = TRUE, prob = c(0.5, 0.5)),
    id_period = 2L
  ),
  data.table::data.table(
    id_coord = 1:n_coords,
    id_lulc = sample(1:2, n_coords, replace = TRUE, prob = c(0.4, 0.6)),
    id_period = 3L
  )
))
db_tps$lulc_data_t <- as_lulc_data_t(lulc_data)

# Create transition metadata
db_tps$trans_meta_t <- create_trans_meta_t(
  db_tps$trans_v,
  min_cardinality_abs = 5L
)

# Add predictor metadata with multiple predictors
pred_spec_tps <- list(
  elevation = list(
    unit = "m",
    pretty_name = "Elevation",
    description = "Elevation above sea level",
    sources = list(list(url = "https://example.com/elevation.tif", md5sum = "abc123"))
  ),
  slope = list(
    unit = "degrees",
    pretty_name = "Slope",
    description = "Terrain slope",
    sources = list(list(url = "https://example.com/slope.tif", md5sum = "def456"))
  ),
  distance_to_road = list(
    unit = "m",
    pretty_name = "Distance to road",
    description = "Distance to nearest road",
    sources = list(list(url = "https://example.com/roads.gpkg", md5sum = "ghi789"))
  ),
  aspect = list(
    unit = "degrees",
    pretty_name = "Aspect",
    description = "Terrain aspect",
    sources = list(list(url = "https://example.com/aspect.tif", md5sum = "jkl012"))
  ),
  soil_type = list(
    pretty_name = "Soil type",
    description = "Soil classification",
    sources = list(list(url = "https://example.com/soil.tif", md5sum = "mno345"))
  )
)
db_tps$pred_meta_t <- create_pred_meta_t(pred_spec_tps)

# Add predictor data - mix of static and time-varying
set.seed(43)
pred_data_static <- data.table::rbindlist(list(
  # Elevation (static, period 0)
  data.table::data.table(
    id_pred = 1L,
    id_coord = 1:n_coords,
    id_period = 0L,
    value = runif(n_coords, 400, 800)
  ),
  # Slope (static, period 0)
  data.table::data.table(
    id_pred = 2L,
    id_coord = 1:n_coords,
    id_period = 0L,
    value = runif(n_coords, 0, 30)
  ),
  # Aspect (static, period 0)
  data.table::data.table(
    id_pred = 4L,
    id_coord = 1:n_coords,
    id_period = 0L,
    value = runif(n_coords, 0, 360)
  )
))

# Time-varying predictor
pred_data_varying <- data.table::rbindlist(lapply(1:3, function(period) {
  data.table::data.table(
    id_pred = 3L, # distance_to_road
    id_coord = 1:n_coords,
    id_period = period,
    value = runif(n_coords, 0, 5000)
  )
}))

db_tps$pred_data_t_float <- as_pred_data_t(
  rbind(pred_data_static, pred_data_varying),
  type = "float"
)

# Add integer predictor (soil_type)
pred_data_int <- data.table::data.table(
  id_pred = 5L,
  id_coord = 1:n_coords,
  id_period = 0L,
  value = sample(1:5, n_coords, replace = TRUE)
)
db_tps$pred_data_t_int <- as_pred_data_t(pred_data_int, type = "int")

expect_equal(
  sort(db_tps$trans_pred_data_v(1L)[, id_coord]),
  c(1, 2, 2, 4, 4, 7, 8, 10, 10, 12, 12, 13, 13, 16, 16, 17, 17, 18, 20, 21, 23, 24)
)

# Test pruning
expect_message(
  trans_preds_result <- db_tps$get_pruned_trans_preds_t(
    corcut = 0.2
  ),
  "Processing transition 1/2"
)

expect_inherits(trans_preds_result, "trans_preds_t")
expect_equal(nrow(trans_preds_result), 4L)

# Verify that all id_trans in result are viable
viable_trans_ids <- db_tps$trans_meta_t[is_viable == TRUE]$id_trans
expect_true(all(trans_preds_result$id_trans %in% viable_trans_ids))

# reset to full set of trans - preds
expect_silent(db_tps$set_full_trans_preds(overwrite = TRUE))
expect_message(
  expect_stdout(
    db_tps$get_pruned_trans_preds_t(
      filter_fun = grrf_filter,
      num.trees = 10,
      gamma = 0.9
    ),
    "Split select weights used"
  ),
  r"{Selected [0-9] predictor\(s\) for transition}"
)

# Test error handling - empty database
test_dir_empty <- tempfile("evoland_empty_")
on.exit(unlink(test_dir_empty, recursive = TRUE), add = TRUE)
db_empty <- evoland_db$new(test_dir_empty)

expect_error(
  db_empty$get_pruned_trans_preds_t(),
  "Table `trans_meta_t` does not exist"
)

# Test with no predictors
test_dir_no_pred <- tempfile("evoland_no_pred_")
on.exit(unlink(test_dir_no_pred, recursive = TRUE), add = TRUE)
db_no_pred <- evoland_db$new(test_dir_no_pred)
db_no_pred$coords_t <- db_tps$coords_t
db_no_pred$periods_t <- db_tps$periods_t
db_no_pred$lulc_meta_t <- db_tps$lulc_meta_t
db_no_pred$lulc_data_t <- db_tps$lulc_data_t
expect_warning(
  db_no_pred$trans_meta_t <- db_tps$trans_meta_t,
  "Overriding existing IDs"
)
expect_error(
  db_no_pred$get_pruned_trans_preds_t(),
  "Table `pred_meta_t` does not exist"
)

# Test print method
expect_stdout(
  print(trans_preds_result),
  "Transition-Predictor|Total relationships"
)
