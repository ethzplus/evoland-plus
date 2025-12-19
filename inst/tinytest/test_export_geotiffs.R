library(tinytest)

# Test export_geotiffs method

# Create test database
test_dir <- tempfile("evoland_export_test_")
on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

db <- evoland_db$new(test_dir)

# Set up coords
db$coords_t <- create_coords_t_square(
  epsg = 2056,
  extent = terra::ext(c(xmin = 2697000, xmax = 2698000, ymin = 1252000, ymax = 1253000)),
  resolution = 100
)

# Set up periods
db$periods_t <- create_periods_t(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2005-01-01",
  end_extrapolated = "2015-01-01"
)

# Set up LULC classes
db$lulc_meta_t <- create_lulc_meta_t(list(
  forest = list(pretty_name = "Forest", src_classes = 1L),
  urban = list(pretty_name = "Urban", src_classes = 2L)
))

# Create synthetic LULC data
n_coords <- nrow(db$coords_t)
lulc_data <- data.table::rbindlist(list(
  data.table::data.table(
    id_coord = 1:n_coords,
    id_lulc = sample(1:2, n_coords, replace = TRUE),
    id_period = 1L
  ),
  data.table::data.table(
    id_coord = 1:n_coords,
    id_lulc = sample(1:2, n_coords, replace = TRUE),
    id_period = 2L
  )
))
db$lulc_data_t <- as_lulc_data_t(lulc_data)

# Add a predictor
pred_data <- data.table::data.table(
  id_coord = rep(1:n_coords, 2),
  id_period = rep(1:2, each = n_coords),
  value = rnorm(n_coords * 2)
)

db$add_predictor(
  pred_spec = list(
    elevation = list(
      unit = "m",
      pretty_name = "Elevation",
      description = "Test elevation",
      sources = list(list(url = "test", md5sum = "test"))
    )
  ),
  pred_data = pred_data,
  pred_type = "float"
)

# Test export_geotiffs
files <- db$export_geotiffs()

expect_true(is.character(files))
expect_true(length(files) >= 2)
expect_true(all(file.exists(files)))
expect_true(all(grepl("\\.tif$", files)))

# Check that lulc_data_t was exported
lulc_file <- files[grepl("lulc_data_t\\.tif$", files)]
expect_equal(length(lulc_file), 1L)

# Check that pred_data_t_float was exported
pred_file <- files[grepl("pred_data_t_float\\.tif$", files)]
expect_equal(length(pred_file), 1L)

# Verify raster properties
lulc_rast <- terra::rast(lulc_file)
expect_equal(terra::nlyr(lulc_rast), 2L)
expect_equal(names(lulc_rast), c("period_1", "period_2"))
expect_equal(terra::crs(lulc_rast, describe = TRUE)$code, "2056")

pred_rast <- terra::rast(pred_file)
expect_equal(terra::nlyr(pred_rast), 2L)
expect_equal(names(pred_rast), c("1_1", "1_2"))

# Test with custom output directory
custom_dir <- file.path(test_dir, "custom_geotiffs")
files2 <- db$export_geotiffs(output_dir = custom_dir, overwrite = TRUE)

expect_true(dir.exists(custom_dir))
expect_true(all(dirname(files2) == custom_dir))
expect_true(all(file.exists(files2)))

# Test overwrite protection
expect_message(
  db$export_geotiffs(output_dir = custom_dir, overwrite = FALSE),
  "Skipping"
)

# Test with custom resolution
files3 <- db$export_geotiffs(
  output_dir = file.path(test_dir, "custom_res"),
  resolution = 200
)

rast_custom <- terra::rast(files3[1])
# Resolution may not be exactly 200 due to extent adjustments
# Terra adjusts resolution to fit the extent evenly
expect_true(all(terra::res(rast_custom) > 150))
expect_true(all(terra::res(rast_custom) < 250))

# Test error handling with empty database
empty_dir <- tempfile("evoland_empty_")
on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)
db_empty <- evoland_db$new(empty_dir)

expect_error(
  db_empty$export_geotiffs(),
  "coords_t.*does not exist|No coordinates found"
)
