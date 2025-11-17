library(tinytest)

testextent <- terra::ext(c(
  xmin = 2697000,
  xmax = 2698000,
  ymin = 1252000,
  ymax = 1253000
))

# Create a set of coordinate points for testing
coords_t <- create_coords_t_square(
  epsg = 2056,
  extent = testextent,
  resolution = 100
)

# Test extract_using_coords_t with SpatRaster
# Create synthetic SpatRaster with multiple layers, extending beyond coords_t
rast_template <- terra::rast(
  extent = terra::extend(testextent, 10000),
  resolution = 100,
  crs = "EPSG:2056"
)

# Create multiple layers with different values
layer1 <- rast_template
terra::values(layer1) <- seq_len(terra::ncell(layer1))
names(layer1) <- "temperature"

layer2 <- rast_template
terra::values(layer2) <- seq_len(terra::ncell(layer2)) * 10
names(layer2) <- "precipitation"

layer3 <- rast_template
terra::values(layer3) <- seq_len(terra::ncell(layer3)) * 0.5
names(layer3) <- "elevation_diff"


# Test SpatRaster extraction
expect_silent(
  extract_using_coords_t(
    c(layer2, layer3), # both layers are double, no warning
    coords_t
  )
)
expect_warning(
  raster_result <- extract_using_coords_t(
    rast_combined <- c(layer1, layer2, layer3), # both layers are double, no warning
    coords_t
  ),
  "By order of hierarchy, the molten data value column will be of type 'double'"
)

# integrated test: extracted values, exact object structure
expect_identical(
  raster_result[id_coord %in% 1:2],
  data.table::data.table(
    id_coord = rep.int(1:2, 3L),
    layer = factor(
      rep(names(rast_combined), each = 2L),
      levels = names(rast_combined)
    ),
    value = c(21101.0, 21102.0, 211010.0, 211020.0, 10550.5, 10551.0)
  )
)
expect_equal(nrow(raster_result), 300L) # 100 coords * 3 layers

# Test extract_using_coords_t with SpatVector
vect_data <-
  coords_t[, .(
    alternate_id_name = id_coord,
    population = .I * 1000L,
    lon,
    lat
  )] |>
  data.table::as.data.table() |>
  terra::vect(crs = "EPSG:2056") |>
  # this will create overlapping buffers, making extraction non-trivial
  terra::buffer(200)

# Test SpatVector extraction
expect_silent(vector_result <- extract_using_coords_t(vect_data, coords_t))


expect_equal(
  vector_result[id_coord %in% 1],
  data.table::data.table(
    id_coord = 1L,
    attribute = factor(
      rep(c("alternate_id_name", "population"), each = 6L),
      levels = c("alternate_id_name", "population")
    ),
    value = c(
      21,
      12,
      11,
      3,
      2,
      1,
      21000,
      12000,
      11000,
      3000,
      2000,
      1000
    )
  )
)

# Test na_omit parameter with SpatRaster containing NAs
rast_with_na <- rast_template
values_with_na <- seq_len(terra::ncell(rast_with_na))
# introduce NA for 30% of cells
set.seed(124555)
values_with_na[sample(seq_along(values_with_na), 210^2 * 0.3)] <- NA_integer_
terra::values(rast_with_na) <- values_with_na
names(rast_with_na) <- "with_na"

# Test with na_omit = TRUE (default)
result_na_omit_true <- extract_using_coords_t(rast_with_na, coords_t, na_omit = TRUE)
expect_true(all(!is.na(result_na_omit_true$value)))
# can test for equality given that we set the seed
expect_equal(nrow(result_na_omit_true), 67)

# Test with na_omit = FALSE
result_na_omit_false <- extract_using_coords_t(rast_with_na, coords_t, na_omit = FALSE)
expect_true(anyNA(result_na_omit_false$value))
expect_equal(nrow(result_na_omit_false), 100L)

# Test na_omit parameter with SpatVector containing NAs
vect_with_na <- vect_data
vect_with_na$population[1:20] <- NA_integer_

# Test with na_omit = TRUE (default)
vector_na_omit_true <- extract_using_coords_t(vect_with_na, coords_t, na_omit = TRUE)
expect_equal(nrow(vector_na_omit_true), 2016L)
expect_true(!anyNA(vector_na_omit_true$value))

# Test with na_omit = FALSE
vector_na_omit_false <- extract_using_coords_t(vect_with_na, coords_t, na_omit = FALSE)
expect_true(anyNA(vector_na_omit_false$value))
