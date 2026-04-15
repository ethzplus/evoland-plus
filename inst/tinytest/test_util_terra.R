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

# Test extract_using_coords_t with SpatRaster ####
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


# both layers are double, no warning; throw away result
expect_silent(extract_using_coords_t(c(layer2, layer3), coords_t))

# layer1 is int, will be coerced to double; keep result for inspection
expect_warning(
  raster_result <- extract_using_coords_t(
    rast_combined <- c(layer1, layer2, layer3),
    coords_t
  ),
  "By order of hierarchy, the molten data value column will be of type 'double'"
)

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

# Test extract_using_coords_t with SpatVector ####
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

expect_silent(vector_result <- extract_using_coords_t(vect_data, coords_t))

expect_equal(
  vector_result[id_coord == 1][order(attribute, -rank(value))],
  # fmt: skip
  data.table::data.table(
    id_coord = 1L,
    attribute = factor(
      rep(c("alternate_id_name", "population"), each = 6L),
      levels = c("alternate_id_name", "population")
    ),
    value = c(
      21,    12,    11,    3,    2,    1,
      21000, 12000, 11000, 3000, 2000, 1000
    )
  )
)

# Test na_omit parameter with SpatRaster containing NAs ####
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


# Test tabular_to_raster ####
# Simple case: data with id_coord and a value column indicating class
data_simple <- data.table::data.table(
  id_coord = coords_t$id_coord,
  lulc = sample(1:5, nrow(coords_t), replace = TRUE)
)
expect_silent(
  rast_simple <- tabular_to_raster(data_simple, coords_t, value_col = "lulc")
)
expect_inherits(rast_simple, "SpatRaster")
expect_equal(terra::nlyr(rast_simple), 1L)
expect_equal(names(rast_simple), "lulc")
expect_equal(terra::res(rast_simple)[1], 100) # inherits from coords

# 2. Complex case: data with additional grouping columns (pivot required)
# Create data with 2 scenarios and 2 years -> should yield 4 layers
data_grouped <- data.table::CJ(
  id_coord = coords_t$id_coord,
  scenario = c("scenA", "scenB"),
  year = c(2020, 2030)
)
data_grouped[, value := as.numeric(id_coord)] # Dummy value

expect_silent(
  rast_grouped <- tabular_to_raster(data_grouped, coords_t, value_col = "value")
)
# Check naming convention logic: grouping_col + _ + value
# implies 4 layers got generated
# e.g., scenario_scenA_year_2020
expect_equal(
  names(rast_grouped),
  c(
    "scenario_scenA_year_2020",
    "scenario_scenA_year_2030",
    "scenario_scenB_year_2020",
    "scenario_scenB_year_2030"
  )
)

# 3. Explicit resolution argument
# Pass a resolution different from the default 100 in coords
expect_silent(
  rast_res_arg <- tabular_to_raster(
    data_simple,
    coords_t,
    value_col = "lulc",
    resolution = 50
  )
)
expect_equal(terra::res(rast_res_arg)[1], 50)

# 4. Estimate resolution from coords (e.g. because not on regular raster)
coords_no_res <- data.table::copy(coords_t)
attr(coords_no_res, "resolution") <- NULL

expect_silent(
  rast_auto_res <- tabular_to_raster(
    data_simple,
    coords_no_res,
    value_col = "lulc"
  )
)
# Should calculate 100 based on coordinate spacing
expect_equal(terra::res(rast_auto_res)[1], 100)
