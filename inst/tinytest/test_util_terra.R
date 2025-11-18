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

# Test compute_neighbors
# Create a simple test coords_t with known distances
test_coords <- data.table::data.table(
  id_coord = 1L:5L,
  lon = c(0, 100, 200, 0, 100),
  lat = c(0, 0, 0, 100, 100),
  elevation = NA_real_,
  geom_polygon = list()
)
test_coords <- as_coords_t(test_coords)

# Test basic neighbor computation with max_distance = 150
neighbors <- compute_neighbors(test_coords, max_distance = 150)

# Check structure
expect_true(data.table::is.data.table(neighbors))
expect_equal(
  names(neighbors),
  c("id_coord_origin", "id_coord_neighbor", "distance_class", "distance")
)

# Check that distances are correct
# Point 1 (0,0) should have neighbor 2 (100,0) at distance 100
# Point 1 (0,0) should have neighbor 4 (0,100) at distance 100
# Point 1 (0,0) should have neighbor 5 (100,100) at distance ~141.4
neighbors_from_1 <- neighbors[id_coord_origin == 1]
expect_equal(nrow(neighbors_from_1), 3L)
expect_true(2L %in% neighbors_from_1$id_coord_neighbor)
expect_true(4L %in% neighbors_from_1$id_coord_neighbor)
expect_true(5L %in% neighbors_from_1$id_coord_neighbor)
expect_equal(
  neighbors_from_1[id_coord_neighbor == 2]$distance,
  100,
  tolerance = 1e-10
)
expect_equal(
  neighbors_from_1[id_coord_neighbor == 4]$distance,
  100,
  tolerance = 1e-10
)
expect_equal(
  neighbors_from_1[id_coord_neighbor == 5]$distance,
  sqrt(100^2 + 100^2),
  tolerance = 1e-10
)

# Point 3 (200,0) should have neighbor 2 (100,0) at distance 100
# and neighbor 5 (100,100) at distance ~141.42
neighbors_from_3 <- neighbors[id_coord_origin == 3]
expect_equal(nrow(neighbors_from_3), 2L)
expect_true(2L %in% neighbors_from_3$id_coord_neighbor)
expect_true(5L %in% neighbors_from_3$id_coord_neighbor)
expect_equal(
  neighbors_from_3[id_coord_neighbor == 2]$distance,
  100,
  tolerance = 1e-10
)
expect_equal(
  neighbors_from_3[id_coord_neighbor == 5]$distance,
  sqrt(100^2 + 100^2),
  tolerance = 1e-10
)

# Test with distance_breaks
neighbors_classified <- compute_neighbors(
  test_coords,
  max_distance = 150,
  distance_breaks = c(0, 100, 150)
)

# Check that distance_class is populated
expect_true(!all(is.na(neighbors_classified$distance_class)))
expect_true(is.factor(neighbors_classified$distance_class))

# With breaks c(0, 100, 150), include.lowest=TRUE, and right=FALSE:
# - [0,100) excludes 100 (lower bound)
# - [100,150] includes both 100 and 150 (upper bound, due to include.lowest)
# Points at distance 100 should be in class [100,150]
# Points at distance 141.4 should be in class [100,150]
neighbors_1_classified <- neighbors_classified[id_coord_origin == 1]
expect_equal(
  as.character(neighbors_1_classified[id_coord_neighbor == 2]$distance_class),
  "[100,150]"
)
expect_equal(
  as.character(neighbors_1_classified[id_coord_neighbor == 4]$distance_class),
  "[100,150]"
)
expect_equal(
  as.character(neighbors_1_classified[id_coord_neighbor == 5]$distance_class),
  "[100,150]"
)

# Test with smaller max_distance
neighbors_small <- compute_neighbors(test_coords, max_distance = 110)

# With max_distance = 110, point 1 should only have neighbors 2 and 4 (distance 100)
# but not 5 (distance ~141.4)
neighbors_from_1_small <- neighbors_small[id_coord_origin == 1]
expect_equal(nrow(neighbors_from_1_small), 2L)
expect_true(2L %in% neighbors_from_1_small$id_coord_neighbor)
expect_true(4L %in% neighbors_from_1_small$id_coord_neighbor)
expect_false(5L %in% neighbors_from_1_small$id_coord_neighbor)

# Test error handling
expect_error(
  compute_neighbors("not_coords_t", max_distance = 100),
  "coords_t must be a coords_t object"
)

expect_error(
  compute_neighbors(test_coords, max_distance = -10),
  "max_distance must be a positive scalar numeric"
)

expect_error(
  compute_neighbors(test_coords, max_distance = 100, distance_breaks = c(1)),
  "distance_breaks must be NULL or a numeric vector with at least 2 elements"
)

expect_error(
  compute_neighbors(test_coords, max_distance = 100, distance_breaks = "invalid"),
  "distance_breaks must be NULL or a numeric vector with at least 2 elements"
)

# Test with real coords_t from earlier in the test file
real_neighbors <- compute_neighbors(coords_t, max_distance = 300)

# Each point in a regular 100m grid should have neighbors
# Interior points should have 8 neighbors within 300m (8-connectivity)
# Edge points should have fewer
expect_true(nrow(real_neighbors) > 0)
expect_true(all(real_neighbors$distance > 0))
expect_true(all(real_neighbors$distance <= 300))

# Check symmetry: if A is neighbor of B, B should be neighbor of A
for (i in 1:min(10, nrow(real_neighbors))) {
  origin <- real_neighbors$id_coord_origin[i]
  neighbor <- real_neighbors$id_coord_neighbor[i]
  dist <- real_neighbors$distance[i]

  reverse_rel <- real_neighbors[
    id_coord_origin == neighbor & id_coord_neighbor == origin
  ]
  expect_equal(nrow(reverse_rel), 1L)
  expect_equal(reverse_rel$distance, dist, tolerance = 1e-10)
}

# Test with distance classification on real data
real_neighbors_class <- compute_neighbors(
  coords_t,
  max_distance = 300,
  distance_breaks = c(0, 150, 300)
)

expect_true(all(!is.na(real_neighbors_class$distance_class)))
expect_equal(
  levels(real_neighbors_class$distance_class),
  c("[0,150)", "[150,300]")
)
