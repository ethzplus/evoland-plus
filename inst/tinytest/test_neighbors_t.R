# Test create_neighbors_t functionality

# Setup sample data
coords <-
  data.table::data.table(
    id_coord = 1:5,
    lon = c(0, 0, 0, 100, 200),
    lat = c(0, 10, 20, 0, 0),
    elevation = 0,
    geom_polygon = list()
  ) |>
  as_coords_t()

# Test 1: Basic functionality with neighbor finding and symmetry
# Points 1, 2, 3 are at (0,0), (0,10), (0,20)
# Dist(1,2) = 10, Dist(2,3) = 10, Dist(1,3) = 20
# With max_distance = 15, should find: (1,2), (2,1), (2,3), (3,2)
res <- create_neighbors_t(coords, max_distance = 15, quiet = TRUE)

expected_pairs <-
  data.table::data.table(
    id_coord_origin = c(1L, 2L, 2L, 3L),
    id_coord_neighbor = c(2L, 1L, 3L, 2L),
    distance = c(10.0, 10.0, 10.0, 10.0)
  ) |>
  as_neighbors_t()

expect_equal(res, expected_pairs)
expect_inherits(res, "neighbors_t")

# Verify symmetry: if A is neighbor of B, B should be neighbor of A
expect_equal(res[id_coord_origin == 1 & id_coord_neighbor == 2]$distance, 10.0)
expect_equal(res[id_coord_origin == 2 & id_coord_neighbor == 1]$distance, 10.0)

# Test 2: Distance classification
neighbors_class <- create_neighbors_t(
  coords,
  max_distance = 25,
  distance_breaks = c(0, 15, 30),
  quiet = TRUE
)

# Pairs < 15: (1,2), (2,1), (2,3), (3,2) [Dist 10]
# Pairs >= 15 & < 30: (1,3), (3,1) [Dist 20]
expect_equal(nrow(neighbors_class), 6)
expect_true("distance_class" %in% names(neighbors_class))
expect_true(is.factor(neighbors_class$distance_class))
expect_equal(length(levels(neighbors_class$distance_class)), 2)

d10_class <- neighbors_class[distance == 10]$distance_class[1]
d20_class <- neighbors_class[distance == 20]$distance_class[1]
expect_true(d10_class != d20_class)

# Test 3: Empty result when max_distance is too small
res_empty <- create_neighbors_t(coords, max_distance = 1, quiet = TRUE)
expect_equal(nrow(res_empty), 0)
expect_inherits(res_empty, "neighbors_t")

# Test 4: Multiple points in same cell (dense points)
coords_dense <-
  data.table::data.table(
    id_coord = 1:3,
    lon = c(0, 0.1, 0.2),
    lat = c(0, 0.1, 0.2),
    elevation = 0,
    geom_polygon = list()
  ) |>
  as_coords_t()

res_dense <- create_neighbors_t(coords_dense, max_distance = 1.0, quiet = TRUE)

# Should find all mutual pairs: (1,2), (2,1), (1,3), (3,1), (2,3), (3,2)
expect_equal(nrow(res_dense), 6)

# Verify distances are correct (Euclidean)
d12 <- sqrt(0.01 + 0.01)
sub <- res_dense[id_coord_origin == 1 & id_coord_neighbor == 2]
expect_equal(sub$distance, d12)

# Test 5: Resolution boundary effects
coords_bound <-
  data.table::data.table(
    id_coord = 1:2,
    lon = c(99, 101),
    lat = c(0, 0),
    elevation = 0,
    geom_polygon = list()
  ) |>
  as_coords_t()

res_bound <- create_neighbors_t(coords_bound, max_distance = 5, quiet = TRUE)

expect_equal(nrow(res_bound), 2) # (1,2) and (2,1)
expect_equal(res_bound$distance[1], 2)

# Test 6: Progress output (quiet = FALSE)
expect_stdout(
  create_neighbors_t(coords, max_distance = 15),
  "Progress"
)

# Test 7: Error handling
expect_error(
  create_neighbors_t("not_coords_t", max_distance = 100),
  "coords_t must be a coords_t object"
)

expect_error(
  create_neighbors_t(coords, max_distance = -10),
  "max_distance must be a positive scalar numeric"
)

expect_error(
  create_neighbors_t(coords, max_distance = 100, distance_breaks = c(1)),
  "distance_breaks must be NULL or a numeric vector with at least 2 elements"
)

expect_error(
  create_neighbors_t(coords, max_distance = 100, distance_breaks = "invalid"),
  "distance_breaks must be NULL or a numeric vector with at least 2 elements"
)
