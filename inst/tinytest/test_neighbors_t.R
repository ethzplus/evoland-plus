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

# Test 1: Basic functionality
# Points 1, 2, 3 are at (0,0), (0,10), (0,20)
# Dist(1,2) = 10, Dist(2,3) = 10, Dist(1,3) = 20
# Point 4 is at (100,0). Dist(1,4) = 100
# Point 5 is at (200,0). Dist(4,5) = 100, Dist(1,5) = 200

# With max_distance = 15
# Should find: (1,2), (2,1), (2,3), (3,2)
res <- create_neighbors_t(coords, max_distance = 15, quiet = TRUE)

expected_pairs <-
  data.table::data.table(
    id_coord_origin = c(1L, 2L, 2L, 3L),
    id_coord_neighbor = c(2L, 1L, 3L, 2L),
    distance = c(10.0, 10.0, 10.0, 10.0)
  ) |>
  as_neighbors_t()

expect_equal(res, expected_pairs)

# Test 2: Multiple points in same cell (Dense)
# Create points very close together
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

# Test 3: Resolution boundary effects
# Points at cell boundaries might be missed if kernel logic is wrong
coords_bound <-
  data.table::data.table(
    id_coord = 1:2,
    lon = c(99, 101), # Straddle 100 boundary if grid was 100
    lat = c(0, 0),
    elevation = 0,
    geom_polygon = list()
  ) |>
  as_coords_t()

# Distance is 2.
res_bound <- create_neighbors_t(coords_bound, max_distance = 5, quiet = TRUE)

expect_equal(nrow(res_bound), 2) # (1,2) and (2,1)
expect_equal(res_bound$distance[1], 2)

# Test 4: Distance Classes
neighbors_class <- create_neighbors_t(
  coords,
  max_distance = 25,
  distance_breaks = c(0, 15, 30),
  quiet = TRUE
)
# Pairs < 15: (1,2), (2,1), (2,3), (3,2) [Dist 10]
# Pairs >= 15 & < 30: (1,3), (3,1) [Dist 20]
# Total 6 rows
expect_equal(nrow(neighbors_class), 6)
expect_true("distance_class" %in% names(neighbors_class))

# Check classification
# 10 falls in [0,15)
# 20 falls in [15,30]
levels_found <- levels(neighbors_class$distance_class)
expect_equal(length(levels_found), 2)

d10_class <- neighbors_class[distance == 10]$distance_class[1]
d20_class <- neighbors_class[distance == 20]$distance_class[1]
expect_true(d10_class != d20_class)

# Test 5: Empty result
res_empty <- create_neighbors_t(coords, max_distance = 1, quiet = TRUE)
expect_equal(nrow(res_empty), 0)
expect_inherits(res_empty, "neighbors_t")
