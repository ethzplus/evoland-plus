# Test distance_neighbors_chunked_cpp(), the neighbour search behind set_neighbors()

# collects all chunks; returns the chunks and the bound table
neighbors <- function(coords, max_distance, breaks = numeric(0), chunk_rows = 1e7, quiet = TRUE) {
  chunks <- list()
  total <- evoland:::distance_neighbors_chunked_cpp(
    coords,
    max_distance = max_distance,
    breaks = breaks,
    chunk_rows = chunk_rows,
    callback = function(chunk) chunks[[length(chunks) + 1L]] <<- chunk,
    quiet = quiet
  )
  dt <- data.table::rbindlist(chunks)
  if (nrow(dt) > 0L) {
    data.table::setkeyv(dt, c("id_coord_origin", "id_coord_neighbor"))
  }
  list(total = total, chunks = chunks, dt = dt)
}

# Setup sample data
coords <- data.table::data.table(
  id_coord = 1:5,
  lon = c(0, 0, 0, 100, 200),
  lat = c(0, 10, 20, 0, 0)
)

# Test 1: Basic functionality with neighbor finding and symmetry
# Points 1, 2, 3 are at (0,0), (0,10), (0,20)
# Dist(1,2) = 10, Dist(2,3) = 10, Dist(1,3) = 20
# With max_distance = 15, should find: (1,2), (2,1), (2,3), (3,2)
res <- neighbors(coords, max_distance = 15)
expect_equal(res$total, 4)
expect_equal(
  res$dt,
  data.table::data.table(
    id_coord_origin = c(1L, 2L, 2L, 3L),
    id_coord_neighbor = c(2L, 1L, 3L, 2L),
    distance = c(10.0, 10.0, 10.0, 10.0),
    key = c("id_coord_origin", "id_coord_neighbor")
  )
)
expect_inherits(res$chunks[[1]], "data.table")

# Test 2: Distance classification: integer codes of cut(right = FALSE, include.lowest = TRUE)
res_class <- neighbors(coords, max_distance = 25, breaks = c(0, 15, 30))
# Pairs < 15: (1,2), (2,1), (2,3), (3,2) [Dist 10]
# Pairs >= 15 & < 30: (1,3), (3,1) [Dist 20]
expect_equal(nrow(res_class$dt), 6L)
expect_true(is.integer(res_class$dt$distance_class))
expect_equal(
  res_class$dt$distance_class,
  as.integer(cut(res_class$dt$distance, c(0, 15, 30), right = FALSE, include.lowest = TRUE))
)
expect_equal(unique(res_class$dt[distance == 10]$distance_class), 1L)
expect_equal(unique(res_class$dt[distance == 20]$distance_class), 2L)
# the last break is closed; outside the breaks is NA
res_edge <- neighbors(coords, max_distance = 25, breaks = c(0, 10, 15))
expect_equal(unique(res_edge$dt[distance == 10]$distance_class), 2L)
expect_true(all(is.na(res_edge$dt[distance == 20]$distance_class)))

# Test 3: No callback when max_distance is too small
res_empty <- neighbors(coords, max_distance = 1)
expect_equal(res_empty$total, 0)
expect_equal(length(res_empty$chunks), 0L)

# Test 4: Multiple points in same cell (dense points)
coords_dense <- data.table::data.table(
  id_coord = 1:3,
  lon = c(0, 0.1, 0.2),
  lat = c(0, 0.1, 0.2)
)
res_dense <- neighbors(coords_dense, max_distance = 1.0)
# Should find all mutual pairs: (1,2), (2,1), (1,3), (3,1), (2,3), (3,2)
expect_equal(nrow(res_dense$dt), 6L)
# Verify distances are correct (Euclidean)
expect_equal(res_dense$dt[id_coord_origin == 1 & id_coord_neighbor == 2]$distance, sqrt(0.02))

# Test 5: Resolution boundary effects: neighbours in adjacent hash cells
coords_bound <- data.table::data.table(id_coord = 1:2, lon = c(99, 101), lat = c(0, 0))
res_bound <- neighbors(coords_bound, max_distance = 5)
expect_equal(nrow(res_bound$dt), 2L) # (1,2) and (2,1)
expect_equal(res_bound$dt$distance[1], 2)

# Test 6: Chunks hold complete neighbourhoods and are disjoint in id_coord_origin
grid <- data.table::CJ(lon = seq(0, 900, 100), lat = seq(0, 900, 100))
grid[, id_coord := seq_len(.N)]
res_whole <- neighbors(grid, max_distance = 250, breaks = c(0, 150, 250))
res_chunked <- neighbors(grid, max_distance = 250, breaks = c(0, 150, 250), chunk_rows = 50)
expect_equal(length(res_whole$chunks), 1L)
expect_true(length(res_chunked$chunks) > 5L)
expect_equal(res_chunked$total, res_whole$total)
expect_equal(res_chunked$dt, res_whole$dt)
origins <- lapply(res_chunked$chunks, function(chunk) unique(chunk$id_coord_origin))
expect_equal(anyDuplicated(unlist(origins)), 0L)
# every chunk but the last reaches chunk_rows
chunk_rows <- vapply(res_chunked$chunks, function(chunk) length(chunk$id_coord_origin), 1L)
expect_true(all(head(chunk_rows, -1L) >= 50L))
# symmetric: every pair appears in both directions
expect_equal(
  res_whole$dt[, .(id_coord_origin, id_coord_neighbor)],
  res_whole$dt[, .(id_coord_origin = id_coord_neighbor, id_coord_neighbor = id_coord_origin)][
    order(id_coord_origin, id_coord_neighbor)
  ],
  check.attributes = FALSE
)

# Test 7: Progress output (quiet = FALSE)
expect_stdout(neighbors(coords, max_distance = 15, quiet = FALSE), "Progress")
