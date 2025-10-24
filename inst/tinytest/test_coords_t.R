library(tinytest)

# test create_coords_t_square
coords_t <- create_coords_t_square(
  epsg = 2056,
  extent = terra::ext(c(
    xmin = 2697000,
    xmax = 2698000,
    ymin = 1252000,
    ymax = 1253000
  )),
  resolution = 100
)

db <- evoland_db$new(":memory:")

expect_stdout(print(db$coords_t), "(0 rows and 5 cols)")
expect_silent(db$coords_t <- coords_t)
expect_stdout(print(db$coords_t), "Coordinate Table")
expect_equal(coords_t, db$coords_t)
expect_equal(db$row_count("coords_t"), 100L)
