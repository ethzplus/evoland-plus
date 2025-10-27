library(tinytest)

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

expect_true(inherits(coords_t, "coords_t"))
expect_stdout(print(coords_t), "Coordinate Table")
expect_equal(nrow(coords_t), 100L)
expect_true(all(
  c("id_coord", "lon", "lat", "elevation", "geom_polygon") %in% names(coords_t)
))
