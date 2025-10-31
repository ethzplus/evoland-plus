#' evoland utility functions to work with terra objects
#'
#' Useful to coax terra raster and vector data into evoland tabular form, and vice versa.
#'
#' @name util_terra
NULL

#' @describeIn util_terra Extract values from a SpatRaster object using a (minimal) `coords_t`
#' @export
extract_using_coords_t <- function(x, coords_t) {
  stopifnot(inherits(x, "SpatRaster"))

  pts <- terra::vect(coords_t[, .(id_coord, lon, lat)])

  terra::extract(
    x = x,
    y = pts,
    method = "simple",
    ID = FALSE,
    bind = TRUE
  ) |>
    data.table::as.data.table() |>
    data.table::melt(
      id.vars = "id_coord",
      variable.name = "layer",
      value.name = "value"
    ) |>
    na.omit(cols = "value")
}
