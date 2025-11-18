#' evoland utility functions to work with terra objects
#'
#' Useful to coax terra raster and vector data into evoland tabular form.
#'
#' @name util_terra
NULL

#' @describeIn util_terra Extract values from a SpatRaster or SpatVector object using a (minimal) `coords_t`
#' @param x The object to extract from; use "simple" extraction for rasters, i.e. no resampling is done.
#' @param coords_t A coords_t object containing coordinate points
#' @return A long data.table with `id_coord`, a `layer`/`attribute` column, and a `value` column.
#' NAs are omitted
#' @export
extract_using_coords_t <- function(x, coords_t, na_omit = TRUE) {
  UseMethod("extract_using_coords_t")
}

#' @exportS3Method
extract_using_coords_t.SpatRaster <- function(x, coords_t, na_omit = TRUE) {
  pts <-
    coords_t[, .(id_coord, lon, lat)] |>
    data.table::as.data.table() |>
    terra::vect()

  out <-
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
    )

  if (na_omit) {
    return(na.omit(out, cols = "value"))
  }

  out
}

#' @exportS3Method
extract_using_coords_t.SpatVector <- function(x, coords_t, na_omit = TRUE) {
  if ("id_coord" %in% names(x)) {
    stop("x cannot have an id_coord attribute")
  }
  pts <-
    coords_t[, .(lon, lat)] |>
    as.matrix() |>
    terra::vect()

  tmp <-
    terra::extract(
      x = x,
      y = pts
    ) |>
    data.table::as.data.table() |>
    merge(
      coords_t[, .(id.y = .I, id_coord)],
      by = "id.y"
    )

  # id.y is based on position, drop in favour of id_coord
  data.table::set(tmp, j = "id.y", value = NULL)

  # y attributes dropped by terra::extract, add back
  data.table::setcolorder(tmp, "id_coord")

  # recover original names
  # terra::extract returns data.frame with aggressive name repair
  data.table::setnames(tmp, c("id_coord", names(x)))

  out <- data.table::melt(
    tmp,
    id.vars = "id_coord",
    variable.name = "attribute",
    value.name = "value"
  )

  if (na_omit) {
    return(na.omit(out, cols = "value"))
  }

  out
}

#' @describeIn util_terra Compute neighboring coordinates within specified distances
#' @param max_distance Maximum distance to search for neighbors (in same units as coordinates)
#' @param distance_breaks Optional numeric vector defining distance class boundaries.
#'   If NULL, computes exact distances without classification.
#'   If provided, must have at least 2 elements defining interval breaks.
#' @param resolution Grid cell size for rasterization (default: 100.0, in same units as coordinates)
#' @return A data.table with columns:
#'   - id_coord_origin: ID of the origin coordinate
#'   - id_coord_neighbor: ID of the neighboring coordinate
#'   - distance: Distance between origin and neighbor
#'   - distance_class: Factor indicating distance class (if distance_breaks provided)
#' @export
compute_neighbors <- function(
  coords_t,
  max_distance,
  distance_breaks = c(0, max_distance),
  resolution = 100.0
) {
  # Validate inputs
  if (!inherits(coords_t, "coords_t")) {
    stop("coords_t must be a coords_t object")
  }

  if (!is.numeric(max_distance) || length(max_distance) != 1 || max_distance <= 0) {
    stop("max_distance must be a positive scalar numeric")
  }

  # Call C++ function
  dt <- distance_neighbors_cpp(
    coords_t = coords_t,
    max_distance = max_distance,
    breaks = distance_breaks,
    resolution = resolution
  )

  # Set data.table allocation
  dt <- data.table::setalloccol(dt)

  # Rename distance_approx to distance
  data.table::setnames(dt, "distance_approx", "distance")

  # Reorder columns
  data.table::setcolorder(
    dt,
    c("id_coord_origin", "id_coord_neighbor", "distance", "distance_class")
  )

  return(dt)
}
