#' Coordinate Tables
#'
#' Create and validate `coords_t` objects, describing the base set of coordinate points
#' upon which your land use model is intended to run. Because the coordinates are
#' declared as points, you can describe sparse domains (e.g. following polity
#' boundaries) or with arbitrary distribution (e.g. square, hexagonal, voronoi, polygon
#' tesselations).
#'
#' @param epsg An integerish scalar representing an EPSG CRS code
#' @param extent A [terra::SpatExtent] object describing the extent of a desired
#'               `coords_t`
#' @param resolution A numeric scalar, describing the required resolution of your
#' `coords_t` object. The unit is that of the epsg.
#'
#' @name coords_t
NULL

#' @describeIn coords_t Cast and validate as coords_t
#' @return A data.table of class "coords_t" with columns:
#'   - `id_coord`: Unique ID for each coordinate pair
#'   - `lon`: Longitude/x coordinate
#'   - `lat`: Latitude/y coordinate
#'   - `elevation`: Elevation (initially NULL)
#'   - `geom_polygon`: Geometry polygon object (for grid cells)
#' @export
as_coords_t <- function(x) {
  data.table::set(x, j = "id_coord", value = as.integer(x[["id_coord"]]))
  new_evoland_table(
    x,
    "coords_t",
    "id_coord"
  )
}

#' @export
validate.coords_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_coord",
      "lon",
      "lat",
      "elevation",
      "geom_polygon"
    )
  )

  stopifnot(
    is.integer(x[["id_coord"]]),
    is.numeric(x[["lon"]]),
    is.numeric(x[["lat"]]),
    !anyDuplicated(x[["id_coord"]])
  )

  return(x)
}

#' @export
#' @describeIn coords_t Print a coords_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.coords_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 1) {
    cat(glue::glue(
      "Coordinate Table\n",
      "longitude (x) range: [{min(x[['lon']])}, {max(x[['lon']])}]\n",
      "latitude  (y) range: [{min(x[['lat']])}, {max(x[['lat']])}]\n\n"
    ))
  } else {
    cat("Coordinate Table\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}

#' @describeIn coords_t Create a set of square coordinates
#' @export
create_coords_t_square <- function(epsg, extent, resolution, ...) {
  if (!rlang::is_scalar_integerish(epsg)) {
    stop("epsg must be scalar integerish")
  }
  if (!rlang::is_scalar_double(resolution)) {
    stop("resolution must be scalar double")
  }
  if (!inherits(extent, "SpatExtent")) {
    stop("extent must be a terra SpatExtent object")
  }
  if (length(list(...)) > 0) {
    stop("Cannot handle additional arguments")
  }

  base_grid_rast <- terra::rast(
    x = extent,
    crs = paste0("epsg:", epsg),
    resolution = resolution
  )

  base_grid_dt <-
    base_grid_rast |>
    terra::crds(na.rm = FALSE) |>
    data.table::as.data.table()
  names(base_grid_dt) <- c("lon", "lat")

  data.table::set(base_grid_dt, j = "id_coord", value = seq_len(nrow(base_grid_dt)))

  # populate later coords-agnostically with point sampling
  data.table::set(base_grid_dt, j = "elevation", value = NA_real_)

  # skipping geom_polygon for the square case
  data.table::set(base_grid_dt, j = "geom_polygon", value = list())

  as_coords_t(base_grid_dt)
}
