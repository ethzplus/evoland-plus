#' Create Coordinate Table from Configuration
#'
#' Creates a coords_t table based on the coordinate system specification in an
#' evoland_config object. This function uses the coords field to establish a
#' base grid and dispatches to type-specific implementation methods.
#'
#' @name coords_t
#'
#' @param config An [evoland_config] instance
#'
#' @return A data.table of class "coords_t" with columns:
#'   - `id_coord`: Unique ID for each coordinate pair
#'   - `lon`: Longitude/x coordinate
#'   - `lat`: Latitude/y coordinate
#'   - `elevation`: Elevation (initially NULL)
#'   - `region`: Region designation (initially NULL)
#'   - `geom_polygon`: Geometry polygon object (for grid cells)
#' @export
as_coords_t <- function(x) {
  new_evoland_table(
    x,
    "coords_t",
    "id_coord"
  )
}

#' @export
create_coords_t <- function(config) {
  coords_spec <- config[["coords"]]

  # Dispatch to type-specific implementation
  create_fun <- switch(
    coords_spec[["type"]],
    square = create_coords_t_square,
    function(x) stop("Unsupported coordinate type specified.")
  )

  as_coords_t(create_fun(coords_spec))
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
      "region",
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

# Create a new coords_t_square from specs
create_coords_t_square <- function(coords_spec) {
  stopifnot(
    # Validate required fields
    c("epsg", "extent", "resolution") %in% names(coords_spec),
    # quasi rlang::is_scalar_integerish()
    (is.numeric(coords_spec[["epsg"]]) &&
      length(coords_spec[["epsg"]]) == 1 &&
      coords_spec[["epsg"]] == as.integer(coords_spec[["epsg"]])),
    is.list(coords_spec[["extent"]]),
    # quasi rlang::is_scalar_double()
    (is.numeric(coords_spec[["resolution"]]) &&
      length(coords_spec[["resolution"]]) == 1)
  )

  base_grid_rast <- terra::rast(
    xmin = coords_spec[["extent"]][["xmin"]],
    xmax = coords_spec[["extent"]][["xmax"]],
    ymin = coords_spec[["extent"]][["ymin"]],
    ymax = coords_spec[["extent"]][["ymax"]],
    crs = paste0("epsg:", coords_spec[["epsg"]]),
    resolution = coords_spec[["resolution"]]
  )

  base_grid_dt <-
    base_grid_rast |>
    terra::crds(na.rm = FALSE) |>
    data.table::as.data.table()
  names(base_grid_dt) <- c("lon", "lat")

  data.table::set(base_grid_dt, j = "id_coord", value = seq_len(nrow(base_grid_dt)))

  # populate later coords-agnostically with point sampling
  data.table::set(base_grid_dt, j = "elevation", value = NA_real_)

  # populate later coords-agnostically with point sampling
  data.table::set(base_grid_dt, j = "region", value = as.factor(NULL))

  # skipping geom_polygon for the square case
  data.table::set(base_grid_dt, j = "geom_polygon", value = list())

  base_grid_dt
}
