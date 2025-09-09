#' Create Coordinate Table from Configuration
#'
#' Creates a coords_t table based on the coordinate system specification in an
#' evoland_config object. This function uses the coords field to establish a
#' base grid and dispatches to type-specific implementation methods.
#'
#' @name coords_t
#'
#' @param config An object of class "evoland_config" containing coordinate
#'   system specifications in the coords field.
#'
#' @return A data.table of class "coords_t" with columns:
#'   - `id_coord`: Unique ID for each coordinate pair
#'   - `lon`: Longitude/x coordinate
#'   - `lat`: Latitude/y coordinate
#'   - `elevation`: Elevation (initially NULL)
#'   - `region`: Region designation (initially NULL)
#'   - `geom_point`: Geometry point object
#'   - `geom_polygon`: Geometry polygon object (for grid cells)
#' @export
create_coords_t <- function(config) {
  stopifnot(inherits(config, "evoland_config"))
  coords_spec <- config[["coords"]]
  coord_type <- coords_spec[["type"]]

  # Dispatch to type-specific implementation
  out <- switch(
    coord_type,
    square = create_coords_t_square(coords_spec),
    stop(glue::glue(
      "Unsupported coordinate type: '{coord_type}'. Currently only 'square' is supported."
    ))
  )

  validate(out)
}

create_coords_t_square <- function(coords_spec) {
  stopifnot(
    # Validate required fields
    c("epsg", "extent", "resolution") %in% names(coords_spec),
    rlang::is_scalar_integerish(coords_spec[["epsg"]]),
    is.list(coords_spec[["extent"]]),
    rlang::is_scalar_double(as.double(coords_spec[["resolution"]]))
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
    data.table::as.data.table() |>
    setNames(c("lon", "lat"))

  data.table::set(base_grid_dt, j = "id_coord", value = seq_len(nrow(base_grid_dt)))

  # populate later coords-agnostically with point sampling
  data.table::set(base_grid_dt, j = "elevation", value = NA_real_)

  # populate later coords-agnostically with point sampling
  data.table::set(base_grid_dt, j = "region", value = as.factor(NULL))

  # skipping geom_polygon for the square case
  data.table::set(base_grid_dt, j = "geom_polygon", value = list())

  data.table::setcolorder(
    base_grid_dt,
    c(
      "id_coord",
      "lon",
      "lat",
      "elevation",
      "region",
      "geom_polygon"
    )
  )

  # Add S3 classes
  class(base_grid_dt) <- c(
    "square_coords_t",
    "coords_t",
    "evoland_t",
    class(base_grid_dt)
  )

  attr(base_grid_dt, "epsg") <- coords_spec[["epsg"]]
  attr(base_grid_dt, "xmin") <- coords_spec[["extent"]][["xmin"]]
  attr(base_grid_dt, "xmax") <- coords_spec[["extent"]][["xmax"]]
  attr(base_grid_dt, "ymin") <- coords_spec[["extent"]][["ymin"]]
  attr(base_grid_dt, "ymax") <- coords_spec[["extent"]][["ymax"]]
  attr(base_grid_dt, "resolution") <- coords_spec[["resolution"]]

  return(base_grid_dt)
}

#' @export
validate.coords_t <- function(x, ...) {
  # Check that it's a data.table
  if (!inherits(x, "data.table")) {
    stop("coords_t must inherit from data.table")
  }

  if (!rlang::is_scalar_integerish(attr(x, "epsg"))) {
    stop("EPSG attribute must be integerish")
  }

  # Check required columns
  required_cols <- c("id_coord", "lon", "lat", "elevation", "region", "geom_polygon")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop(glue::glue(
      "coords_t missing required columns: {paste(missing_cols, collapse = ', ')}"
    ))
  }

  # Check column types
  if (!is.integer(x[["id_coord"]])) {
    stop("id_coord must be integer")
  }

  if (!is.numeric(x[["lon"]]) || !is.numeric(x[["lat"]])) {
    stop("lon and lat must be numeric")
  }

  if (!is.factor(x[["region"]])) {
    stop("region must be factor")
  }

  # Check for unique id_coord values
  if (anyDuplicated(x[["id_coord"]])) {
    stop("id_coord values must be unique")
  }

  if (!inherits(x[["geom_polygon"]], "sfc")) {
    warning("`geom_polygon` is not a simple feature collection")
  }
  if (all(is.na(x[["elevation"]]))) {
    # TODO insert name of method for sampling / setting elevation
    warning("`elevation` is all NA, populate using ")
  }
  if (all(is.na(x[["region"]]))) {
    # TODO insert name of method for sampling / setting region
    warning("`region` is all NA, populate using ")
  }

  return(x)
}

#' @export
print.square_coords_t <- function(x, ...) {
  cat(glue::glue(
    "Square Coordinate Table; CRS: EPSG:{attr(x, 'epsg')}\n",
    "Dimensions: ",
    "{attr(x, 'xmax') - attr(x, 'xmin')} by {attr(x, 'ymax') - attr(x, 'ymin')}; ",
    "Resolution: {attr(x, 'resolution')}\n",
    "longitude (x) range: [{attr(x, 'xmin')}, {attr(x, 'xmax')}]\n",
    "latitude  (y) range: [{attr(x, 'ymin')}, {attr(x, 'ymax')}]\n\n"
  ))
  NextMethod(...)
  invisible(x)
}
