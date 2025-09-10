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
create_coords_t <- function(config) {
  coords_spec <- config[["coords"]]
  coord_type <- coords_spec[["type"]]

  # Dispatch to type-specific implementation
  switch(
    coord_type,
    square = create_coords_t_square(coords_spec),
    stop(glue::glue(
      "Unsupported coordinate type: '{coord_type}'. Currently only 'square' is supported."
    ))
  )
}

#' @export
validate.coords_t <- function(x, ...) {
  # Check that it's a data.table
  if (!inherits(x, "data.table")) {
    stop("coords_t must inherit from data.table")
  }

  # Check required columns
  check_missing_names(x, c("id_coord", "lon", "lat", "elevation", "region", "geom_polygon"))

  # Check column types
  if (!is.integer(x[["id_coord"]])) {
    id_ints <- as.integer(x[["id_coord"]])
    if (anyNA(id_ints)) {
      stop("id_coord must be int or coercible to it")
    }
    data.table::set(x, j = "id_coord", value = id_ints)
  }

  if (!is.numeric(x[["lon"]]) || !is.numeric(x[["lat"]])) {
    stop("lon and lat must be numeric")
  }

  if (!is.factor(x[["region"]])) {
    data.table::set(
      x,
      j = "region",
      value = as.factor(x[["region"]])
    )
    warning("coerced region to factor")
  }

  # if empty, don't run soft checks
  if (nrow(x) == 0L) {
    return(x)
  }

  # Check for unique id_coord values
  if (anyDuplicated(x[["id_coord"]])) {
    stop("id_coord values must be unique")
  }

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

# This is the active binding function for coords_t handling in the evoland_db class
utils::globalVariables("self")
active_binding_coords_t <- function(coords_t) {
  if (missing(coords_t)) {
    coords_t <-
      DBI::dbGetQuery(self$connection, "from coords_t") |>
      data.table::as.data.table(key = "id_coord")

    data.table::set(coords_t, j = "region", value = as.factor(coords_t[["region"]]))

    return(new_evoland_table(coords_t, "coords_t"))
  }
  self$commit(coords_t, "coords_t", mode = "upsert")
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
  data.table::setkeyv(base_grid_dt, "id_coord")

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

  new_evoland_table(base_grid_dt, "coords_t")
}
