#' evoland utility functions to work with terra objects
#'
#' Useful to coax terra raster and vector data into evoland tabular form.
#'
#' @name util_terra
NULL

#' @describeIn util_terra Extract values from a SpatRaster or SpatVector object using a
#' (minimal) `coords_t` object. Returns a long data.table with `id_coord`,
#' `layer`/`attribute`, and `value`.
#' @param x The object to extract from; use "simple" extraction for rasters, i.e. no
#' resampling is done.
#' @param coords A coords object containing coordinate points with an "epsg" attribute
#' @param na_omit Logical, whether to omit rows with NA values in the output
#' @export
extract_using_coords_t <- function(x, coords, na_omit = TRUE) {
  UseMethod("extract_using_coords_t")
}

#' @exportS3Method
extract_using_coords_t.SpatRaster <- function(x, coords, na_omit = TRUE) {
  # read by cell index rather than through point geometries, which cost several times the
  # memory (ethzplus/evoland-plus#86)
  cells <- coords_cells(coords, x)
  out <-
    data.table::data.table(id_coord = cells[["id_coord"]], terra::extract(x, cells[["cell"]])) |>
    data.table::melt(
      id.vars = "id_coord",
      variable.name = "layer",
      value.name = "value"
    )

  if (na_omit) {
    return(stats::na.omit(out, cols = "value"))
  }

  out
}

#' @exportS3Method
extract_using_coords_t.SpatVector <- function(x, coords, na_omit = TRUE) {
  if ("id_coord" %in% names(x)) {
    stop("x cannot have an id_coord attribute")
  }
  pts <-
    coords[, .(lon, lat)] |>
    as.matrix() |>
    terra::vect(crs = paste0("epsg:", attr(coords, "epsg")))

  tmp <-
    terra::extract(
      x = x,
      y = pts
    ) |>
    data.table::as.data.table() |>
    merge(
      coords[, .(id.y = .I, id_coord)],
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
    return(stats::na.omit(out, cols = "value"))
  }

  out
}

#' @describeIn util_terra Converts a table with id_coord information to a SpatRaster.
#' Useful for spatial analysis and validation that requires raster format.
#'
#' @param data A data.table with column id_coord and value_col
#' @param coords A [coords_t] object with coordinate information. Must have
#'   `epsg` and optionally `resolution` attributes.
#' @param value_col Character, name of column in data to use as raster values. Default
#' "id_lulc".
#' @param resolution Numeric, raster resolution in CRS units. If `NULL`,
#'   use "resolution" attribute on coords, or estimate from coord spacing.
#'
#' @return [terra::SpatRaster] with LULC values. If multiple periods present in data,
#'   returns a multi-layer raster with one layer per period.
#'
#' @export
tabular_to_raster <- function(data, coords, value_col = "id_lulc", resolution = NULL) {
  # Validate input
  stopifnot(
    "data must inherit from data.table" = inherits(data, "data.table"),
    "data must have id_coord column" = "id_coord" %in% names(data),
    "data must have value column" = value_col %in% names(data),
    "coords must have epsg attribute" = !is.null(attr(coords, "epsg"))
  )

  grid <- coords_grid(coords, resolution)

  # if more than 2 columns, we have columns that are not id_coord or value
  is_multilayer <- length(names(data)) > 2L

  if (is_multilayer) {
    # we dcast (pivot wider) to obtain n layers:
    # n cols for each tuple of grouping_cols
    # m rows along all id_coord
    grouping_cols <- setdiff(names(data), c("id_coord", value_col))
    formula_str <- paste("id_coord ~", paste(grouping_cols, collapse = " + "))
    data <- data.table::dcast(
      data = data,
      formula = stats::as.formula(formula_str),
      value.var = value_col
    )
    # splice in grouping cols into names - will become layer names
    layernames <-
      names(data)[-1] |>
      strsplit(split = "_") |>
      vapply(\(x) paste0(grouping_cols, "_", x, collapse = "_"), character(1))
    names(data) <- c("id_coord", layernames)
  } else {
    layernames <- value_col
  }

  lapply(layernames, \(layername) grid_to_rast(grid, data, layername)) |>
    terra::rast()
}

#' Raster grid of a coordinate table
#'
#' @description
#' Internal helpers for the code that works on rasters (the CLUMPY and Dinamica allocators,
#' [tabular_to_raster()]): a raster covering the coordinate points, and the raster cell of each
#' point, to move values between rows keyed by `id_coord` and raster cells.
#'
#' - `coords_grid()` builds the grid.
#' - `coords_cells()` finds the cell of every coordinate point in any raster `x`; points
#'   outside it get `NA`. [extract_using_coords_t()] reads rasters through it.
#' - `grid_values()` lays values given per coordinate point out over every cell of the grid,
#'   in terra's cell order; cells without a coordinate point, or without a value, are `NA`.
#' - `grid_to_rast()` does the same, but returns a single-layer raster.
#' - `rast_to_grid()` reads a raster on the grid back into one value per coordinate point.
#'
#' The cells are only meaningful for a regular grid of points: on any other set of points,
#' several points can fall into one cell.
#'
#' @param coords A [coords_t] (or `db$coords_minimal`): `id_coord`, `lon`, `lat` and an `epsg`
#'   attribute.
#' @param resolution Numeric scalar or `NULL`; the cell size in CRS units. `NULL` uses the
#'   `resolution` attribute of `coords`, or else the smallest distance among its first 1000
#'   points.
#' @param grid A list as returned by `coords_grid()`: `template`, an empty
#'   [terra::SpatRaster], and `cells`, a data.table with `id_coord` and `cell`.
#' @param x For `grid_values()` and `grid_to_rast()`, a data.table with `id_coord` and
#'   `value_col`; for `coords_cells()`, any [terra::SpatRaster]; for `rast_to_grid()`, a
#'   single-layer [terra::SpatRaster] on the grid.
#' @param value_col Character scalar, the column of `x` holding the values.
#'
#' @return `coords_grid()`: the grid, see `grid`. `coords_cells()`: a data.table with
#'   `id_coord` and `cell`. `grid_values()`: a vector of the type of
#'   `x[[value_col]]`, one element per cell. `grid_to_rast()`: a [terra::SpatRaster] with one
#'   layer named `value_col`. `rast_to_grid()`: a data.table with `id_coord` and `value`.
#'
#' @name coords_grid
#' @keywords internal
coords_grid <- function(coords, resolution = NULL) {
  if (is.null(resolution)) {
    resolution <- attr(coords, "resolution")
    if (is.null(resolution)) {
      # estimate from first 1000 points
      resolution <-
        coords[seq_len(min(1000L, nrow(coords))), .(lon, lat)] |>
        stats::dist() |> # lower triangular distance matrix
        min() # minimum distance
    }
  }

  template <- terra::rast(
    x = terra::ext(
      min(coords[["lon"]]) - resolution / 2,
      max(coords[["lon"]]) + resolution / 2,
      min(coords[["lat"]]) - resolution / 2,
      max(coords[["lat"]]) + resolution / 2
    ),
    crs = paste0("epsg:", attr(coords, "epsg")),
    resolution = resolution
  )

  list(template = template, cells = coords_cells(coords, template))
}

#' @rdname coords_grid
coords_cells <- function(coords, x) {
  data.table::data.table(
    id_coord = coords[["id_coord"]],
    cell = terra::cellFromXY(x, as.matrix(coords[, .(lon, lat)]))
  )
}

#' @rdname coords_grid
grid_values <- function(grid, x, value_col) {
  # the cell of every coordinate point that carries a value
  valued_cells <- grid[["cells"]][
    x[, .(id_coord, value = .SD[[1L]]), .SDcols = value_col],
    on = "id_coord",
    nomatch = NULL
  ]
  # every cell of the raster, NA unless it holds such a point
  data.table::data.table(cell = seq_len(terra::ncell(grid[["template"]])))[
    valued_cells,
    on = "cell",
    value := i.value
  ][["value"]]
}

#' @rdname coords_grid
grid_to_rast <- function(grid, x, value_col) {
  terra::rast(grid[["template"]], vals = grid_values(grid, x, value_col)) |>
    stats::setNames(value_col)
}

#' @rdname coords_grid
rast_to_grid <- function(grid, x) {
  grid[["cells"]][, .(id_coord, value = terra::extract(x, cell)[[1L]])]
}
