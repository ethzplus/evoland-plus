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
  pts <-
    coords[, .(id_coord, lon, lat)] |>
    data.table::as.data.table() |>
    terra::vect(crs = paste0("epsg:", attr(coords, "epsg")))

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
    return(na.omit(out, cols = "value"))
  }

  out
}

#' @describeIn util_terra Converts a table with id_coord information to a SpatRaster.
#' Useful for spatial analysis and validation that requires raster format.
#'
#' @param data A data.table with column id_coord and value_col
#' @param coords A [coords_t] object with coordinate information. Must have
#'   `epsg` and optionally `resolution` attributes.
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

  # Determine resolution if not provided
  if (is.null(resolution)) {
    resolution <- attr(coords, "resolution")
    if (is.null(resolution)) {
      # estimate from first 1000 points
      resolution <-
        coords[seq_len(min(1000L, nrow(coords))), .(lon, lat)] |>
        dist() |> # lower triangular distance matrix
        min() # minimum distance
    }
  }

  extent <- terra::ext(
    min(coords$lon) - resolution / 2,
    max(coords$lon) + resolution / 2,
    min(coords$lat) - resolution / 2,
    max(coords$lat) + resolution / 2
  )

  # Create raster template
  rast_template <- terra::rast(
    x = extent,
    crs = paste0("epsg:", attr(coords, "epsg")),
    resolution = resolution
  )

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
      formula = as.formula(formula_str),
      value.var = value_col
    )
    # splice in grouping cols into names - will become layer names
    names(data) <- c(
      "id_coord",
      names(data)[-1] |>
        strsplit(split = "_") |>
        vapply(\(x) paste0(grouping_cols, "_", x, collapse = "_"), character(1))
    )
  }

  joint <- merge(
    data,
    coords[, .(id_coord, lon, lat)],
    by = "id_coord"
  )

  # Rasterize
  out <- terra::rasterize(
    x = as.matrix(joint[, .(lon, lat)]),
    y = rast_template,
    values = joint[, -c("id_coord", "lon", "lat")],
    fun = "first"
  )

  if (!is_multilayer) {
    names(out) <- value_col
  }
  out
}
