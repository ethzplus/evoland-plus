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
    terra::vect(crs = terra::crs(x)) # TODO we need to add an EPSG attr to coords

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
    terra::vect(crs = terra::crs(x)) # TODO we need to add an EPSG attr to coords

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

# TODO move this to a neighbors_t.R file that includes a formal class definition
# for the return table, including validation, print, coercion
