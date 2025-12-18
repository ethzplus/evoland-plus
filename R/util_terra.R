#' evoland utility functions to work with terra objects
#'
#' Useful to coax terra raster and vector data into evoland tabular form.
#'
#' @name util_terra
NULL

#' @describeIn util_terra Extract values from a SpatRaster or SpatVector object using a (minimal) `coords_t`
#' @param x The object to extract from; use "simple" extraction for rasters, i.e. no resampling is done.
#' @param coords_t A coords_t object containing coordinate points with an "epsg" attribute
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
    terra::vect(crs = paste0("epsg:", attr(coords_t, "epsg")))

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
    terra::vect(crs = paste0("epsg:", attr(coords_t, "epsg")))

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

#' Convert tabular LULC data to raster
#'
#' @description
#' Converts a `lulc_data_t` table (with coordinates) to a SpatRaster.
#' Useful for spatial analysis and validation that requires raster format.
#'
#' @param lulc_data A `lulc_data_t` object or data.table with columns:
#'   id_coord, id_lulc, (optionally id_period)
#' @param coords_t A coords_t object with coordinate information. Must have
#'   `epsg` and optionally `resolution` attributes.
#' @param resolution Numeric, raster resolution in CRS units. If NULL,
#'   attempts to infer from coords_t attributes or data spacing.
#'
#' @return SpatRaster with LULC values. If multiple periods present in lulc_data,
#'   returns a multi-layer raster with one layer per period.
#'
#' @export
tabular_to_raster <- function(lulc_data, coords_t, resolution = NULL) {
  # Validate input
  stopifnot(
    "lulc_data must have id_coord and id_lulc columns" = all(
      c("id_coord", "id_lulc") %in% names(lulc_data)
    ),
    "coords_t must have epsg attribute" = !is.null(attr(coords_t, "epsg"))
  )

  epsg <- attr(coords_t, "epsg")

  # Merge with coordinates to get lon/lat
  dat <- merge(
    lulc_data,
    coords_t[, .(id_coord, lon, lat)],
    by = "id_coord"
  )

  # Determine resolution if not provided
  if (is.null(resolution)) {
    # Try to get from coords_t attribute
    resolution <- attr(coords_t, "resolution")

    if (is.null(resolution)) {
      # Estimate from minimum distance between points
      sample_coords <- coords_t[seq_len(min(1000L, nrow(coords_t)))]
      dists <- as.matrix(dist(sample_coords[, .(lon, lat)]))
      diag(dists) <- Inf
      resolution <- min(dists[dists > 0]) * 0.9 # Slightly smaller than min distance
    }
  }

  # Get extent
  extent <- terra::ext(
    min(coords_t$lon) - resolution / 2,
    max(coords_t$lon) + resolution / 2,
    min(coords_t$lat) - resolution / 2,
    max(coords_t$lat) + resolution / 2
  )

  # Create raster template
  rast_template <- terra::rast(
    x = extent,
    crs = paste0("epsg:", epsg),
    resolution = resolution
  )

  # Check if we have multiple periods
  has_periods <- "id_period" %in% names(dat)

  if (has_periods) {
    periods <- sort(unique(dat[["id_period"]]))

    # Create multi-layer raster
    rast_list <- list()

    for (period in periods) {
      period_data <- dat[id_period == period]

      # Rasterize
      period_rast <- terra::rasterize(
        x = as.matrix(period_data[, .(lon, lat)]),
        y = rast_template,
        values = period_data[["id_lulc"]],
        fun = "first"
      )

      names(period_rast) <- paste0("period_", period)
      rast_list[[length(rast_list) + 1L]] <- period_rast
    }

    # Combine into multi-layer raster
    rast <- terra::rast(rast_list)
  } else {
    # Single layer
    rast <- terra::rasterize(
      x = as.matrix(dat[, .(lon, lat)]),
      y = rast_template,
      values = dat[["id_lulc"]],
      fun = "first"
    )

    names(rast) <- "id_lulc"
  }

  rast
}
