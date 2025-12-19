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

#' Convert tabular data with coordinates to raster
#'
#' @description
#' Converts any tabular data (with an id_coord column) to a SpatRaster.
#' Useful for spatial analysis and validation that requires raster format.
#'
#' @param data A data.table with at minimum an `id_coord` column and one or more
#'   value columns to be rasterized. Can optionally include grouping columns
#'   (e.g., id_period, id_pred, id_intrv) which will create separate layers.
#' @param value_col Character string specifying the column name to rasterize.
#'   If NULL (default), attempts to detect a suitable value column by excluding
#'   id_coord, lat, lon, and any grouping columns starting with "id_".
#' @param coords_t A coords_t object with coordinate information. Must have
#'   `epsg` and optionally `resolution` attributes.
#' @param resolution Numeric, raster resolution in CRS units. If NULL,
#'   attempts to infer from coords_t attributes or data spacing.
#' @param layer_cols Character vector of column names to use for creating
#'   separate layers. If NULL (default), automatically detects grouping columns
#'   (all columns starting with "id_" except id_coord). Set to character(0)
#'   to force a single-layer output.
#'
#' @return SpatRaster with values from the specified column. If grouping columns
#'   are present, returns a multi-layer raster with one layer per unique
#'   combination of grouping values.
#'
#' @export
tabular_to_raster <- function(
  data,
  coords_t,
  value_col = NULL,
  resolution = NULL,
  layer_cols = NULL
) {
  # Validate input
  stopifnot(
    "data must have id_coord column" = "id_coord" %in% names(data),
    "coords_t must have epsg attribute" = !is.null(attr(coords_t, "epsg"))
  )

  epsg <- attr(coords_t, "epsg")

  # Auto-detect value column if not specified
  if (is.null(value_col)) {
    excluded_cols <- c("id_coord", "lon", "lat")
    candidate_cols <- setdiff(names(data), excluded_cols)

    # Remove grouping columns (those starting with "id_")
    grouping_cols <- grep("^id_", candidate_cols, value = TRUE)
    candidate_cols <- setdiff(candidate_cols, grouping_cols)

    if (length(candidate_cols) == 0L) {
      stop(
        "Could not auto-detect value column. ",
        "Please specify value_col explicitly."
      )
    } else if (length(candidate_cols) > 1L) {
      stop(
        "Multiple potential value columns found: ",
        paste(candidate_cols, collapse = ", "),
        ". Please specify value_col explicitly."
      )
    }

    value_col <- candidate_cols[1L]
  }

  stopifnot(
    "value_col must be present in data" = value_col %in% names(data)
  )

  # Merge with coordinates to get lon/lat
  dat <- merge(
    data,
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

  # Auto-detect layer columns if not specified
  if (is.null(layer_cols)) {
    # Find all id_* columns except id_coord and the value column
    all_id_cols <- grep("^id_", names(dat), value = TRUE)
    layer_cols <- setdiff(all_id_cols, c("id_coord", value_col))
  }

  # Check if we have grouping columns for layers
  has_layers <- length(layer_cols) > 0L

  if (has_layers) {
    # Create a composite key for unique combinations of layer columns
    dat[, layer_key := do.call(paste, c(.SD, sep = "_")), .SDcols = layer_cols]
    layer_values <- sort(unique(dat[["layer_key"]]))

    # Create multi-layer raster
    rast_list <- list()

    for (layer_val in layer_values) {
      layer_data <- dat[layer_key == layer_val]

      # Rasterize
      layer_rast <- terra::rasterize(
        x = as.matrix(layer_data[, .(lon, lat)]),
        y = rast_template,
        values = layer_data[[value_col]],
        fun = "first"
      )

      # Create descriptive layer name
      if (length(layer_cols) == 1L) {
        # Single grouping column: use column name prefix
        col_name <- sub("^id_", "", layer_cols[1L])
        layer_name <- paste0(col_name, "_", layer_val)
      } else {
        # Multiple grouping columns: use composite key
        layer_name <- layer_val
      }

      names(layer_rast) <- layer_name
      rast_list[[length(rast_list) + 1L]] <- layer_rast
    }

    # Combine into multi-layer raster
    rast <- terra::rast(rast_list)
  } else {
    # Single layer
    rast <- terra::rasterize(
      x = as.matrix(dat[, .(lon, lat)]),
      y = rast_template,
      values = dat[[value_col]],
      fun = "first"
    )

    names(rast) <- value_col
  }

  rast
}
