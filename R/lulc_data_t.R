#' Create LULC Data Table
#'
#' Creates an `lulc_data_t` table and validates that it matches the schema.
#'
#' @name lulc_data_t
#'
#' @param x An object that can be passed to [data.table::setDT()]
#'
#' @return A data.table of class "lulc_data_t" with columns:
#'   - `id_run`: Foreign key to runs_t
#'   - `id_coord`: Foreign key to coords_t
#'   - `id_lulc`: Foreign key to lulc_meta_t
#'   - `id_period`: Foreign key to periods_t
#'
#' @seealso [evoland_db] method `$lulc_data_as_rast()` for converting to terra SpatRast
#' @export
as_lulc_data_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_coord = integer(0),
      id_lulc = integer(0),
      id_period = integer(0)
    )
  }
  cast_dt_col(x, "id_coord", "int")
  new_evoland_table(
    x,
    "lulc_data_t",
    c("id_coord", "id_lulc", "id_period")
  )
}

#' @export
validate.lulc_data_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_coord",
      "id_lulc",
      "id_period"
    )
  )

  stopifnot(
    is.integer(x[["id_coord"]]),
    is.integer(x[["id_lulc"]]),
    is.integer(x[["id_period"]]),
    !anyDuplicated(x, by = c("id_coord", "id_lulc", "id_period"))
  )

  return(x)
}

#' @export
#' @describeIn lulc_data_t Print an lulc_data_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.lulc_data_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_coords <- data.table::uniqueN(x[["id_coord"]])
    n_lulc <- data.table::uniqueN(x[["id_lulc"]])
    n_periods <- data.table::uniqueN(x[["id_period"]])

    cat(glue::glue(
      "LULC Data Table\n",
      "Observations: {nrow(x)}\n",
      "Coordinates: {n_coords}, LULC classes: {n_lulc}, Periods: {n_periods}\n\n"
    ))
  } else {
    cat("LULC Data Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}

lulc_data_as_rast <- function(self, extent = NULL, resolution = NULL, id_period = NULL) {
  self$with_tables(
    c("lulc_data_t", "coords_t"),
    function() {
      # (re)construct raster template
      meta <- self$get_table_metadata("coords_t")
      if (is.null(resolution)) {
        resolution <- meta[["resolution"]]
        stopifnot(
          "no resolution associated with coords_t, set manually!" = is.numeric(resolution)
        )
      }
      if (is.null(extent)) {
        if (!is.null(meta[["xmin"]])) {
          extent <- terra::ext(c(
            xmin = meta[["xmin"]],
            xmax = meta[["xmax"]],
            ymin = meta[["ymin"]],
            ymax = meta[["ymax"]]
          ))
        } else {
          # add padding; terra coordinate points are cell centers
          extent <- self$extent |> terra::extend(resolution / 2)
        }
      }

      rast_template <- terra::rast(
        x = extent,
        crs = paste0("epsg:", meta[["epsg"]]),
        resolution = resolution
      )

      # Build query to join lulc_data_t with coords_t
      where_clause <- ""
      if (!is.null(id_period)) {
        period_list <- toString(as.integer(id_period))
        where_clause <- glue::glue("WHERE lulc.id_period IN ({period_list})")
      }

      dat <- self$get_query(glue::glue(
        r"(
          select
            coords.lon,
            coords.lat,
            lulc.id_lulc,
            lulc.id_period
          from
            lulc_data_t as lulc
          inner join
            coords_t as coords
            on lulc.id_coord = coords.id_coord
          {where_clause}
          order by lulc.id_period, coords.lon, coords.lat
          )"
      ))

      if (nrow(dat) == 0L) {
        stop("No LULC data found for the specified periods")
      }

      # Check if multiple points fell in same cells
      for (period in unique(dat[["id_period"]])) {
        cell_ids <- terra::cellFromXY(rast_template, dat[id_period == period, .(lon, lat)])
        if (anyDuplicated(cell_ids)) {
          warning(
            "Multiple points fell within the same raster cell for period ",
            period,
            ". This suggests the resolution (",
            resolution,
            ") is too coarse. Consider using a finer resolution. ",
            "Keeping first value per cell.",
            call. = FALSE
          )
        }
      }

      terra::rasterize(
        x = dat[, c("lon", "lat")],
        y = rast_template,
        value = dat[["id_lulc"]],
        fun = "first",
        by = paste0("id_period_", dat[["id_period"]])
      )
    }
  )
}
