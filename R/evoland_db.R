#' R6 Class for Folder-Based Data Storage Interface
#'
#' @description
#' An R6 class that provides an interface to a folder-based data storage system
#' for the evoland package. Each table is stored as a parquet (or JSON) file.
#' This class uses DuckDB for in-memory SQL operations while persisting data
#' to disk in parquet format for better compression.
#'
#' Inherits from [parquet_db] for generic database operations.
#'
#' @seealso
#' Additional methods and active bindings are added to this class in separate files:
#'
#' - [evoland_db_tables] - Table active bindings (coords_t, lulc_data_t, etc.)
#' - [evoland_db_views] - View active bindings (lulc_meta_long_v, etc.) and methods
#' - [evoland_db_neighbors] - Neighbor analysis methods
#'
#' @include parquet_db.R
#' @export

evoland_db <- R6::R6Class(
  classname = "evoland_db",
  inherit = parquet_db,

  ## Public Methods ----
  public = list(
    #' @description
    #' Initialize a new evoland_db object
    #' @param path Character string. Path to the data folder.
    #' @param ... passed on to `set_report`
    #'
    #' @return A new `evoland_db` object
    initialize = function(
      path,
      ...
    ) {
      # Initialize parent class with spatial extension
      super$initialize(
        path = path,
        extensions = "spatial"
      )

      # Set evoland-specific reporting metadata
      self$set_report(...)

      invisible(self)
    },

    ### Setter methods ----
    #' @description
    #' Set reporting metadata
    #' @param ... each named argument is entered into the table with the argument name
    #' as its key
    set_report = function(...) {
      db_set_report(self, ...)
    },

    #' @description
    #' Set coordinates for DB. Cannot overwrite existing table (would mean cascading deletion)
    #' @param type string; which type of coordinates to set, see [coords_t]
    #' @param ... named arguments are passed to the appropriate coordinate creator function
    set_coords = function(type = c("square"), ...) {
      if (self$row_count("coords_t") > 0L) {
        warning("coords_t is not empty! Refusing to overwrite; start with fresh DB")
        return(invisible(NULL))
      }
      create_fun <- switch(
        type,
        square = create_coords_t_square,
        function(x) stop("Unsupported coordinate type specified.")
      )

      self$commit(as_coords_t(create_fun(...)), "coords_t", method = "overwrite")
    },

    #' @description
    #' Set periods for DB. See [`periods_t`]
    #' @param period_length_str ISO 8601 duration string specifying the length of each
    #' period (currently only accepting years, e.g., "P5Y" for 5 years)
    #' @param start_observed Start date of the observed data (YYYY-MM-DD)
    #' @param end_observed End date of the observed data (YYYY-MM-DD)
    #' @param end_extrapolated End date for extrapolation time range (YYYY-MM-DD)
    set_periods = function(
      period_length_str = "P10Y",
      start_observed = "1985-01-01",
      end_observed = "2020-01-01",
      end_extrapolated = "2060-01-01"
    ) {
      if (self$row_count("periods_t") > 0L) {
        warning("periods_t is not empty! Refusing to overwrite; start with fresh DB")
        return(invisible(NULL))
      }

      self$commit(
        do.call(create_periods_t, as.list(environment())),
        "periods_t"
      )
    },

    ### Adder methods ----
    #' @description
    #' Add a predictor to the database
    #' @param pred_spec List of predictor specification; see [create_pred_meta_t()]
    #' @param pred_data An object that can be coerced to [`pred_data_t`], but doesn't have an
    #' `id_pred`
    #' @param pred_type Passed to [as_pred_data_t()]; one of float, int, bool
    add_predictor = function(pred_spec, pred_data, pred_type) {
      stopifnot(length(pred_spec) == 1)

      self$pred_meta_t <- create_pred_meta_t(pred_spec)

      existing_meta <- self$fetch(
        "pred_meta_t",
        where = glue::glue("name = '{names(pred_spec)}'")
      )

      data.table::set(pred_data, j = "id_pred", value = as.integer(existing_meta[["id_pred"]]))
      data.table::setcolorder(pred_data, c("id_pred", "id_coord", "id_period", "value"))

      self$commit(
        as_pred_data_t(pred_data, pred_type),
        paste0("pred_data_t_", pred_type),
        method = "upsert"
      )
    },

    ### Export methods ----
    #' @description
    #' Export all spatial tables to GeoTIFF files
    #' @param output_dir Character string. Directory to write GeoTIFF files.
    #'   If NULL (default), creates a "geotiffs" subdirectory in the database path.
    #' @param resolution Numeric, raster resolution in CRS units. If NULL,
    #'   attempts to infer from coords_t attributes.
    #' @param overwrite Logical. Whether to overwrite existing files. Default FALSE.
    #'
    #' @return Character vector of file paths that were written
    export_geotiffs = function(output_dir = NULL, resolution = NULL, overwrite = FALSE) {
      # Set up output directory
      if (is.null(output_dir)) {
        output_dir <- file.path(self$path, "geotiffs")
      }

      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      message("Exporting spatial tables to: ", output_dir)

      # Get coords_t
      coords <- self$coords_t
      if (is.null(coords) || nrow(coords) == 0L) {
        stop("No coordinates found in database. Cannot export spatial data.")
      }

      # List all tables
      all_tables <- self$list_tables()

      # Define tables to export and their value columns
      export_specs <- list(
        lulc_data_t = "id_lulc",
        intrv_masks_t = "id_intrv"
      )

      # Add pred_data_t_* tables
      pred_tables <- grep("^pred_data_t_(float|int|bool)$", all_tables, value = TRUE)
      for (pred_table in pred_tables) {
        export_specs[[pred_table]] <- "value"
      }

      # Add lulc_data_t_* tables (simulation results)
      sim_tables <- grep("^lulc_data_t_", all_tables, value = TRUE)
      for (sim_table in sim_tables) {
        export_specs[[sim_table]] <- "id_lulc"
      }

      # Export each table
      exported_files <- character(0)

      for (table_name in names(export_specs)) {
        if (!table_name %in% all_tables) {
          next
        }

        value_col <- export_specs[[table_name]]
        output_file <- file.path(output_dir, paste0(table_name, ".tif"))

        if (file.exists(output_file) && !overwrite) {
          message("  Skipping ", table_name, " (file exists, use overwrite = TRUE)")
          next
        }

        message("  Exporting ", table_name, "...")

        tryCatch(
          {
            # Fetch table
            table_data <- self$fetch(table_name)

            if (nrow(table_data) == 0L) {
              message("    Skipping (empty table)")
              next
            }

            # Convert to raster
            rast <- tabular_to_raster(
              data = table_data,
              coords_t = coords,
              value_col = value_col,
              resolution = resolution
            )

            # Write to file
            terra::writeRaster(
              rast,
              filename = output_file,
              overwrite = overwrite,
              gdal = c("COMPRESS=LZW")
            )

            exported_files <- c(exported_files, output_file)
            message("    Written: ", basename(output_file), " (", terra::nlyr(rast), " layers)")
          },
          error = function(e) {
            message("    Error exporting ", table_name, ": ", e$message)
          }
        )
      }

      message("Export complete. ", length(exported_files), " files written.")
      invisible(exported_files)
    }
  )
)
