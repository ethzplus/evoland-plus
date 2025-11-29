#' R6 Class for Folder-Based Data Storage Interface
#'
#' @description
#' An R6 class that provides an interface to a folder-based data storage system
#' for the evoland package. Each table is stored as a parquet (or CSV) file.
#' This class uses DuckDB for in-memory SQL operations while persisting data
#' to disk in parquet format for better compression.
#'
#' Inherits from [parquet_duckdb] for generic database operations.
#'
#' @seealso
#' Additional methods and active bindings are added to this class in separate files:
#'
#' - [evoland_db_tables] - Table active bindings (coords_t, lulc_data_t, etc.)
#' - [evoland_db_views] - View active bindings (lulc_meta_long_v, etc.) and methods
#' - [evoland_db_neighbors] - Neighbor analysis methods
#'
#' @include parquet_duckdb.R
#' @export

evoland_db <- R6::R6Class(
  classname = "evoland_db",
  inherit = parquet_duckdb,

  ## Public Methods ----
  public = list(
    #' @description
    #' Initialize a new evoland_db object
    #' @param path Character string. Path to the data folder.
    #' @param default_format Character. Default file format ("parquet" or "csv").
    #' Default is "parquet".
    #' @param ... passed on to `set_report`
    #'
    #' @return A new `evoland_db` object
    initialize = function(
      path,
      default_format = c("parquet", "csv"),
      ...
    ) {
      # Initialize parent class with spatial extension
      super$initialize(
        path = path,
        default_format = default_format,
        extensions = "spatial"
      )

      # Set evoland-specific reporting metadata
      self$set_report(...)

      invisible(self)
    },

    #' @description
    #' Fetch data from storage with evoland-specific view support
    #' @param table_name Character string. Name of the table to query.
    #' @param where Character string. Optional WHERE clause for the SQL query.
    #' @param limit integerish, limit the amount of rows to return
    #'
    #' @return A data.table
    fetch = function(table_name, where = NULL, limit = NULL) {
      # Check if this is a view (active binding)
      if (
        # TODO these should probably not be active bindings, but instead methods with
        # predefined query parameters
        table_name %in%
          c("lulc_meta_long_v", "pred_sources_v", "transitions_v", "extent", "coords_minimal")
      ) {
        return(self[[table_name]])
      }

      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        stop("Table `", table_name, "` does not exist")
      }

      super$fetch(table_name, where, limit)
    },

    ### Setter methods ----
    #' @description
    #' Set reporting metadata
    #' @param ... each named argument is entered into the table with the argument name
    #' as its key
    set_report = function(...) {
      params <- list(...)
      if (self$row_count("reporting_t") == 0L) {
        # only upsert if these values are missing upon DB init
        params[["report_name"]] <-
          params[["report_name"]] %||% "evoland_scenario"
        params[["report_name_pretty"]] <-
          params[["report_name_pretty"]] %||% "Default Evoland Scenario"
        params[["report_include_date"]] <-
          params[["report_include_date"]] %||% "TRUE"
        params[["creator_username"]] <-
          params[["creator_username"]] %||% Sys.getenv("USER", unset = "unknown")
      }
      params[["last_opened"]] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      params[["last_opened_username"]] <- Sys.getenv("USER", unset = "unknown")

      self$commit_upsert(
        data.table::as.data.table(list(
          key = names(params), # cannot name a column "key" in data.table()
          value = unlist(params)
        )),
        table_name = "reporting_t",
        key_cols = "key"
      )
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

      self$commit_overwrite(as_coords_t(create_fun(...)), "coords_t")
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

      self$commit_append(
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

      data.table::set(pred_data, j = "id_pred", value = existing_meta[["id_pred"]])
      data.table::setcolorder(pred_data, c("id_pred", "id_coord", "id_period", "value"))

      self$commit_upsert(
        as_pred_data_t(pred_data, pred_type),
        paste0("pred_data_t_", pred_type)
      )
    }
  )
)
