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

    ### Evoland-specific methods ----

    #' @description
    #' Fetch data from storage with evoland-specific view support
    #' @param table_name Character string. Name of the table to query.
    #' @param where Character string. Optional WHERE clause for the SQL query.
    #' @param limit integerish, limit the amount of rows to return
    #'
    #' @return A data.table
    fetch = function(table_name, where = NULL, limit = NULL) {
      # Check if this is a view that needs special handling
      view_result <- switch(
        table_name,
        lulc_meta_long_v = make_lulc_meta_long_v(self, private, where),
        pred_sources_v = make_pred_sources_v(self, private, where),
        transitions_v = make_transitions_v(self, private, where),
        NULL
      )

      if (!is.null(view_result)) {
        return(view_result)
      }

      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        return(private$get_empty_table(table_name))
      }

      # Call parent method
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
    },

    #' @description
    #' Print method for evoland_db
    #' @param ... Not used
    #' @return self (invisibly)
    print = function(...) {
      cat("<evoland_db> (extends parquet_duckdb)\n")
      cat(sprintf("Database path: %s\n", self$path))
      cat(sprintf("Default format: %s\n\n", self$default_format))

      cat("Database methods (inherited):\n")
      cat("  Commit: commit_overwrite(x, table_name, autoincrement_cols, map_cols),\n")
      cat("          commit_append(x, table_name, autoincrement_cols, map_cols),\n")
      cat("          commit_upsert(x, table_name, key_cols, autoincrement_cols, map_cols)\n")
      cat("  Fetch:  fetch(table_name, where, limit)\n")
      cat("  Delete: delete_from(table_name, where)\n")
      cat("  Other:  list_tables(), execute(statement), get_query(statement),\n")
      cat("          attach_table(table_name, columns), detach_table(table_name),\n")
      cat("          row_count(table_name)\n\n")

      cat("Set / Add methods:\n")
      cat("  set_report(...), set_coords(type, ...),\n")
      cat("  set_periods(period_length_str, start_observed, end_observed, end_extrapolated),\n")
      cat("  add_predictor(pred_spec, pred_data, pred_type)\n\n")

      cat("Active bindings (read-write):\n")
      cat("  coords_t, periods_t, lulc_meta_t, lulc_data_t, pred_data_t_float,\n")
      cat("  pred_data_t_int, pred_data_t_bool, pred_meta_t, trans_meta_t, trans_preds_t,\n")
      cat("  intrv_meta_t, intrv_masks_t, trans_models_t, alloc_params_t\n\n")

      cat("Active bindings (read-only):\n")
      cat("  extent, coords_minimal, lulc_meta_long_v, pred_sources_v\n\n")

      cat(sprintf("Tables: %d\n", length(self$list_tables())))

      invisible(self)
    }
  ),

  ## Active Bindings ----
  active = list(
    ### Bindings for tables ----
    #' @field coords_t A `coords_t` instance; see [create_coords_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    coords_t = function(x) {
      create_active_binding(self, "coords_t", as_coords_t)(x)
    },

    #' @field periods_t A `periods_t` instance; see [create_periods_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    periods_t = function(x) {
      create_active_binding(self, "periods_t", as_periods_t)(x)
    },

    #' @field lulc_meta_t A `lulc_meta_t` instance; see [create_lulc_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    lulc_meta_t = function(x) {
      create_active_binding(self, "lulc_meta_t", as_lulc_meta_t)(x)
    },

    #' @field lulc_meta_long_v Return a `lulc_meta_long_v` instance, i.e. unrolled `lulc_meta_t`.
    lulc_meta_long_v = function() {
      make_lulc_meta_long_v(self, private)
    },

    #' @field lulc_data_t A `lulc_data_t` instance; see [as_lulc_data_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    lulc_data_t = function(x) {
      create_active_binding(self, "lulc_data_t", as_lulc_data_t)(x)
    },

    #' @field transitions_v Get the transitions from `lulc_data_t`.
    transitions_v = function() {
      make_transitions_v(self, private)
    },

    #' @field pred_data_t_float A `pred_data_t_float` instance; see
    #' [create_pred_data_t()] for the type of object to assign. Assigning is an
    #' upsert operation.
    pred_data_t_float = function(x) {
      create_active_binding(
        self,
        "pred_data_t_float",
        as_pred_data_t,
        type = "float"
      )(x)
    },

    #' @field pred_data_t_int A `pred_data_t_int` instance; see
    #' [create_pred_data_t()] for the type of object to assign. Assigning is an
    #' upsert operation.
    pred_data_t_int = function(x) {
      create_active_binding(
        self,
        "pred_data_t_int",
        as_pred_data_t,
        type = "int"
      )(x)
    },

    #' @field pred_data_t_bool A `pred_data_t_bool` instance; see
    #' [create_pred_data_t()] for the type of object to assign. Assigning is an
    #' upsert operation.
    pred_data_t_bool = function(x) {
      create_active_binding(
        self,
        "pred_data_t_bool",
        as_pred_data_t,
        type = "bool"
      )(x)
    },

    ### Bindings for descriptions, views, etc. ----
    #' @field extent Return a terra SpatExtent based on coords_t
    extent = function() {
      make_extent_db(self, private)
    },

    #' @field coords_minimal data.table with only (id_coord, lon, lat)
    coords_minimal = function() {
      make_coords_minimal(self, private)
    },

    #' @field pred_meta_t A `pred_meta_t` instance; see [create_pred_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    pred_meta_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("pred_meta_t")
        return(as_pred_meta_t(x))
      }
      stopifnot(inherits(x, "pred_meta_t"))
      self$commit_upsert(
        x,
        table_name = "pred_meta_t",
        key_cols = "name",
        autoincrement_cols = "id_pred"
      )
    },

    #' @field pred_sources_v Retrieve a table of distinct predictor urls and their
    #' md5sum
    pred_sources_v = function() {
      make_pred_sources_v(self, private)
    },

    #' @field trans_meta_t A `trans_meta_t` instance; see [create_trans_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    trans_meta_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("trans_meta_t")
        return(as_trans_meta_t(x))
      }
      stopifnot(inherits(x, "trans_meta_t"))
      self$commit_upsert(
        x,
        table_name = "trans_meta_t",
        key_cols = c("id_lulc_anterior", "id_lulc_posterior"),
        autoincrement_cols = "id_trans"
      )
    },

    #' @field trans_preds_t A `trans_preds_t` instance; see [create_trans_preds_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    trans_preds_t = function(x) {
      create_active_binding(self, "trans_preds_t", as_trans_preds_t)(x)
    },

    #' @field intrv_meta_t A `intrv_meta_t` instance; see [create_intrv_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    intrv_meta_t = function(x) {
      if (missing(x)) {
        return(as_intrv_meta_t(
          convert_list_cols(self$fetch("intrv_meta_t"), "params", kv_df_to_list)
        ))
      }
      stopifnot(inherits(x, "intrv_meta_t"))

      self$commit_upsert(x, "intrv_meta_t", key_cols = "id_intrv", map_cols = "params")
    },

    #' @field intrv_masks_t A `intrv_masks_t` instance; see [as_intrv_masks_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    intrv_masks_t = function(x) {
      create_active_binding(self, "intrv_masks_t", as_intrv_masks_t)(x)
    },

    #' @field trans_models_t A `trans_models_t` instance; see [create_trans_models_t()] for the type
    #' of object to assign. Assigning is an upsert operation.
    trans_models_t = function(x) {
      if (missing(x)) {
        return(as_trans_models_t(
          convert_list_cols(
            self$fetch("trans_models_t"),
            c("model_params", "goodness_of_fit"),
            kv_df_to_list
          )
        ))
      }
      stopifnot(inherits(x, "trans_models_t"))

      self$commit_upsert(
        x,
        "trans_models_t",
        key_cols = c("id_trans", "id_period"),
        map_cols = c("model_params", "goodness_of_fit")
      )
    },

    #' @field alloc_params_t A `alloc_params_t` instance; see [as_alloc_params_t()] for the type
    #' of object to assign. Assigning is an upsert operation.
    alloc_params_t = function(x) {
      if (missing(x)) {
        return(as_alloc_params_t(
          convert_list_cols(
            self$fetch("alloc_params_t"),
            c("alloc_params", "goodness_of_fit"),
            kv_df_to_list
          )
        ))
      }
      stopifnot(inherits(x, "alloc_params_t"))

      self$commit_upsert(
        x,
        "alloc_params_t",
        key_cols = c("id_trans", "id_period"),
        map_cols = c("alloc_params", "goodness_of_fit")
      )
    }
  ),

  ## Private Methods ----
  private = list(
    # Get empty table with proper structure (evoland-specific)
    #
    # param table_name Character string table name
    # return Empty data.table with correct columns
    get_empty_table = function(table_name) {
      # Define empty table structures
      empty_tables <- list(
        reporting_t = suppressWarnings(data.table::data.table(
          key = character(0),
          value = character(0)
        )),
        coords_t = as_coords_t(),
        periods_t = as_periods_t(),
        lulc_meta_t = as_lulc_meta_t(),
        lulc_data_t = as_lulc_data_t(),
        pred_meta_t = as_pred_meta_t(),
        pred_data_t_float = as_pred_data_t(type = "float"),
        pred_data_t_int = as_pred_data_t(type = "int"),
        pred_data_t_bool = as_pred_data_t(type = "bool"),
        trans_meta_t = as_trans_meta_t(),
        trans_preds_t = as_trans_preds_t(),
        intrv_meta_t = as_intrv_meta_t(),
        intrv_masks_t = as_intrv_masks_t(),
        trans_models_t = as_trans_models_t(),
        alloc_params_t = as_alloc_params_t()
      )

      if (table_name %in% names(empty_tables)) {
        return(empty_tables[[table_name]])
      }

      # Default: return empty data.table
      data.table::data.table()
    }
  )
)

# Helper function to create simple Bindings with standard fetch/commit pattern
# param self The R6 instance (self).
# param table_name Character string. Name of the table.
# param as_fn Function to convert fetched data to the appropriate type.
# param ... Additional arguments passed to as_fn.
# return A function suitable for use as an active binding.
create_active_binding <- function(self, table_name, as_fn, ...) {
  extra_args <- list(...)
  function(x) {
    if (missing(x)) {
      x <- self$fetch(table_name)
      return(do.call(as_fn, c(list(x), extra_args)))
    }
    stopifnot(inherits(x, table_name))
    self$commit_upsert(x, table_name)
  }
}


# Helper functions for converting between list and data.frame formats for DuckDB MAPs
convert_list_cols <- function(x, cols, fn) {
  for (col in cols) {
    x[[col]] <- lapply(x[[col]], fn)
  }
  x
}

list_to_kv_df <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(data.frame(
      key = character(0),
      value = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    key = names(x),
    value = as.character(unlist(x)),
    stringsAsFactors = FALSE
  )
}

kv_df_to_list <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(NULL)
  }

  out <- list()

  for (row in seq_len(nrow(x))) {
    key <- x$key[row]
    val <- x$value[row]

    # Try numeric conversion
    num_val <- suppressWarnings(as.numeric(val))
    if (!is.na(num_val)) {
      out[[key]] <- num_val
    } else {
      out[[key]] <- val
    }
  }

  out
}
