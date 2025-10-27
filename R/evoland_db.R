#' R6 Class for DuckDB Database Interface
#'
#' @description
#' An R6 class that provides an interface to a DuckDB database for the evoland
#' package. This class handles database initialization, data commits, and data
#' fetching operations.
#'
#' @import R6 duckdb
#' @export

evoland_db <- R6::R6Class(
  classname = "evoland_db",

  ## Public Methods ----
  public = list(
    #' @field connection DBI connection object to the DuckDB database
    connection = NULL,

    #' @field path Character string path to the database file
    path = NULL,

    #' @field write_mode Logical indicating if database is opened in write mode
    write_mode = NULL,

    #' @description
    #' Initialize a new evoland_db object
    #' @param path Character string. Path to the DuckDB database file. May also be ":memory:".
    #' @param write Logical. Whether to open the database in write mode. Default is TRUE.
    #'   If FALSE, the database file must already exist.
    #' @param report_name Character string. Name of the report scenario.
    #' @param report_name_pretty Character string. Pretty name of the report scenario.
    #' @param report_include_date Logical. Whether to include the date in the report scenario.
    #' @param report_username Character string. Username for the report scenario,
    #' defaults to $USER env var
    #'
    #' @return A new `evoland_db` object
    initialize = function(
      path,
      write = TRUE,
      report_name = "evoland_scenario",
      report_name_pretty = "Default Evoland Scenario",
      report_include_date = FALSE,
      report_username = Sys.getenv("USER", unset = "unknown")
    ) {
      self$path <- path
      self$write_mode <- write

      # Check if file exists
      file_exists <- file.exists(path)

      # Validate write mode requirements
      if (!write && !file_exists) {
        stop(glue::glue("Database file '{path}' does not exist and write=FALSE"))
      }

      # Create connection
      self$connection <- DBI::dbConnect(
        duckdb::duckdb(),
        dbdir = path,
        read_only = !write
      )

      # If file doesn't exist and we're in write mode, create schema
      if (!file_exists && write) {
        private$create_schema()
      }

      # Upsert report metadata
      reporting_t <- data.table::rowwiseDT(
        key=, value=, # nolint
        "report_name", report_name,
        "report_name_pretty", report_name_pretty,
        "report_include_date", report_include_date,
        "report_username", report_username,
        "last_opened", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )

      self$commit(reporting_t, "reporting_t", mode = "upsert")

      invisible(self)
    },

    #' @description
    #' Commit data to the database
    #' @param x Data object to commit.
    #' @param table_name Table to target
    #' @param mode Character string. One of "upsert" (default), "append", or "overwrite".
    #'
    #' @return NULL (called for side effects)
    commit = function(x, table_name, mode = "upsert") {
      if (!self$write_mode) {
        stop("Database opened in read-only mode. Cannot commit data.")
      }

      # Validate mode
      valid_modes <- c("append", "upsert", "overwrite")
      if (!mode %in% valid_modes) {
        stop(glue::glue(
          "Invalid mode '{mode}'. Must be one of: {paste(valid_modes, collapse = ', ')}"
        ))
      }

      # Handle different commit modes
      switch(
        mode,
        overwrite = private$commit_overwrite(x, table_name),
        upsert = private$commit_upsert(x, table_name),
        append = private$commit_append(x, table_name)
      )

      invisible(NULL)
    },

    #' @description
    #' Fetch data from the database
    #' @param table_name Character string. Name of the database table or view to query.
    #' @param where Character string. Optional WHERE clause for the SQL query.
    #' @param limit integerish, limit the amount of rows to return
    #'
    #' @return A table of the original class
    fetch = function(table_name, where = NULL, limit = NULL) {
      # Build SQL query
      sql <- glue::glue("FROM {table_name}")

      if (!is.null(where)) {
        sql <- glue::glue("{sql} WHERE {where}")
      }
      if (!is.null(limit)) {
        sql <- glue::glue("{sql} LIMIT {limit}")
      }

      DBI::dbGetQuery(self$connection, sql) |>
        data.table::as.data.table()
    },

    #' @description
    #' List all tables in the database
    #' @return Character vector of table names
    list_tables = function() {
      DBI::dbListTables(self$connection)
    },

    #' @description
    #' Execute statement
    #' @param statement A SQL statement
    #' @return No. of rows affected by statement
    execute = function(statement) {
      DBI::dbExecute(self$connection, statement)
    },

    #' @description
    #' Get table row count
    #' @param table_name Character string. Name of the database table or view to query.
    #' @return No. of rows affected by statement
    row_count = function(table_name) {
      qry <- glue::glue("select count() from {table_name};")
      DBI::dbGetQuery(self$connection, qry)[[1]]
    },

    #' @description
    #' Empty a table
    #' @param table_name Character string. Name of the database table or view to delete.
    #' @param where Character string, defaults to NULL: delete everything in table.
    #' @return No. of rows affected by statement
    delete_from = function(table_name, where = NULL) {
      qry <- glue::glue("delete from {table_name}")
      if (!is.null(where)) {
        qry <- glue::glue("{qry} where {where}")
      }
      DBI::dbExecute(self$connection, qry)
    },

    #' @description
    #' Copy full DB to a different one
    #' @param target_path Character string. Name of the database file to copy to
    #' @param source_db Character string. Name of the database to copy from. Defaults to
    #' this object's DB path
    copy_db = function(target_path, source_db) {
      if (missing(source_db)) {
        # find source_db from attached databases
        dbs <- DBI::dbGetQuery(self$connection, "pragma database_list;")
        dbs$file <- ifelse(is.na(dbs$file), ":memory:", dbs$file)
        source_db <- dbs$name[dbs$file == self$path]
      }

      target_db <- tools::file_path_sans_ext(basename(target_path))
      DBI::dbExecute(
        self$connection,
        glue::glue("attach '{target_path}';")
      )
      on.exit(DBI::dbExecute(
        self$connection,
        glue::glue("detach {target_db};")
      ))
      DBI::dbExecute(
        self$connection,
        glue::glue("copy from database {source_db} to {target_db};")
      )
    },

    #' @description
    #' Set coordinates for DB; overwrites on repeated call
    #' @param type string; which type of coordinates to set, see [coords_t]
    #' @param ... named arguments are passed to the appropriate coordinate creator function
    set_coords = function(type = c("square"), ...) {
      if (self$row_count("coords_t") > 0L && interactive()) {
        choice <- utils::menu(
          choices = c("Yes, overwrite coordinates", "No, cancel operation"),
          title = "Coordinates table already exists. Overwriting will drop all dependent relations. Continue?"
        )
        if (choice != 1) {
          message("Operation cancelled.")
          return(invisible(NULL))
        }
      }
      create_fun <- switch(
        type,
        square = create_coords_t_square,
        function(x) stop("Unsupported coordinate type specified.")
      )

      self$commit(as_coords_t(create_fun(...)), "coords_t", mode = "overwrite")
    }
  ),

  ## Active Bindings ----
  active = list(
    #' @field coords_t A `coords_t` instance; see [create_coords_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    coords_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("coords_t")
        if (rlang::has_name(x, "region")) {
          data.table::set(x, j = "region", value = as.factor(x[["region"]]))
        }
        return(as_coords_t(x))
      }
      stopifnot(inherits(x, "coords_t"))
      self$commit(x, "coords_t", mode = "upsert")
    },

    #' @field periods_t A `periods_t` instance; see [create_periods_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    periods_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("periods_t")
        return(as_periods_t(x))
      }
      stopifnot(inherits(x, "periods_t"))
      self$commit(x, "periods_t", mode = "upsert")
    },

    #' @field lulc_meta_t A `lulc_meta_t` instance; see [create_lulc_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    lulc_meta_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("lulc_meta_t")
        return(as_lulc_meta_t(x))
      }
      stopifnot(inherits(x, "lulc_meta_t"))
      self$commit(x, "lulc_meta_t", mode = "upsert")
    },

    #' @field lulc_meta_long_v Return a `lulc_meta_long_v` instance, i.e. unrolled `lulc_meta_t`.
    lulc_meta_long_v = function() {
      self$fetch("lulc_meta_long_v") |>
        new_evoland_table("lulc_meta_long_v", keycols = NULL)
    },

    #' @field lulc_data_t A `lulc_data_t` instance; see [as_lulc_data_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    lulc_data_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("lulc_data_t")
        return(as_lulc_data_t(x))
      }
      stopifnot(inherits(x, "lulc_data_t"))
      self$commit(x, "lulc_data_t", mode = "upsert")
    },

    #' @field pred_meta_t A `pred_meta_t` instance; see [create_pred_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    pred_meta_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("pred_meta_t")
        return(as_pred_meta_t(x))
      }
      stopifnot(inherits(x, "pred_meta_t"))
      self$commit(x, "pred_meta_t", mode = "upsert")
    },

    #' @field pred_sources_v Retrieve a table of distinct predictor urls and their
    #' md5sum
    pred_sources_v = function() {
      self$fetch("pred_sources_v") |>
        new_evoland_table("pred_sources_v", keycols = NULL)
    },

    #' @field pred_data_t_float A `pred_data_t_float` instance; see
    #' [create_pred_data_t()] for the type of object to assign. Assigning is an
    #' upsert operation.
    pred_data_t_float = function(x) {
      if (missing(x)) {
        x <- self$fetch("pred_data_t_float")

        return(as_pred_data_t(x, "float"))
      }
      stopifnot(inherits(x, c("pred_data_t_float", "pred_data_t")))
      self$commit(x, "pred_data_t_float", mode = "upsert")
    },

    #' @field pred_data_t_int A `pred_data_t_int` instance; see
    #' [create_pred_data_t()] for the type of object to assign. Assigning is an
    #' upsert operation.
    pred_data_t_int = function(x) {
      if (missing(x)) {
        x <- self$fetch("pred_data_t_int")
        return(as_pred_data_t(x, "int"))
      }
      stopifnot(inherits(x, c("pred_data_t_int", "pred_data_t")))
      self$commit(x, "pred_data_t_int", mode = "upsert")
    },

    #' @field pred_data_t_bool A `pred_data_t_bool` instance; see
    #' [create_pred_data_t()] for the type of object to assign. Assigning is an
    #' upsert operation.
    pred_data_t_bool = function(x) {
      if (missing(x)) {
        x <- self$fetch("pred_data_t_bool")
        return(as_pred_data_t(x, "bool"))
      }
      stopifnot(inherits(x, c("pred_data_t_bool", "pred_data_t")))
      self$commit(x, "pred_data_t_bool", mode = "upsert")
    },

    #' @field trans_meta_t A `trans_meta_t` instance; see [create_trans_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    trans_meta_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("trans_meta_t")
        return(as_trans_meta_t(x))
      }
      stopifnot(inherits(x, "trans_meta_t"))

      # Custom upsert for trans_meta_t to maintain primary key integrity
      duckdb::duckdb_register(
        conn = self$connection,
        "temporary_trans_data",
        df = x
      )

      on.exit(duckdb::duckdb_unregister(
        self$connection,
        "temporary_trans_data"
      ))

      # Use INSERT ... ON CONFLICT DO UPDATE to maintain id_trans relationship
      sql <- "
        INSERT INTO trans_meta_t (id_trans, id_lulc_anterior, id_lulc_posterior,
                                  cardinality, frequency_rel, frequency_abs, is_viable)
        SELECT id_trans, id_lulc_anterior, id_lulc_posterior,
               cardinality, frequency_rel, frequency_abs, is_viable
        FROM temporary_trans_data
        ON CONFLICT (id_lulc_anterior, id_lulc_posterior)
        DO UPDATE SET
          cardinality = EXCLUDED.cardinality,
          frequency_rel = EXCLUDED.frequency_rel,
          frequency_abs = EXCLUDED.frequency_abs,
          is_viable = EXCLUDED.is_viable
      "
      DBI::dbExecute(self$connection, sql)
    },

    #' @field trans_preds_t A `trans_preds_t` instance; see [create_trans_preds_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    trans_preds_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("trans_preds_t")
        return(as_trans_preds_t(x))
      }
      stopifnot(inherits(x, "trans_preds_t"))
      self$commit(x, "trans_preds_t", mode = "upsert")
    },

    #' @field intrv_meta_t A `intrv_meta_t` instance; see [create_intrv_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    intrv_meta_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("intrv_meta_t")

        x[["params"]] <- lapply(
          x[["params"]],
          kv_df_to_list
        )

        return(as_intrv_meta_t(x))
      }
      stopifnot(inherits(x, "intrv_meta_t"))

      # Convert params list to data.frame format for DuckDB MAP conversion
      x[["params"]] <- lapply(
        x[["params"]],
        list_to_kv_df
      )

      duckdb::duckdb_register(
        conn = self$connection,
        name = "tmp_table",
        df = x
      )

      on.exit(duckdb::duckdb_unregister(self$connection, "tmp_table"))

      # Use INSERT OR REPLACE with MAP conversion for params field
      sql <- "
        INSERT OR REPLACE INTO intrv_meta_t
        SELECT
          id_intrv, id_period_list, id_trans_list, pre_allocation,
          name, pretty_name, description, sources,
          map_from_entries(params) as params
        FROM tmp_table
      "
      DBI::dbExecute(self$connection, sql)
    },

    #' @field intrv_masks_t A `intrv_masks_t` instance; see [as_intrv_masks_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    intrv_masks_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("intrv_masks_t")
        return(as_intrv_masks_t(x))
      }
      stopifnot(inherits(x, "intrv_masks_t"))
      self$commit(x, "intrv_masks_t", mode = "upsert")
    },

    #' @field trans_models_t A `trans_models_t` instance; see [create_trans_models_t()] for the type
    #' of object to assign. Assigning is an upsert operation.
    trans_models_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("trans_models_t")

        x[["model_params"]] <- lapply(
          x[["model_params"]],
          kv_df_to_list
        )
        x[["goodness_of_fit"]] <- lapply(
          x[["goodness_of_fit"]],
          kv_df_to_list
        )

        return(as_trans_models_t(x))
      }
      stopifnot(inherits(x, "trans_models_t"))

      # Convert lists to data.frame format for DuckDB MAP conversion
      x[["model_params"]] <- lapply(
        x[["model_params"]],
        list_to_kv_df
      )
      x[["goodness_of_fit"]] <- lapply(
        x[["goodness_of_fit"]],
        list_to_kv_df
      )

      duckdb::duckdb_register(
        conn = self$connection,
        name = "tmp_table",
        df = x
      )

      on.exit(duckdb::duckdb_unregister(self$connection, "tmp_table"))

      # Use INSERT OR REPLACE with MAP conversion for params field
      sql <- "
        INSERT OR REPLACE INTO trans_models_t
        SELECT
          id_trans, id_period, model_family,
          map_from_entries(model_params) as model_params,
          map_from_entries(goodness_of_fit) as goodness_of_fit,
          model_obj_part, model_obj_full
        FROM tmp_table
      "
      DBI::dbExecute(self$connection, sql)
    },

    #' @field alloc_params_t A `alloc_params_t` instance; see [as_alloc_params_t()] for the type
    #' of object to assign. Assigning is an upsert operation.
    alloc_params_t = function(x) {
      if (missing(x)) {
        x <-
          self$fetch("alloc_params_t") |>
          data.table::as.data.table()

        x[["alloc_params"]] <- lapply(
          x[["alloc_params"]],
          kv_df_to_list
        )
        x[["goodness_of_fit"]] <- lapply(
          x[["goodness_of_fit"]],
          kv_df_to_list
        )

        return(as_alloc_params_t(x))
      }
      stopifnot(inherits(x, "alloc_params_t"))

      # Convert lists to data.frame format for DuckDB MAP conversion
      x[["alloc_params"]] <- lapply(
        x[["alloc_params"]],
        list_to_kv_df
      )
      x[["goodness_of_fit"]] <- lapply(
        x[["goodness_of_fit"]],
        list_to_kv_df
      )

      duckdb::duckdb_register(
        conn = self$connection,
        name = "tmp_table",
        df = x
      )

      on.exit(duckdb::duckdb_unregister(self$connection, "tmp_table"))

      # Use INSERT OR REPLACE with MAP conversion for params field
      sql <- "
        INSERT OR REPLACE INTO alloc_params_t
        SELECT
          id_trans, id_period,
          map_from_entries(alloc_params) as model_params,
          map_from_entries(goodness_of_fit) as goodness_of_fit
        FROM tmp_table
      "
      DBI::dbExecute(self$connection, sql)
    }
  ),

  ## Private Methods ----
  private = list(
    # r6 hook called on gc(); Close the database connection
    #
    # return NULL (called for side effects)
    finalize = function() {
      if (!is.null(self$connection)) {
        DBI::dbDisconnect(self$connection)
        self$connection <- NULL
      }
    },

    # create the database schema
    create_schema = function() {
      # Read schema from inst/schema.sql
      schema_sql <-
        system.file("schema.sql", package = "evoland") |>
        readLines(warn = FALSE) |>
        paste(collapse = "\n")

      DBI::dbExecute(self$connection, schema_sql)
    },

    # Commit data in overwrite mode
    #
    # param x Data frame to commit
    # param table_name Character string table name
    commit_overwrite = function(x, table_name) {
      # Delete existing data without dropping the entity relations.
      DBI::dbExecute(self$connection, glue::glue("DELETE FROM {table_name}"))

      # Insert new data
      DBI::dbAppendTable(self$connection, table_name, x)
    },

    # Commit data in append mode
    #
    # param x Data frame to commit
    # param table_name Character string table name
    commit_append = function(x, table_name) {
      DBI::dbAppendTable(self$connection, table_name, x)
    },

    # Commit data in upsert mode
    #
    # param x Data frame to commit
    # param table_name Character string table name
    commit_upsert = function(x, table_name) {
      duckdb::duckdb_register(
        conn = self$connection,
        "temporary_data_table",
        df = x
      )

      on.exit(duckdb::duckdb_unregister(
        self$connection,
        "temporary_data_table"
      ))

      # Build INSERT OR REPLACE query
      sql <- glue::glue(
        "INSERT OR REPLACE INTO {table_name} SELECT * FROM temporary_data_table"
      )
      DBI::dbExecute(self$connection, sql)
    }
  )
)

# utility function to transform a list to DuckDB-compatible k/v dataframe
list_to_kv_df <- function(param_list) {
  if (is.null(param_list)) {
    return(NULL)
  }
  data.frame(
    k = names(param_list),
    v = unlist(param_list)
  )
}

# utility function to transform a DuckDB key/value dataframe to list
kv_df_to_list <- function(x) {
  if (is.null(x)) {
    return(NULL)
  } else if (is.list(x[["key"]]) || is.list(x[["value"]])) {
    stop("cannot work on complex map keys or values")
  } else if (is.numeric(x[["value"]])) {
    out <- as.list(x[["value"]])
    names(out) <- x[["key"]]
    return(out)
  }
  # assuming a VARCHAR : VARCHAR map, we want to retrieve numerics
  nums <- suppressWarnings(as.numeric(x[["value"]]))
  out <- list()
  for (row in seq_len(nrow(x))) {
    key <- x[["key"]][[row]]
    if (is.na(nums[[row]])) {
      # if as.numeric conversion resulted in NA, keep unaltered string
      out[[key]] <- x[["value"]][[row]]
      next
    }
    # if as.numeric conversion resulted in numeric, keep numeric
    out[[key]] <- nums[[row]]
  }
  out
}
