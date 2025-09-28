#' R6 Class for DuckDB Database Interface
#'
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

    #' Initialize a new evoland_db object
    #'
    #' @param path Character string. Path to the DuckDB database file. May also be ":memory:".
    #' @param write Logical. Whether to open the database in write mode. Default is TRUE.
    #'   If FALSE, the database file must already exist.
    #'
    #' @return A new `evoland_db` object
    initialize = function(path, write = TRUE) {
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
      } else {
        DBI::dbExecute(self$connection, "load spatial;")
      }

      invisible(self)
    },

    #' Commit data to the database
    #'
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

    #' Fetch data from the database
    #'
    #' @param table_name Character string. Name of the database table or view to query.
    #' @param where Character string. Optional WHERE clause for the SQL query.
    #' @param limit integerish, limit the amount of rows to return
    #'
    #' @return A table of the original class
    fetch = function(table_name, where = NULL, limit = NULL) {
      # Build SQL query
      sql <- glue::glue("SELECT * FROM {table_name}")

      if (!is.null(where)) {
        sql <- glue::glue("{sql} WHERE {where}")
      }
      if (!is.null(limit)) {
        sql <- glue::glue("{sql} LIMIT {limit}")
      }

      DBI::dbGetQuery(self$connection, sql) |>
        data.table::as.data.table()
    },

    #' List all tables in the database
    #'
    #' @return Character vector of table names
    list_tables = function() {
      DBI::dbListTables(self$connection)
    },

    #' Execute statement
    #' @param statement A SQL statement
    #' @return No. of rows affected by statement
    execute = function(statement) {
      DBI::dbExecute(self$connection, statement)
    },

    #' Get table row count
    #' @param table_name Character string. Name of the database table or view to query.
    #' @return No. of rows affected by statement
    row_count = function(table_name) {
      qry <- glue::glue("select count() from {table_name};")
      DBI::dbGetQuery(self$connection, qry)[[1]]
    },

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
    }
  ),

  ## Active Bindings ----
  active = list(
    #' @field config Retrieve [evoland_config]; may be assigned a config
    #' ([read_evoland_config()]) only if no other config is present
    config = function(config_data) {
      if (missing(config_data)) {
        config_data <- DBI::dbGetQuery(
          self$connection,
          "select r_obj from config_t limit 1"
        )
        if (nrow(config_data) == 0L) {
          stop("No config ingested yet", call. = FALSE)
        }

        config_data <- qs::qdeserialize(
          config_data[["r_obj"]][[1]]
        )

        out <- validate(structure(config_data, class = "evoland_config"))
        return(out)
      }
      if (!inherits(config_data, "evoland_config")) {
        stop("Can only insert evoland_config objects")
      }
      if (self$row_count("config_t") > 0L) {
        stop("DB already has a config! Use db$delete_from('config_t') to delete it")
      }
      config_json <- "{}" # empty until we can reliably (de)serialize JSON

      df <- data.table::data.table(
        config = config_json,
        r_obj = list(qs::qserialize(config_data))
      )
      self$commit(df, "config_t", mode = "overwrite")
    },

    #' @field coords_t A `coords_t` instance; see [create_coords_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    coords_t = function(coords_t) {
      if (missing(coords_t)) {
        coords_t <-
          DBI::dbGetQuery(self$connection, "from coords_t") |>
          data.table::as.data.table(key = "id_coord")

        data.table::set(coords_t, j = "region", value = as.factor(coords_t[["region"]]))

        return(new_evoland_table(coords_t, "coords_t"))
      }
      # TODO for this and the following: ensure proper inheritance before attempting upsert
      self$commit(coords_t, "coords_t", mode = "upsert")
    },

    #' @field periods_t A `periods_t` instance; see [create_periods_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    periods_t = function(periods_t) {
      if (missing(periods_t)) {
        periods_t <-
          DBI::dbGetQuery(self$connection, "from periods_t") |>
          data.table::as.data.table(key = "id_period")

        return(new_evoland_table(periods_t, "periods_t"))
      }
      self$commit(periods_t, "periods_t", mode = "upsert")
    },

    #' @field lulc_meta_t A `lulc_meta_t` instance; see [create_lulc_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    lulc_meta_t = function(lulc_meta_t) {
      if (missing(lulc_meta_t)) {
        lulc_meta_t <-
          DBI::dbGetQuery(self$connection, "from lulc_meta_t") |>
          data.table::as.data.table(key = "id_lulc")

        return(new_evoland_table(lulc_meta_t, "lulc_meta_t"))
      }
      self$commit(lulc_meta_t, "lulc_meta_t", mode = "upsert")
    },

    #' @field lulc_data_t A `lulc_data_t` instance; see [create_lulc_data_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    lulc_data_t = function(lulc_data_t) {
      if (missing(lulc_data_t)) {
        lulc_data_t <-
          DBI::dbGetQuery(self$connection, "from lulc_data_t") |>
          data.table::as.data.table()

        data.table::setkeyv(lulc_data_t, c("id_coord", "id_lulc", "id_period"))

        return(new_evoland_table(lulc_data_t, "lulc_data_t"))
      }
      self$commit(lulc_data_t, "lulc_data_t", mode = "upsert")
    },

    #' @field pred_meta_t A `pred_meta_t` instance; see [create_pred_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    pred_meta_t = function(pred_meta_t) {
      if (missing(pred_meta_t)) {
        pred_meta_t <-
          DBI::dbGetQuery(self$connection, "from pred_meta_t") |>
          data.table::as.data.table(key = "id_pred")

        return(new_evoland_table(pred_meta_t, "pred_meta_t"))
      }
      stopifnot(inherits(pred_meta_t, "pred_meta_t"))
      self$commit(pred_meta_t, "pred_meta_t", mode = "upsert")
    },

    #' @field pred_sources_v Retrieve a table of distinct predictor urls and their
    #' md5sum
    pred_sources_v = function() {
      pred_sources_v <-
        DBI::dbGetQuery(self$connection, "from pred_sources_v") |>
        data.table::as.data.table() |>
        new_evoland_table("pred_sources_v")
    },

    #' @field pred_data_t_float A `pred_data_t_float` instance; see
    #' [create_pred_data_t()] for the type of object to assign. Assigning is an
    #' upsert operation.
    pred_data_t_float = function(pred_data_t_float) {
      if (missing(pred_data_t_float)) {
        pred_data_t_float <-
          DBI::dbGetQuery(self$connection, "from pred_data_t_float") |>
          data.table::as.data.table()

        data.table::setkeyv(pred_data_t_float, c("id_pred", "id_coord", "id_period"))

        return(new_evoland_table(pred_data_t_float, c("pred_data_t_float", "pred_data_t")))
      }
      stopifnot(inherits(pred_data_t_float, c("pred_data_t_float", "pred_data_t")))
      self$commit(pred_data_t_float, "pred_data_t_float", mode = "upsert")
    },

    #' @field pred_data_t_int A `pred_data_t_int` instance; see
    #' [create_pred_data_t()] for the type of object to assign. Assigning is an
    #' upsert operation.
    pred_data_t_int = function(pred_data_t_int) {
      if (missing(pred_data_t_int)) {
        pred_data_t_int <-
          DBI::dbGetQuery(self$connection, "from pred_data_t_int") |>
          data.table::as.data.table()

        data.table::setkeyv(pred_data_t_int, c("id_pred", "id_coord", "id_period"))

        return(new_evoland_table(pred_data_t_int, c("pred_data_t_int", "pred_data_t")))
      }
      stopifnot(inherits(pred_data_t_int, c("pred_data_t_int", "pred_data_t")))
      self$commit(pred_data_t_int, "pred_data_t_int", mode = "upsert")
    },

    #' @field pred_data_t_bool A `pred_data_t_bool` instance; see
    #' [create_pred_data_t()] for the type of object to assign. Assigning is an
    #' upsert operation.
    pred_data_t_bool = function(pred_data_t_bool) {
      if (missing(pred_data_t_bool)) {
        pred_data_t_bool <-
          DBI::dbGetQuery(self$connection, "from pred_data_t_bool") |>
          data.table::as.data.table()

        data.table::setkeyv(pred_data_t_bool, c("id_pred", "id_coord", "id_period"))

        return(new_evoland_table(pred_data_t_bool, c("pred_data_t_bool", "pred_data_t")))
      }
      stopifnot(inherits(pred_data_t_bool, c("pred_data_t_bool", "pred_data_t")))
      self$commit(pred_data_t_bool, "pred_data_t_bool", mode = "upsert")
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
