#' R6 Class for DuckDB Database Interface
#'
#' An R6 class that provides an interface to a DuckDB database for the evoland
#' package. This class handles database initialization, data commits, and data
#' fetching operations.
#'
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
    #' @return A new `evoland_db` object.
    #'
    #' @examples
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
    }
  ),

  ## Active Bindings ----
  active = list(
    #' Get the model configuration
    #' @field config An `evoland_config`
    config = function() {
      config_data <- self$fetch("config_t")
      if (nrow(config_data) == 0L) {
        stop("No config ingested yet", call. = FALSE)
      }

      config_data <- qs::qdeserialize(
        config_data[["r_obj"]][[1]]
      )

      validate(
        structure(
          config_data,
          class = "evoland_config"
        )
      )
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
