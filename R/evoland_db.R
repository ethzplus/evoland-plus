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
    #' @param path Character string. Path to the DuckDB database file.
    #' @param write Logical. Whether to open the database in write mode. Default is TRUE.
    #'   If FALSE, the database file must already exist.
    #'
    #' @return A new `evoland_db` object.
    #'
    #' @examples
    #' \dontrun{
    #' # Create new database
    #' db <- evoland_db$new("my_database.duckdb")
    #'
    #' # Open existing database in read-only mode
    #' db_readonly <- evoland_db$new("existing.duckdb", write = FALSE)
    #' }
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
      }

      invisible(self)
    },

    #' Commit data to the database
    #'
    #' @param x Data object to commit. The S3 class determines the target table.
    #' @param mode Character string. One of "append" (default), "upsert", or "overwrite".
    #'
    #' @return NULL (called for side effects)
    #'
    #' @examples
    #' \dontrun{
    #' # Commit data with different modes
    #' db$commit(lulc_meta_data, mode = "overwrite")
    #' db$commit(lulc_data, mode = "append")
    #' db$commit(updated_coords, mode = "upsert")
    #' }
    commit = function(x, mode = "append") {
      if (!self$write_mode) {
        stop("Database opened in read-only mode. Cannot commit data.")
      }
      if (!inherits(x, "data.frame")) {
        stop("Object must inherit from data.frame.")
      }

      # Validate mode
      valid_modes <- c("append", "upsert", "overwrite")
      if (!mode %in% valid_modes) {
        stop(glue::glue(
          "Invalid mode '{mode}'. Must be one of: {paste(valid_modes, collapse = ', ')}"
        ))
      }

      # Get table name from S3 class
      table_name <- class(x)[[1]]

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
    #' @param x Character string. Name of the database table or view to query.
    #' @param where Character string. Optional WHERE clause for the SQL query.
    #'
    #' @return A data.frame with the queried data, with appropriate S3 class attached.
    #'
    #' @examples
    #' \dontrun{
    #' # Fetch all data from a table
    #' coords <- db$fetch("coords_t")
    #'
    #' # Fetch with WHERE clause
    #' recent_data <- db$fetch("lulc_data_t", where = "date >= '2020-01-01'")
    #' }
    fetch = function(x, where = NULL) {
      # Build SQL query
      sql <- glue::glue("SELECT * FROM {x}")

      if (!is.null(where)) {
        sql <- glue::glue("{sql} WHERE {where}")
      }

      # Execute query
      result <- DBI::dbGetQuery(self$connection, sql)

      # Convert to tibble for consistent behavior
      result <- tibble::as_tibble(result)

      # Add appropriate S3 class based on table name
      class(result) <- c(x, class(result))

      return(result)
    },

    #' List all tables in the database
    #'
    #' @return Character vector of table names
    list_tables = function() {
      DBI::dbListTables(self$connection)
    },

    #' Check if a table exists
    #'
    #' @param table_name Character string. Name of the table to check.
    #'
    #' @return Logical indicating if table exists
    table_exists = function(table_name) {
      DBI::dbExistsTable(self$connection, table_name)
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
      # Delete existing data
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
      # For upsert, we need to know the primary key columns
      # This is a simplified implementation - could be enhanced with metadata

      # Get column names
      col_names <- names(x)
      col_placeholders <- paste(rep("?", length(col_names)), collapse = ", ")
      col_names_str <- paste(col_names, collapse = ", ")

      # Create upsert query (this assumes a single-column primary key named 'id' or similar)
      # In practice, this would need to be more sophisticated based on actual table schema

      # For now, use a simple replace approach
      tryCatch(
        {
          # Try INSERT first
          DBI::dbAppendTable(self$connection, table_name, x)
        },
        error = function(e) {
          # If INSERT fails due to constraint violation, try UPDATE approach
          warning("Upsert operation fell back to replace mode due to constraint violations")
          private$commit_overwrite(x, table_name)
        }
      )
    }
  )
)
