#' R6 Base Class for Parquet-Backed DuckDB Storage
#'
#' @description
#' A domain-agnostic R6 class that provides an interface to a folder-based data
#' storage system using DuckDB for in-memory SQL operations and parquet files
#' for efficient on-disk persistence. This class can be inherited by
#' domain-specific database classes.
#'
#' @export

parquet_duckdb <- R6::R6Class(
  classname = "parquet_duckdb",

  ## Public Methods ----
  public = list(
    #' @field connection DBI connection object to an in-memory DuckDB database
    connection = NULL,

    #' @field path Character string path to the data folder
    path = NULL,

    #' @field default_format Default file format for new tables
    default_format = NULL,

    #' @field writeopts Default write options for DuckDB
    writeopts = NULL,

    #' @description
    #' Initialize a new parquet_duckdb object
    #' @param path Character string. Path to the data folder.
    #' @param default_format Character. Default file format ("parquet" or "csv").
    #' Default is "parquet".
    #' @param extensions Character vector of DuckDB extensions to load (e.g., "spatial")
    #'
    #' @return A new `parquet_duckdb` object
    initialize = function(
      path,
      default_format = c("parquet", "csv"),
      extensions = character(0)
    ) {
      # Create folder if it doesn't exist
      self$path <- ensure_dir(path)

      # Set format / writeopts
      self$default_format <- match.arg(default_format)
      self$writeopts <- switch(
        self$default_format,
        parquet = "format parquet, compression zstd",
        csv = "format csv",
        stop(glue::glue("Unsupported format: {self$default_format}"))
      )

      # Create in-memory connection for SQL operations
      self$connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

      # load extensions
      for (ext in extensions) {
        self$execute(glue::glue("install {ext}; load {ext};"))
      }

      invisible(self)
    },

    ### Core Database Methods ----

    #' @description
    #' Execute a SQL statement
    #' @param statement A SQL statement
    #' @return Number of rows affected by statement
    execute = function(statement) {
      DBI::dbExecute(self$connection, statement)
    },

    #' @description
    #' Execute a SQL query and return results
    #' @param statement A SQL query statement
    #' @return A data.table with query results
    get_query = function(statement) {
      DBI::dbGetQuery(self$connection, statement) |>
        data.table::as.data.table()
    },

    #' @description
    #' Attach a table from parquet/CSV file as a temporary table in DuckDB
    #' @param table_name Character. Name of table to attach.
    #' @param columns Character vector. Optional SQL column selection, defaults to "*"
    #' @param where Character. Optional SQL WHERE clause to subset the table.
    #' @return Invisible NULL (called for side effects)
    attach_table = function(table_name, columns = "*", where = NULL) {
      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        stop(glue::glue("Table '{table_name}' does not exist at path: {self$path}"))
      }

      # Build SQL query
      sql <- glue::glue(
        "create temp table {table_name} as ",
        "select {paste(columns, collapse = ', ')} ",
        "from read_{file_info$format}('{file_info$path}')"
      )

      if (!is.null(where)) {
        sql <- glue::glue("{sql} where {where}")
      }

      # Execute SQL
      self$execute(sql)
      invisible(NULL)
    },

    #' @description
    #' Detach a table from the in-memory database
    #' @param table_name Character. Name of table to drop.
    #' @return Invisible NULL (called for side effects)
    detach_table = function(table_name) {
      self$execute(paste0("drop table if exists ", table_name, ";"))
      invisible(NULL)
    },

    #' @description
    #' Get row count for a table
    #' @param table_name Character string. Name of the table to query.
    #' @return Integer number of rows
    row_count = function(table_name) {
      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        return(0L)
      }

      self$get_query(
        glue::glue("select count(*) as n from read_{file_info$format}('{file_info$path}')")
      )[[1]]
    },

    #' @description
    #' List all tables (files) in storage
    #' @return Character vector of table names
    list_tables = function() {
      list.files(self$path, pattern = "\\.(parquet|csv)$", full.names = FALSE) |>
        tools::file_path_sans_ext() |>
        unique() |>
        sort()
    },

    #' @description
    #' Execute a function with specified tables attached, handling attach/detach automatically.
    #' If a table is already attached in the DuckDB instance, it won't be re-attached or detached.
    #'
    #' @param tables Character vector of table names to attach
    #' @param func Function to execute with tables attached
    #' @param ... Additional arguments passed to func
    #' @return Result of func
    with_tables = function(tables, func, ...) {
      # Track which tables we attach (so we know which to detach)
      attached_tables <- character(0)

      # Check which tables are already attached
      existing_tables <- DBI::dbListTables(self$connection)

      # Attach tables that aren't already present
      for (table in tables) {
        if (!table %in% existing_tables) {
          self$attach_table(table)
          attached_tables <- c(attached_tables, table)
        }
      }

      # Ensure cleanup on exit
      on.exit(
        {
          for (table in attached_tables) {
            self$detach_table(table)
          }
        },
        add = TRUE
      )

      # Execute the function
      func(...)
    },

    #' @description
    #' Fetch data from a table
    #' @param table_name Character string. Name of the table to query.
    #' @param where Character string. Optional WHERE clause for the SQL query.
    #' @param limit Integer. Optional limit on number of rows to return.
    #'
    #' @return A data.table
    fetch = function(table_name, where = NULL, limit = NULL) {
      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        return(data.table::data.table())
      }

      # build sql query
      sql <- glue::glue("select * from read_{file_info$format}('{file_info$path}')")

      if (!is.null(where)) {
        sql <- glue::glue("{sql} where {where}")
      }
      if (!is.null(limit)) {
        sql <- glue::glue("{sql} limit {limit}")
      }

      self$get_query(sql)
    },

    #' @description
    #' Delete rows from a table
    #' @param table_name Character string. Name of the table to delete from.
    #' @param where Character string. Optional WHERE clause; if NULL, deletes all rows.
    #' @return Number of rows deleted
    delete_from = function(table_name, where = NULL) {
      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        return(0L)
      }

      count_before <- self$row_count(table_name)

      if (is.null(where)) {
        file.remove(file_info$path)
        return(count_before)
      }

      self$execute(glue::glue(
        r"{
        copy (
          select * from read_{file_info$format}('{file_info$path}')
          where not ({where})
        )
        to '{file_info$path}' ({self$writeopts})
        }"
      ))

      count_after <- self$row_count(table_name)

      return(count_before - count_after)
    },

    ### Commit Methods ----

    #' @description
    #' Commit data in overwrite mode
    #' @param x Data frame to commit
    #' @param table_name Character string table name
    #' @param autoincrement_cols Character vector of column names to auto-increment
    #' @param map_cols Character vector of columns to convert to MAP format
    #' @return Invisible NULL (called for side effects)
    commit_overwrite = function(
      x,
      table_name,
      autoincrement_cols = character(0),
      map_cols = character(0)
    ) {
      file_path <- file.path(self$path, paste0(table_name, ".", self$default_format))

      private$register_new_data_v(x, map_cols)
      on.exit(private$cleanup_new_data_v(map_cols), add = TRUE)

      # Warn if overriding existing IDs
      if (length(intersect(autoincrement_cols, names(x))) > 0) {
        warning(glue::glue(
          "Overriding existing IDs ({toString(autoincrement_cols)}) with row numbers;\n",
          "Assign these IDs manually and do not pass any autoincrement_cols to avoid this warning"
        ))
      }

      # Build SELECT expression
      ordinary_cols <- setdiff(names(x), autoincrement_cols)
      select_expr <- glue::glue_collapse(
        c(glue::glue("row_number() over () as {autoincrement_cols}"), ordinary_cols),
        sep = ",\n "
      )

      self$execute(glue::glue(
        r"{
        copy (
          select {select_expr}
          from new_data_v
        ) to '{file_path}' ({self$writeopts})
        }"
      ))

      invisible(NULL)
    },

    #' @description
    #' Commit data in append mode
    #' @param x Data frame to commit
    #' @param table_name Character string table name
    #' @param autoincrement_cols Character vector of column names to auto-increment
    #' @param map_cols Character vector of columns to convert to MAP format
    #' @return Invisible NULL (called for side effects)
    commit_append = function(
      x,
      table_name,
      autoincrement_cols = character(0),
      map_cols = character(0)
    ) {
      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        return(self$commit_overwrite(x, table_name, autoincrement_cols, map_cols))
      }

      self$attach_table(table_name)
      on.exit(self$detach_table(table_name), add = TRUE)
      private$set_autoincrement_vars(table_name, autoincrement_cols)

      private$register_new_data_v(x, map_cols)
      on.exit(private$cleanup_new_data_v(map_cols), add = TRUE)

      ordinary_cols <- setdiff(names(x), autoincrement_cols)
      select_new <- glue::glue_collapse(
        c(
          glue::glue(
            "row_number() over () + getvariable('max_{autoincrement_cols}') as {autoincrement_cols}"
          ),
          ordinary_cols
        ),
        sep = ",\n "
      )

      # Concatenation using UNION ALL; "by name" handles missing columns
      self$execute(glue::glue(
        r"{
          copy (
            select * from {table_name}
            union all by name
            select {select_new}
            from new_data_v
          )
          to '{file_info$path}' ({self$writeopts})
          }"
      ))

      invisible(NULL)
    },

    #' @description
    #' Commit data in upsert mode (update existing, insert new)
    #' @param x Data frame to commit
    #' @param table_name Character string table name
    #' @param key_cols Character vector of columns that define uniqueness
    #' @param autoincrement_cols Character vector of column names to auto-increment
    #' @param map_cols Character vector of columns to convert to MAP format
    #' @return Invisible NULL (called for side effects)
    commit_upsert = function(
      x,
      table_name,
      key_cols = grep("^id_", names(x), value = TRUE),
      autoincrement_cols = character(0),
      map_cols = character(0)
    ) {
      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        return(self$commit_overwrite(x, table_name, autoincrement_cols, map_cols))
      } else if (length(key_cols) == 0) {
        return(self$commit_append(x, table_name, autoincrement_cols, map_cols))
      }

      self$attach_table(table_name)
      on.exit(self$detach_table(table_name), add = TRUE)

      private$set_autoincrement_vars(table_name, autoincrement_cols)

      private$register_new_data_v(x, map_cols)
      on.exit(private$cleanup_new_data_v(map_cols), add = TRUE)

      # Update existing data
      ordinary_cols <- setdiff(names(x), union(key_cols, autoincrement_cols))
      update_select_expr <- glue::glue_collapse(
        glue::glue("{ordinary_cols} = new_data_v.{ordinary_cols}"),
        sep = ",\n "
      )
      update_join_condition <- glue::glue_collapse(
        glue::glue("{table_name}.{key_cols} = new_data_v.{key_cols}"),
        sep = "\nand "
      )

      self$execute(glue::glue(
        r"{
        update {table_name} set
          {update_select_expr}
        from new_data_v
        where
          {update_join_condition};
        }"
      ))

      # Insert new data
      insert_select_expr <- glue::glue_collapse(
        c(
          glue::glue(
            "row_number() over () + getvariable('max_{autoincrement_cols}') as {autoincrement_cols}"
          ),
          glue::glue("new_data_v.{setdiff(names(x), autoincrement_cols)}")
        ),
        sep = ",\n "
      )
      null_condition <- glue::glue_collapse(
        glue::glue("{table_name}.{key_cols} is null"),
        sep = "\nand "
      )

      self$execute(glue::glue(
        r"{
        insert into {table_name}
        select
          {insert_select_expr}
        from
          new_data_v
        left join
          {table_name}
        on
          {update_join_condition}
        where
          {null_condition}
        ;
        }"
      ))

      self$execute(glue::glue("copy {table_name} to '{file_info$path}' ({self$writeopts})"))

      invisible(NULL)
    },

    #' @description
    #' Print method for parquet_duckdb
    #' @param ... Not used
    #' @return self (invisibly)
    print = function(...) {
      cat("<parquet_duckdb>\n")
      cat(sprintf("Database path: %s\n", self$path))
      cat(sprintf("Default format: %s\n", self$default_format))
      cat(sprintf("Tables: %d\n", length(self$list_tables())))
      invisible(self)
    }
  ),

  ## Private Methods ----
  private = list(
    # R6 hook called on gc(); Close the database connection
    finalize = function() {
      if (!is.null(self$connection)) {
        DBI::dbDisconnect(self$connection)
        self$connection <- NULL
      }
    },

    # Get file path and format for a table
    #
    # @param table_name Character string table name
    # @return List with path, format, and exists flag
    get_file_path = function(table_name) {
      # Check for parquet first, then csv
      parquet_path <- file.path(self$path, paste0(table_name, ".parquet"))
      csv_path <- file.path(self$path, paste0(table_name, ".csv"))

      if (file.exists(parquet_path)) {
        return(list(path = parquet_path, format = "parquet", exists = TRUE))
      } else if (file.exists(csv_path)) {
        return(list(path = csv_path, format = "csv", exists = TRUE))
      } else {
        # Return default format for new file
        default_path <- file.path(
          self$path,
          paste0(table_name, ".", self$default_format)
        )
        return(list(
          path = default_path,
          format = self$default_format,
          exists = FALSE
        ))
      }
    },

    # Register new_data_v table, optionally converting MAP columns
    #
    # @param x Data to register
    # @param map_cols Character vector of columns to convert to MAP format
    # @return NULL (called for side effects)
    register_new_data_v = function(x, map_cols = character(0)) {
      if (length(map_cols) == 0) {
        # No MAP conversion needed - register directly
        duckdb::duckdb_register(self$connection, "new_data_v", x)
      } else {
        # Convert list columns to key-value dataframes
        x <-
          data.table::copy(x) |>
          convert_list_cols(map_cols, list_to_kv_df)

        # Register as intermediate table
        duckdb::duckdb_register(self$connection, "new_data_raw", x)

        # Build SELECT expression with map_from_entries for MAP columns
        map_exprs <- glue::glue("map_from_entries({map_cols}) as {map_cols}")
        other_cols <- setdiff(names(x), map_cols)
        all_exprs <- c(other_cols, map_exprs)
        select_expr <- glue::glue_collapse(all_exprs, sep = ", ")

        # Create new_data_v from new_data_raw
        self$execute(glue::glue(
          "create temp table new_data_v as select {select_expr} from new_data_raw"
        ))
      }

      invisible(NULL)
    },

    # Cleanup new_data_v and related tables
    #
    # @param map_cols Character vector indicating if MAP conversion was used
    # @return NULL (called for side effects)
    cleanup_new_data_v = function(map_cols = character(0)) {
      if (length(map_cols) == 0) {
        duckdb::duckdb_unregister(self$connection, "new_data_v")
      } else {
        self$execute(
          "drop table if exists new_data_v;
          drop view if exists new_data_v"
        )
        duckdb::duckdb_unregister(self$connection, "new_data_raw")
      }

      invisible(NULL)
    },

    # Set DuckDB variables max_{colname} for autoincrement columns
    #
    # @param table_name Character string table name
    # @param autoincrement_cols Character vector of column names
    # @return NULL (called for side effects)
    set_autoincrement_vars = function(table_name, autoincrement_cols) {
      if (length(autoincrement_cols) == 0L) {
        return(NULL)
      }

      if (!DBI::dbExistsTable(self$connection, table_name)) {
        # Attach/detach if not already attached
        self$attach_table(table_name)
        on.exit(self$detach_table(table_name))
      }

      existing_cols <-
        glue::glue("select column_name from (describe {table_name})") |>
        self$get_query() |>
        (\(x) x[[1]])() |>
        intersect(autoincrement_cols)

      missing_cols <- setdiff(autoincrement_cols, existing_cols)

      set_exprs <- glue::glue_collapse(
        c(
          glue::glue(
            "set variable max_{existing_cols} =
              (select coalesce(max({existing_cols}), 0) from {table_name});"
          ),
          glue::glue(
            "set variable max_{missing_cols} = 0;"
          )
        ),
        sep = "\n"
      )

      self$execute(set_exprs)
      invisible(NULL)
    }
  )
)
