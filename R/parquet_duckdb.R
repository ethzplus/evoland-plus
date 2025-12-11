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
    #' @param default_format Character. Default file format ("parquet" or "json").
    #' Default is "parquet".
    #' @param extensions Character vector of DuckDB extensions to load (e.g., "spatial")
    #'
    #' @return A new `parquet_duckdb` object
    initialize = function(
      path,
      default_format = c("parquet", "json"),
      extensions = c("json")
    ) {
      # Create folder if it doesn't exist
      self$path <- ensure_dir(path)

      # Set format / writeopts
      self$default_format <- match.arg(default_format)
      self$writeopts <- private$format_to_writeopts(self$default_format)

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
      # it's not currently possible to get the duckdb driver to build data.tables directly
      result <- data.table::as.data.table(
        DBI::dbGetQuery(self$connection, statement)
      )

      # Convert list columns containing data.frames to data.tables
      list_cols <- names(result)[vapply(result, is.list, logical(1))]
      for (col in list_cols) {
        data.table::set(
          result,
          j = col,
          value = lapply(result[[col]], function(x) {
            if (is.data.frame(x)) data.table::as.data.table(x) else x
          })
        )
      }

      result
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
      list.files(self$path, pattern = "\\.(parquet|json)$", full.names = FALSE) |>
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
    #' @param map_cols Vector of columns to be converted from key/value structs to R lists
    #' @param json_colspec For JSON files, an optional column type specification
    #'
    #' @return A data.table
    fetch = function(
      table_name,
      where = NULL,
      limit = NULL,
      map_cols = NULL,
      json_colspec = NULL
    ) {
      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        stop("Table `", table_name, "` does not exist")
      }

      # build sql query
      if (!is.null(json_colspec)) {
        if (file_info$format != "json") {
          stop("Cannot pass json_colspec if format is not json")
        }
        sql <- glue::glue(
          "from read_<file_info$format>('<file_info$path>', columns = {<json_colspec>})",
          .open = "<",
          .close = ">"
        )
      } else {
        sql <- glue::glue("from read_{file_info$format}('{file_info$path}')")
      }

      if (!is.null(where)) {
        sql <- glue::glue("{sql} where {where}")
      }
      if (!is.null(limit)) {
        sql <- glue::glue("{sql} limit {limit}")
      }

      res <- self$get_query(sql)

      if (!is.null(map_cols) && nrow(res) > 0) {
        return(convert_list_cols(res, map_cols, kv_df_to_list))
      }

      res
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

      # Use the same format as the existing file
      writeopts <- private$format_to_writeopts(file_info$format)

      self$execute(glue::glue(
        r"{
        copy (
          select * from read_{file_info$format}('{file_info$path}')
          where not ({where})
        )
        to '{file_info$path}' ({writeopts})
        }"
      ))

      count_after <- self$row_count(table_name)

      return(count_before - count_after)
    },

    #' @description
    #' Commit data using overwrite, append, or upsert modes. Handles autoincrement, key
    #' identity columns, and list-to-MAP conversion.
    #' @param x Data frame to commit. If character, in-duckdb-memory table.
    #' @param table_name Character string table name
    #' @param key_cols Character vector of columns that define uniqueness. If missing,
    #' use all columns starting with `id_`
    #' @param autoincrement_cols Character vector of column names to auto-increment
    #' @param map_cols Character vector of columns to convert to MAP format
    #' @param method Character, one of "overwrite", "append", "upsert" (upsert being an
    #' update for existing rows, and insert for new rows)
    #' @param format Character. Optional format override ("parquet" or "json"). If NULL,
    #' uses the existing file's format or default_format for new files.
    #' @return Invisible NULL (called for side effects)
    commit = function(
      x,
      table_name,
      key_cols,
      autoincrement_cols = character(0),
      map_cols = character(0),
      method = c("overwrite", "append", "upsert"),
      format = NULL
    ) {
      method <- match.arg(method)

      private$register_new_data_v(x, map_cols)
      on.exit(private$cleanup_new_data_v(), add = TRUE)
      all_cols <- self$get_query(
        "select column_name from (describe new_data_v)"
      )[[1]]

      file_info <- private$get_file_path(table_name, format)

      if (method == "overwrite" || !file_info$exists) {
        # in case overwrite explicitly required, or no previously existing data to
        # append or upsert to; rest of logic can be skipped
        return(private$commit_overwrite(
          table_name = table_name,
          all_cols = all_cols,
          autoincrement_cols = autoincrement_cols,
          file_info = file_info
        ))
      }

      self$attach_table(table_name)
      on.exit(self$detach_table(table_name), add = TRUE)
      private$set_autoincrement_vars(table_name, autoincrement_cols)

      if (missing(key_cols)) {
        key_cols <- grep("^id_", all_cols, value = TRUE)
      }

      if (method == "append" || length(key_cols) == 0L) {
        # if there are no key columns to join on, upsert becomes append
        private$commit_append(
          table_name = table_name,
          all_cols = all_cols,
          autoincrement_cols = autoincrement_cols,
          file_info = file_info
        )
      } else {
        private$commit_upsert(
          table_name = table_name,
          all_cols = all_cols,
          key_cols = key_cols,
          autoincrement_cols = autoincrement_cols,
          file_info = file_info
        )
      }
    },

    #' @description
    #' Print method for parquet_duckdb
    #' @param ... Not used
    #' @return self (invisibly)
    print = function(...) {
      # gather data to be printed
      classes <- class(self)
      classes <- classes[classes != "R6"]

      all_names <- names(self)
      methods <- character(0)
      active_bindings <- character(0)

      names(self$.__enclos_env__$private)
      if (!is.null(self$.__enclos_env__$super)) {
        # exclude private super names
        super_names <- setdiff(
          ls(self$.__enclos_env__$super),
          ls(self$.__enclos_env__$super$.__enclos_env__$private)
        )
      } else {
        super_names <- character(0)
      }
      nonsuper_names <- setdiff(all_names, super_names)

      for (name in nonsuper_names) {
        # Check if it's an active binding first; subset2 would evaluate it
        if (bindingIsActive(name, self$.__enclos_env__$self)) {
          active_bindings <- c(active_bindings, name)
        } else {
          obj <- .subset2(self, name)
          if (is.function(obj) && !name %in% c("initialize", "print", "clone")) {
            methods <- c(methods, name)
          }
        }
      }

      methods <- sort(methods)
      active_bindings <-
        active_bindings[!grepl("_t($|_)", active_bindings)] |>
        sort()

      # actually start printing
      if (length(classes) == 1) {
        cat("<", classes[1], "> Object\n")
      } else {
        cat(classes[1], "Object. Inherits from", toString(classes[-1]), "\n")
      }

      compression <- if (grepl("compression\\s+(\\w+)", self$writeopts)) {
        sub(".*compression\\s+(\\w+).*", "\\1", self$writeopts)
      } else {
        "none"
      }

      # Database info on one line
      cat(
        glue::glue(
          "Database: {self$path} | Format: {self$default_format} | Compression: {compression}"
        ),
        "\n\n"
      )

      tables <- self$list_tables()
      if (length(tables) > 0) {
        cat("Tables present:\n  ")
        cat(strwrap(toString(tables), width = 80), sep = "\n  ")
        cat("\n")
      } else {
        cat("Tables present: (none)\n\n")
      }

      if (length(super_names) > 0) {
        cat("DB Methods:\n  ")
        cat(strwrap(toString(super_names), width = 80), sep = "\n  ")
        cat("\n")
      }

      if (length(methods) > 0) {
        cat("Public methods:\n  ")
        cat(strwrap(toString(methods), width = 80), sep = "\n  ")
        cat("\n")
      }

      if (length(active_bindings) > 0) {
        cat("Active bindings:\n  ")
        cat(strwrap(toString(active_bindings), width = 80), sep = "\n  ")
      }

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

    ### Commit Methods ----
    #' param x Data frame to commit. If character, in-duckdb-memory table.
    #' param table_name Character string table name
    #' param autoincrement_cols Character vector of column names to auto-increment
    #' param map_cols Character vector of columns to convert to MAP format
    #' return Invisible NULL (called for side effects)
    commit_overwrite = function(
      table_name,
      all_cols,
      autoincrement_cols = character(0),
      file_info
    ) {
      # Warn if overriding existing IDs
      if (length(intersect(autoincrement_cols, all_cols)) > 0) {
        warning(glue::glue(
          "Overriding existing IDs ({toString(autoincrement_cols)}) with row numbers;\n",
          "Assign these IDs manually and do not pass any autoincrement_cols to avoid this warning"
        ))
      }

      # Build SELECT expression
      ordinary_cols <- setdiff(all_cols, autoincrement_cols)
      select_expr <- glue::glue_collapse(
        c(glue::glue("row_number() over () as {autoincrement_cols}"), ordinary_cols),
        sep = ",\n "
      )

      writeopts <- private$format_to_writeopts(file_info$format)
      self$execute(glue::glue(
        r"{
        copy (
          select {select_expr}
          from new_data_v
        ) to '{file_info$path}' ({writeopts})
        }"
      ))
    },

    #' param x Data frame to commit. If character, in-duckdb-memory table.
    #' param table_name Character string table name
    #' param autoincrement_cols Character vector of column names to auto-increment
    #' param map_cols Character vector of columns to convert to MAP format
    #' return Invisible NULL (called for side effects)
    commit_append = function(
      table_name,
      all_cols,
      autoincrement_cols = character(0),
      file_info
    ) {
      ordinary_cols <- setdiff(all_cols, autoincrement_cols)
      select_new <- glue::glue_collapse(
        c(
          glue::glue(
            "row_number() over () + getvariable('max_{autoincrement_cols}') as {autoincrement_cols}"
          ),
          ordinary_cols
        ),
        sep = ",\n "
      )

      writeopts <- private$format_to_writeopts(file_info$format)

      # Concatenation using UNION ALL; "by name" handles missing columns
      self$execute(glue::glue(
        r"{
          copy (
            select * from {table_name}
            union all by name
            select {select_new}
            from new_data_v
          )
          to '{file_info$path}' ({writeopts})
          }"
      ))
    },

    #' param x Data frame to commit. If character, in-duckdb-memory table.
    #' param table_name Character string table name
    #' param key_cols Character vector of columns that define uniqueness. If missing,
    #' use all columns starting with `id_`
    #' param autoincrement_cols Character vector of column names to auto-increment
    #' param map_cols Character vector of columns to convert to MAP format
    #' return Invisible NULL (called for side effects)
    commit_upsert = function(
      table_name,
      all_cols,
      key_cols,
      autoincrement_cols = character(0),
      file_info
    ) {
      # Update existing data
      ordinary_cols <- setdiff(all_cols, union(key_cols, autoincrement_cols))
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
          glue::glue("new_data_v.{setdiff(all_cols, autoincrement_cols)}")
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

      writeopts <- private$format_to_writeopts(file_info$format)

      self$execute(glue::glue("copy {table_name} to '{file_info$path}' ({writeopts})"))
    },

    # Get file path and format for a table
    #
    # @param table_name Character string table name
    # @param format_override Character. Optional format override
    # @return List with path, format, and exists flag
    get_file_path = function(table_name, format_override = NULL) {
      # Check for parquet first, then json
      parquet_path <- file.path(self$path, paste0(table_name, ".parquet"))
      json_path <- file.path(self$path, paste0(table_name, ".json"))

      if (file.exists(parquet_path)) {
        return(list(path = parquet_path, format = "parquet", exists = TRUE))
      } else if (file.exists(json_path)) {
        return(list(path = json_path, format = "json", exists = TRUE))
      } else {
        # Return format override or default format for new file
        use_format <- format_override %||% self$default_format
        default_path <- file.path(
          self$path,
          paste0(table_name, ".", use_format)
        )
        return(list(
          path = default_path,
          format = use_format,
          exists = FALSE
        ))
      }
    },

    # Convert format to writeopts
    #
    # @param format Character. Format name ("parquet" or "json")
    # @return Character. DuckDB write options string
    format_to_writeopts = function(format) {
      switch(
        format,
        parquet = "format parquet, compression zstd",
        json = "format json",
        stop(glue::glue("Unsupported format: {format}"))
      )
    },

    # Register new_data_v table, optionally converting MAP columns
    #
    # @param x Data to register
    # @param map_cols Character vector of columns to convert to MAP format
    # @return NULL (called for side effects)
    register_new_data_v = function(x, map_cols = character(0)) {
      if (is.character(x)) {
        # TODO add tests
        self$execute(glue::glue("create view new_data_v as from {x}"))
        return(invisible(NULL))
      }

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
    cleanup_new_data_v = function() {
      try(duckdb::duckdb_unregister(self$connection, "new_data_v"), silent = TRUE)
      try(duckdb::duckdb_unregister(self$connection, "new_data_raw"), silent = TRUE)
      try(self$execute("drop table if exists new_data_v"), silent = TRUE)
      try(self$execute("drop view if exists new_data_v"), silent = TRUE)

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
