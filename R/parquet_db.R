#' R6 Base Class for Parquet-Backed DuckDB Storage
#'
#' @description
#' A domain-agnostic R6 class that provides an interface to a folder-based data
#' storage system using DuckDB for in-memory SQL operations and parquet files
#' for efficient on-disk persistence. This class can be inherited by
#' domain-specific database classes.
#'
#' @export

parquet_db <- R6::R6Class(
  classname = "parquet_db",

  ## Public Methods ----
  public = list(
    #' @field connection DBI connection object to an in-memory DuckDB database
    connection = NULL,

    #' @field path Character string path to the data folder
    path = NULL,

    #' @field writeopts Write options for DuckDB parquet output
    writeopts = "format parquet, compression zstd",

    #' @field read_only If true, prevents writes that are not parallel-safe
    read_only = NULL,

    #' @description
    #' Initialize a new parquet_db object
    #' @param path Character string. Path to the data folder.
    #' @param read_only Logical. If true, prevents writes that are not parallel-safe.
    #' @param extensions Character vector of DuckDB extensions to load (e.g., "spatial")
    #'
    #' @return A new `parquet_db` object
    initialize = function(
      path,
      read_only = FALSE,
      extensions = character(0)
    ) {
      # Create folder if it doesn't exist
      self$path <- ensure_dir(path)

      # Create in-memory connection for SQL operations
      self$connection <- DBI::dbConnect(
        duckdb::duckdb(),
        dbdir = ":memory:"
      )
      self$read_only <- read_only

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
      result <-
        DBI::dbGetQuery(self$connection, statement)
      # set in place
      data.table::setDT(result)

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
    #' Get row count for a table (without applying id_run subsetting); returns 0
    #' if table does not exist
    #' @param table_name Character string. Name of the table to query.
    #' @return Integer number of rows
    row_count = function(table_name) {
      table_path <- self$get_table_path(table_name)
      if (!file.exists(table_path)) {
        return(0L)
      }

      self$get_query(glue::glue("select count(*) from '{table_path}'"))[[1]]
    },

    #' @description
    #' Get maximum for a column in a table (without applying id_run subsetting);
    #' returns 0 if table does not exist
    #' @param table_name Character string. Name of the table to query.
    #' @param column_name Character string. Name of the column to get the maximum value for.
    #' @return Maximum value of the column
    column_max = function(table_name, column_name) {
      table_path <- self$get_table_path(table_name)
      if (!file.exists(table_path)) {
        return(0L)
      }

      self$get_query(glue::glue('select max("{column_name}") from "{table_path}"'))[[1]]
    },

    #' @description
    #' List all tables (files and folders) in storage
    #' @return Character vector of table names
    list_tables = function() {
      list.files(self$path, pattern = "\\.parquet$", full.names = FALSE) |>
        tools::file_path_sans_ext() |>
        sort()
    },

    #' @description
    #' Fetch data from a table
    #' @param table_name Character string. Name of the table to query.
    #' @param cols SQL column selection string (e.g., "col1, col2" or "*")
    #' @param where Character string. Optional WHERE clause for the SQL query.
    #' @param limit Integer. Optional limit on number of rows to return.
    #' @param map_cols Vector of columns to be converted from key/value structs to R lists
    #'
    #' @return A data.table
    fetch = function(
      table_name,
      cols = NULL,
      where = NULL,
      limit = NULL
    ) {
      if (!file.exists(table_path <- self$get_table_path(table_name))) {
        stop("Table `", table_name, "` does not exist at `", table_path, "`")
      }

      metadata <- private$read_parquet_metadata(table_path)
      map_cols <- resolve_cols(NULL, metadata, "map_cols")
      if (!is.null(cols)) {
        map_cols <- intersect(cols, map_cols)
      }
      read_expr <- self$get_read_expr(table_name)

      # build sql query
      sql <- glue::glue("from {read_expr}")

      if (!is.null(cols)) {
        sql <- glue::glue("select {cols_to_select_expr(cols)} {sql}")
      }
      if (!is.null(where)) {
        sql <- glue::glue("{sql} where {where}")
      }
      if (!is.null(limit)) {
        sql <- glue::glue("{sql} limit {limit}")
      }

      res <- self$get_query(sql)

      # convert MAP columns back to list-columns if needed
      if (!is.null(map_cols) && nrow(res) > 0) {
        res <- convert_list_cols(res, map_cols, kv_df_to_list)
      }

      for (key in names(metadata)) {
        data.table::setattr(res, key, metadata[[key]])
      }

      res
    },

    #' @description
    #' Get table metadata
    #' @param table_name Character string. Name of the table to query.
    #' @return Named list
    get_table_metadata = function(table_name) {
      table_path <- self$get_table_path(table_name)
      if (!file.exists(table_path)) {
        stop("Table `", table_name, "` does not exist")
      }
      private$read_parquet_metadata(table_path)
    },

    #' @description
    #' Delete rows from a table
    #' @param table_name Character string. Name of the table to delete from.
    #' @param where Character string. Optional WHERE clause; if NULL, deletes all rows.
    #' @return Number of rows deleted
    delete_from = function(table_name, where = NULL) {
      stopifnot(!self$read_only)
      table_path <- self$get_table_path(table_name)

      if (!file.exists(table_path)) {
        return(0L)
      }

      count_before <- self$row_count(table_name)

      if (is.null(where)) {
        unlink(table_path, recursive = TRUE)
        return(count_before)
      }

      # Preserve existing metadata
      metadata_existing <- private$read_parquet_metadata(table_path)
      partition_clause <- resolve_partition_clause(NULL, metadata_existing)
      metadata_clause <- resolve_metadata_clause(NULL, metadata_existing)

      self$execute(glue::glue(
        r"{
        copy (
          select * from '{table_path}'
          where not ({where})
        )
        to '{table_path}' (
          {self$writeopts}
          {metadata_clause}
          {partition_clause}
        )
        }"
      ))

      count_after <- self$row_count(table_name)

      return(count_before - count_after)
    },

    #' @description
    #' Commit data using overwrite, append, or upsert modes. Handles partitioning,
    #' key identity columns, and list-to-MAP conversion. These four
    #' special column types may be passed as attributes to the `x` argument. If the
    #' table has previously been written to, these settings are recovered from the
    #' parquet metadata.
    #' @param x If data.table, the data to commit. If character, treated as an
    #' in-DuckDB-memory table or view name.
    #' @param table_name Target table name to commit to.
    #' @param method Character, one of "overwrite", "append", "upsert" (upsert being an
    #' update for existing rows, and insert for new rows; this necessitates loading the
    #' full data into memory to know what to update. This may be expensive.
    #' @return Number of rows written
    commit = function(
      x,
      table_name,
      method = c("overwrite", "append", "upsert")
    ) {
      method <- match.arg(method)

      table_path <- self$get_table_path(table_name)
      on.exit(private$cleanup_new_data_v(), add = TRUE)

      if (method == "overwrite" || !file.exists(table_path)) {
        stopifnot(!self$read_only)
        # explicit overwrite or first write: only retrieve
        private$register_new_data_v(x, map_cols = resolve_cols(x, attr = "map_cols"))

        return(private$commit_overwrite(
          table_path = table_path,
          partition_clause = resolve_partition_clause(x),
          metadata_clause = resolve_metadata_clause(x)
        ))
      }

      # fmt: skip
      {
        # schema/pre-existing metadata takes precedence
        metadata_existing  <- private$read_parquet_metadata(table_path)
        map_cols           <- resolve_cols(x, metadata_existing, "map_cols")
        key_cols           <- resolve_cols(x, metadata_existing, "key_cols")
        alternate_key_cols <- resolve_cols(x, metadata_existing, "alternate_key_cols")
        partition_cols     <- resolve_cols(x, metadata_existing, "partition_cols")
        partition_clause   <- resolve_partition_clause(x, metadata_existing)
        metadata_clause    <- resolve_metadata_clause(x, metadata_existing)
        all_new_cols       <- private$register_new_data_v(x, map_cols)
      }

      if (method == "append" || length(key_cols) == 0L) {
        # if there are no key columns to join on, upsert becomes append
        if (length(key_cols) && getOption("evoland.parquet_db_append_warning", TRUE)) {
          warning(
            "!! No uniqueness checks are performed when appending.\n",
            "  Only use if you need high speed _and_ know you're not introducing duplicates\n",
            "  Use upsert to be safe.\n",
            "  Set option 'evoland.parquet_db_append_warning' to FALSE to disable this warning."
          )
        }
        private$commit_append(
          table_path = table_path,
          partition_clause = partition_clause,
          metadata_clause = metadata_clause
        )
      } else {
        stopifnot(!self$read_only)
        private$commit_upsert(
          table_path = table_path,
          all_new_cols = all_new_cols,
          key_cols = key_cols,
          alternate_key_cols = alternate_key_cols,
          partition_cols = partition_cols,
          partition_clause = partition_clause,
          metadata_clause = metadata_clause
        )
      }
    },

    #' @description
    #' Print method for parquet_db
    #' @param subheaders optional character vector; insert as subheaders lines
    #' @param ... Not used
    #' @return self (invisibly)
    print = function(subheaders = character(0), ...) {
      # gather data to be printed
      classes <- class(self)
      classes <- classes[classes != "R6"]

      all_names <- names(self)
      methods <- character(0)
      active_bindings <- character(0)

      if (!is.null(self$.__enclos_env__$super)) {
        # exclude private super names
        super_names <- setdiff(
          ls(self$.__enclos_env__$super),
          c(
            ls(self$.__enclos_env__$super$.__enclos_env__$private),
            "initialize",
            "print",
            "clone"
          )
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
        cat("<", classes[1], "> Object", sep = "")
      } else {
        cat("<", classes[1], "> Object. Inherits from <", toString(classes[-1]), ">", sep = "")
      }

      # Basic DB descriptors
      cat("\n | Database:", self$path)
      cat("\n | Write Options:", self$writeopts)
      if (length(subheaders) > 0) {
        cat("\n |", paste(subheaders, collapse = "\n | "))
      }
      cat("\n\n")

      tables <- self$list_tables()
      if (length(tables) > 0) {
        cat("Tables Present:\n  ")
        cat(strwrap(toString(tables), width = 80), sep = "\n  ")
        cat("\n")
      } else {
        cat("Tables Present: (none)\n\n")
      }

      if (length(super_names) > 0) {
        cat("DB Methods:\n  ")
        cat(strwrap(toString(super_names), width = 80), sep = "\n  ")
        cat("\n")
      }

      if (length(methods) > 0) {
        cat("Public Methods:\n  ")
        cat(strwrap(toString(methods), width = 80), sep = "\n  ")
        cat("\n")
      }

      if (length(active_bindings) > 0) {
        cat("Active Bindings:\n  ")
        cat(strwrap(toString(active_bindings), width = 80), sep = "\n  ")
      }

      invisible(self)
    },

    #' @description Get file path (or directory path) for a table
    #' @param table_name Character string table name
    #' @return Character path
    get_table_path = function(table_name) {
      file.path(self$path, paste0(table_name, ".parquet"))
    },

    #' @description Get SQL expression to read a table
    #' @param table_name Character string table name
    #' @return Character string SQL expression
    get_read_expr = function(table_name) {
      paste0("'", self$get_table_path(table_name), "'")
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

    # overwrites table_path with pre-registered data from new_data_v
    commit_overwrite = function(
      table_path,
      partition_clause,
      metadata_clause
    ) {
      if (nzchar(partition_clause)) {
        # duckdb overwrite leaves empty partition folders in place, do it cleanly
        unlink(table_path, recursive = TRUE)
      }

      self$execute(glue::glue(
        r"{
        copy ( from new_data_v ) to '{table_path}' (
          {self$writeopts}
          {metadata_clause}
          {partition_clause}
        )
        }"
      ))
    },

    # append new_data_v to table_path; if partitioned, simply use duckdb APPEND;
    # if not partitioned, union and rewrite whole file
    commit_append = function(
      table_path,
      partition_clause,
      metadata_clause
    ) {
      if (nzchar(partition_clause)) {
        # partitioned append: simply write new data to same path with append
        self$execute(glue::glue(
          r"{
          copy new_data_v
          to '{table_path}' ({self$writeopts} {metadata_clause} {partition_clause}, append)
          }"
        ))
      } else {
        stopifnot(!self$read_only)
        # standard append: union and rewrite file; "by name" handles missing columns
        self$execute(glue::glue(
          r"{
          copy (
            from '{table_path}'
            union all by name
            from new_data_v
          )
          to '{table_path}' ({self$writeopts} {metadata_clause})
          }"
        ))
      }
    },

    commit_upsert = function(
      all_new_cols,
      key_cols,
      alternate_key_cols,
      table_path,
      partition_cols,
      partition_clause,
      metadata_clause
    ) {
      private$create_old_data_t(table_path, key_cols, alternate_key_cols)
      on.exit(self$execute("drop table old_data_t"), add = TRUE)

      # Load entire table or touched partitions into memory
      semi_join <- if (length(partition_cols)) {
        glue::glue(
          "semi join (",
          "  select distinct {cols_to_select_expr(partition_cols)} from new_data_v",
          ") using ({cols_to_select_expr(partition_cols)})"
        )
      } else {
        ""
      }
      self$execute(glue::glue(
        "insert into old_data_t from (from '{table_path}' {semi_join})"
      ))

      # Exclude key columns; because of unique constraint, this will correctly
      # error out on duplicates
      ordinary_cols <- setdiff(all_new_cols, c(key_cols, alternate_key_cols))
      update_assign_expr <- glue::glue_collapse(
        glue::glue('"{ordinary_cols}" = new_data_v."{ordinary_cols}"'),
        sep = ",\n "
      )

      # Merge new_data_v into old_data_t using key_cols
      self$execute(glue::glue(
        r"{
        merge into old_data_t
        using new_data_v
        using ({cols_to_select_expr(c(key_cols, alternate_key_cols))}) -- natural join
        when matched then update set {update_assign_expr}
        when not matched then insert by name
        }"
      ))

      if (length(partition_cols)) {
        private$cleanup_affected_partitions(table_path, partition_cols)
      }

      self$execute(glue::glue(
        "copy old_data_t to '{table_path}' (
          {self$writeopts}
          {metadata_clause}
          {partition_clause},
          overwrite_or_ignore
        )"
      ))
    },

    # register new_data_v view. If x is string, simply alias an in-memory DB object. If
    # x is data.table, optionally convert to MAP columns
    register_new_data_v = function(x, map_cols = character(0)) {
      if (is.character(x)) {
        self$execute(glue::glue("create view new_data_v as from {x}"))
        names <- self$get_query(glue::glue("select column_name from (describe {x})"))[[1]]
        return(names)
      }

      if (length(map_cols) == 0) {
        # No MAP conversion needed - register directly
        duckdb::duckdb_register(self$connection, "new_data_v", x)
        return(names(x))
      }

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

      return(names(x))
    },

    # cleanup new_data_v and related tables
    cleanup_new_data_v = function() {
      try(duckdb::duckdb_unregister(self$connection, "new_data_v"), silent = TRUE)
      try(duckdb::duckdb_unregister(self$connection, "new_data_raw"), silent = TRUE)
      try(self$execute("drop table if exists new_data_v"), silent = TRUE)
      try(self$execute("drop view if exists new_data_v"), silent = TRUE)

      invisible(NULL)
    },

    # creates an old_table_t with constraints that could not be applied using CTAS syntax
    create_old_data_t = function(table_path, key_cols, alternate_key_cols) {
      unique_key <- if (length(key_cols)) {
        glue::glue(", unique ({cols_to_select_expr(key_cols)})")
      } else {
        ""
      }
      unique_alt <- if (length(alternate_key_cols)) {
        glue::glue(", unique ({cols_to_select_expr(alternate_key_cols)})")
      } else {
        ""
      }

      colspecs <- self$get_query(glue::glue(
        "select '\"' || column_name || '\" ' || column_type from (describe '{table_path}')"
      ))[[1]]

      self$execute(glue::glue(
        r"{
        create table old_data_t (
          {toString(colspecs)}
          {unique_key}
          {unique_alt}
        )
        }"
      ))
    },

    # read parquet metadata as named list; if no metadata or no file, return empty list
    read_parquet_metadata = function(table_path) {
      if (!file.exists(table_path)) {
        return(list())
      }

      if (dir.exists(table_path)) {
        # need to glob if partitioned
        path <- glue::glue("{table_path}/**/*.parquet")
      } else {
        path <- table_path
      }

      x <- self$get_query(glue::glue(
        "select distinct key, value from parquet_kv_metadata('{path}')"
      ))

      read_raw <- function(y) {
        y |>
          rawToChar() |>
          strsplit(", ") |>
          (\(z) z[[1]])() |>
          gsub('\\"', "", x = _) |>
          utils::type.convert(as.is = TRUE)
      }
      result <- lapply(x[["value"]], read_raw)
      names(result) <- vapply(x[["key"]], read_raw, character(1))

      result
    },

    # clean up partitions folders affected by a pending upsert
    cleanup_affected_partitions = function(table_path, partition_cols) {
      # manually delete all files in affected partition folders
      # overwrite_or_ignore does not reliably delete full partitions:
      # https://github.com/duckdb/duckdb/issues/10282 might fix this
      # overwrite deletes everything and only rewrites touched partitions.
      affected_partitions <- self$get_query(glue::glue(
        "select distinct {cols_to_select_expr(partition_cols)} from new_data_v",
        partition_cols = partition_cols
      ))

      apply(affected_partitions, 1, function(row) {
        paste0(names(row), "=", row, collapse = .Platform$file.sep)
      }) |>
        file.path(table_path, partition_dir = _) |>
        unlink(recursive = TRUE)
    }
  )
)
