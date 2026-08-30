# Only ever one DuckLake catalog is attached per connection, so its alias is a constant
# rather than something a caller should set.
CATALOG_ALIAS <- "ducklake_db"

# Contention shows up differently per catalog backend: the first two come from
# SQLite and DuckDB-file catalogs, the last from DuckLake's own commit path.
# A server-backed catalog reports its own; add them here when one is adopted.
TRANSIENT_CATALOG_ERRORS <- paste(
  "database is locked",
  "Could not set lock on file",
  "Failed to commit DuckLake transaction",
  sep = "|"
)

#' R6 Base Class for DuckLake-Backed Storage
#'
#' @description
#' A domain-agnostic R6 class that provides an interface to a folder-based data
#' storage system. An in-memory DuckDB instance attaches a DuckLake catalog, so
#' that writes are atomic and readers see snapshot-isolated data even while
#' other processes are writing. This class can be inherited by domain-specific
#' database classes.
#'
#' By default a database is one self-contained folder: the catalog lives in
#' SQLite at `<path>/catalog.sqlite` and the data files at `<path>/data/`.
#' Passing `catalog` and/or `data_path` puts either half somewhere else, for
#' instance a shared PostgreSQL catalog or a bucket:
#'
#' @examples
#' ducklake_db$new(
#'   path = "scratch",
#'   catalog = "postgres:dbname=evoland host=catalog.example.org",
#'   data_path = "s3://evoland/lake/"
#' )
#'
#' @export

ducklake_db <- R6::R6Class(
  classname = "ducklake_db",

  ## Public Methods ----
  public = list(
    #' @field connection DBI connection object to an in-memory DuckDB database
    connection = NULL,

    #' @field path Character string path to the data folder
    path = NULL,

    #' @field catalog Character string, the DuckLake catalog connection, e.g.
    #' `"sqlite:<path>/catalog.sqlite"`
    catalog = NULL,

    #' @field data_path Character string, where DuckLake writes its data files
    data_path = NULL,

    #' @field read_only If true, the catalog is attached read-only
    read_only = NULL,

    #' @field retry_max Integer, how often a contended catalog write is retried
    retry_max = 20L,

    #' @field retry_wait Numeric, base wait in seconds for the retry backoff
    retry_wait = 0.1,

    #' @description
    #' Initialize a new ducklake_db object
    #' @param path Character string. Path to the data folder.
    #' @param read_only Logical. If true, the catalog is attached read-only.
    #' @param extensions Character vector of additional DuckDB extensions to load
    #' @param catalog Character string. DuckLake catalog connection; defaults to
    #' SQLite at `<path>/catalog.sqlite`. Anything DuckLake accepts works, e.g.
    #' `"postgres:dbname=evoland host=..."` or `"mysql:..."`.
    #' @param data_path Character string. Where DuckLake writes its data files;
    #' defaults to `<path>/data/`. May be remote, e.g. `"s3://bucket/prefix/"`.
    #'
    #' @return A new `ducklake_db` object
    initialize = function(
      path,
      read_only = FALSE,
      extensions = character(0),
      catalog = NULL,
      data_path = NULL
    ) {
      # the local folder is only needed if either catalog or data get stored there
      self$path <- if (is.null(catalog) || is.null(data_path)) ensure_dir(path) else path
      self$catalog <- catalog %||% glue::glue("sqlite:{file.path(path, 'catalog.sqlite')}")
      self$data_path <- data_path %||% paste0(ensure_dir(file.path(path, "data")), "/")
      self$read_only <- read_only

      # `shared_home = TRUE` pins DuckDB's extension and secret storage to ~/.duckdb.
      # Stating the choice explicitly silences the storage-location message that duckdb
      # emits whenever the location is resolved implicitly, and keeps extensions cached
      # across sessions instead of re-downloading them on every instantiation.
      self$connection <- DBI::dbConnect(
        duckdb::duckdb(shared_home = TRUE),
        dbdir = ":memory:"
      )

      # ducklake plus whatever backs the catalog and the data files are on the
      # critical path for opening the database at all
      for (ext in unique(c("ducklake", private$backend_extensions(), extensions))) {
        self$execute(glue::glue("install {ext}; load {ext};"))
      }

      # DuckLake resolves logical snapshot conflicts itself; catalog lock contention
      # is handled by private$with_retry()
      self$execute("set ducklake_max_retry_count = 40")

      private$with_retry(function() {
        self$execute(glue::glue(
          "attach 'ducklake:{self$catalog}' as {CATALOG_ALIAS} ({options_str})",
          options_str = glue::glue_collapse(
            c(
              glue::glue("DATA_PATH '{self$data_path}'"),
              if (read_only) "READ_ONLY"
            ),
            sep = ", "
          )
        ))
      })

      if (!read_only) {
        # persisted in the catalog, so this only does work on first open
        private$with_retry(function() {
          self$execute(
            glue::glue("call {CATALOG_ALIAS}.set_option('parquet_compression', 'zstd')")
          )
        })
      }

      invisible(self)
    },

    ### Core Database Methods ----

    #' @description
    #' Execute a SQL statement
    #' @param statement A SQL statement
    #' @return Number of rows affected by statement
    execute = function(statement) {
      # TODO why not with_retry each dbExecute to handle lock contention, is the overhead so great?
      DBI::dbExecute(self$connection, statement)
    },

    #' @description
    #' Execute a SQL query and return results. Reads contend for the catalog
    #' lock just as writes do, so they are retried on the same terms.
    #' @param statement A SQL query statement
    #' @return A data.table with query results
    get_query = function(statement) {
      # TODO check if reading actually causes lock contention (try on sqlite and duckdb)
      result <- private$with_retry(function() {
        DBI::dbGetQuery(self$connection, statement)
      })
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
      if (!table_name %in% self$list_tables()) {
        return(0L)
      }

      self$get_query(glue::glue(
        "select count(*) from {private$table_ref(table_name)}"
      ))[[1]]
    },

    #' @description
    #' Get maximum for a column in a table (without applying id_run subsetting);
    #' returns 0 if table does not exist
    #' @param table_name Character string. Name of the table to query.
    #' @param column_name Character string. Name of the column to get the maximum value for.
    #' @return Maximum value of the column
    column_max = function(table_name, column_name) {
      if (!table_name %in% self$list_tables()) {
        return(0L)
      }

      self$get_query(glue::glue(
        'select max("{column_name}") from {private$table_ref(table_name)}'
      ))[[1]]
    },

    #' @description
    #' List all tables in storage
    #' @return Character vector of table names
    list_tables = function() {
      self$get_query(glue::glue(
        "select table_name from information_schema.tables
         where table_catalog = '{CATALOG_ALIAS}'
         order by table_name"
      ))[[1]]
    },

    #' @description
    #' Fetch data from a table
    #' @param table_name Character string. Name of the table to query.
    #' @param cols SQL column selection string (e.g., "col1, col2" or "*")
    #' @param where Character string. Optional WHERE clause for the SQL query.
    #' @param limit Integer. Optional limit on number of rows to return.
    #'
    #' @return A data.table
    fetch = function(
      table_name,
      cols = NULL,
      where = NULL,
      limit = NULL
    ) {
      if (!table_name %in% self$list_tables()) {
        stop("Table `", table_name, "` does not exist in `", self$path, "`")
      }

      metadata <- self$get_table_metadata(table_name)
      map_cols <- private$col_specs(table_name)[["map_cols"]]
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
      if (length(map_cols) > 0 && nrow(res) > 0) {
        res <- convert_list_cols(res, map_cols, kv_df_to_list)
      }

      for (key in names(metadata)) {
        data.table::setattr(res, key, metadata[[key]])
      }

      res
    },

    #' @description
    #' Get table metadata, stored as a comment on the catalog table
    #' @param table_name Character string. Name of the table to query.
    #' @return Named list
    get_table_metadata = function(table_name) {
      if (!table_name %in% self$list_tables()) {
        stop("Table `", table_name, "` does not exist")
      }

      comment <- self$get_query(glue::glue(
        "select comment from duckdb_tables()
         where database_name = '{CATALOG_ALIAS}' and table_name = '{table_name}'"
      ))[[1]]

      deserialize_metadata(comment)
    },

    #' @description
    #' Delete rows from a table
    #' @param table_name Character string. Name of the table to delete from.
    #' @param where Character string. Optional WHERE clause; if NULL, deletes all rows.
    #' @return Number of rows deleted
    delete_from = function(table_name, where = NULL) {
      stopifnot(!self$read_only)

      if (!table_name %in% self$list_tables()) {
        return(0L)
      }

      where_clause <- if (is.null(where)) "" else glue::glue("where {where}")

      private$with_retry(function() {
        self$execute(glue::glue(
          "delete from {private$table_ref(table_name)} {where_clause}"
        ))
      })
    },

    #' @description
    #' Commit data using overwrite, append, or upsert modes. Handles partitioning,
    #' key identity columns, and list-to-MAP conversion. Which columns serve which
    #' purpose is read from the table's `as_<table_name>()` prototype where one
    #' exists, and otherwise from `x`'s attributes or the stored table metadata.
    #' @param x If data.table, the data to commit. If character, treated as an
    #' in-DuckDB-memory table or view name.
    #' @param table_name Target table name to commit to.
    #' @param method Character, one of "overwrite", "append", "upsert" (upsert being an
    #' update for existing rows, and insert for new rows).
    #' @return Number of rows written
    commit = function(
      x,
      table_name,
      method = c("overwrite", "append", "upsert")
    ) {
      method <- match.arg(method)

      specs <- private$col_specs(table_name, x)
      on.exit(private$cleanup_new_data_v(), add = TRUE)
      all_new_cols <- private$register_new_data_v(x, specs[["map_cols"]])

      table_exists <- table_name %in% self$list_tables()
      metadata <- private$resolve_metadata(
        x,
        if (table_exists) self$get_table_metadata(table_name) else list()
      )

      if (method == "overwrite" || !table_exists) {
        stopifnot(!self$read_only)
        return(private$commit_overwrite(table_name, specs, metadata))
      }

      if (method == "append" || length(specs[["key_cols"]]) == 0L) {
        # if there are no key columns to join on, upsert becomes append
        if (
          length(specs[["key_cols"]]) &&
            getOption("evoland.parquet_db_append_warning", TRUE)
        ) {
          warning(
            "!! No uniqueness checks are performed when appending.\n",
            "  Only use if you need high speed _and_ know you're not introducing duplicates\n",
            "  Use upsert to be safe.\n",
            "  Set option 'evoland.parquet_db_append_warning' to FALSE to disable this warning."
          )
        }
        return(private$commit_append(table_name, metadata))
      }

      stopifnot(!self$read_only)
      private$commit_upsert(table_name, all_new_cols, specs, metadata)
    },

    #' @description
    #' Print method for ducklake_db
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
      cat("\n | Catalog:", self$catalog)
      cat("\n | Data Path:", self$data_path)
      cat("\n | Read Only:", self$read_only)
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

    # TODO can we move this to be a private function? or can children (evoland_db) not overwrite the method then?
    #' @description Get SQL expression to read a table
    #' @param table_name Character string table name
    #' @return Character string SQL expression
    get_read_expr = function(table_name) {
      private$table_ref(table_name)
    }
  ),

  ## Private Methods ----
  private = list(
    # R6 hook called on gc(); Close the database connection
    finalize = function() {
      # TODO do we want to flush inlined data on cleanup? use ducklake_flush_inlined_data
      # TODO do we need to hedge against connection being null?
      if (!is.null(self$connection)) {
        # TODO why not use duckdb_shutdown?
        DBI::dbDisconnect(self$connection)
        # why do we need to assign null, does that help the GC?
        self$connection <- NULL
      }
    },

    # fully qualified reference to a table in the attached catalog
    table_ref = function(table_name) {
      glue::glue('{CATALOG_ALIAS}."{table_name}"')
    },

    # DuckLake itself does not pull in the extensions that back the catalog and
    # the data files, so derive them from where those were pointed
    backend_extensions = function() {
      catalog_ext <- switch(
        sub(":.*$", "", self$catalog),
        sqlite = "sqlite",
        postgres = , # fallthrough
        postgresql = "postgres",
        mysql = "mysql",
        # duckdb-file catalogs and anything unrecognised need nothing extra
        NULL
      )

      remote_data <- grepl("^(s3|gcs|r2|az|abfss?|https?)://", self$data_path)

      c(catalog_ext, if (remote_data) "httpfs")
    },

    # Retry a catalog operation in the face of lock contention. DuckLake's own
    # `ducklake_max_retry_count` covers logical snapshot conflicts, but not
    # contention on the catalog itself; uncoordinated writers need this wrapper
    # to all get through. Only transient errors are retried, with exponential
    # backoff and jitter; anything else is re-raised immediately.
    with_retry = function(fn) {
      for (attempt in seq_len(self$retry_max)) {
        result <- try(fn(), silent = TRUE)

        if (!inherits(result, "try-error")) {
          return(result)
        }

        condition <- attr(result, "condition")
        if (
          !grepl(TRANSIENT_CATALOG_ERRORS, conditionMessage(condition)) ||
            attempt == self$retry_max
        ) {
          stop(condition)
        }

        Sys.sleep(stats::runif(1L, 0, self$retry_wait * 2^(attempt - 1L)))
      }
    },

    ### Commit Methods ----

    # replace table_name wholesale with pre-registered data from new_data_v
    commit_overwrite = function(table_name, specs, metadata) {
      table_ref <- private$table_ref(table_name)

      # one transaction, so that a concurrent reader never observes the table
      # in its intermediate, empty state
      rows <- private$with_retry(function() {
        self$execute("begin transaction")
        on.exit(try(self$execute("rollback"), silent = TRUE), add = TRUE)

        # create the table empty first, so that partitioning is already in
        # effect for the initial batch of rows
        self$execute(glue::glue(
          "create or replace table {table_ref} as from new_data_v limit 0"
        ))

        # partitioning is a pruning hint only; set it once, at table creation
        if (length(specs[["partition_cols"]])) {
          self$execute(glue::glue(
            "alter table {table_ref}
             set partitioned by ({cols_to_select_expr(specs[['partition_cols']])})"
          ))
        }

        inserted <- self$execute(glue::glue(
          "insert into {table_ref} by name (from new_data_v)"
        ))

        self$execute("commit")
        on.exit(NULL)

        inserted
      })

      # create or replace drops the comment carrying the metadata
      private$write_metadata(table_name, metadata)

      rows
    },

    # append new_data_v to table_name; "by name" tolerates missing columns
    commit_append = function(table_name, metadata) {
      rows <- private$with_retry(function() {
        self$execute(glue::glue(
          "insert into {private$table_ref(table_name)} by name (from new_data_v)"
        ))
      })

      private$write_metadata(table_name, metadata)

      rows
    },

    commit_upsert = function(table_name, all_new_cols, specs, metadata) {
      key_cols <- specs[["key_cols"]]
      alternate_key_cols <- specs[["alternate_key_cols"]]

      private$check_source_uniqueness(table_name, key_cols, alternate_key_cols)

      # Alternate keys identify the same rows as the primary key, so they are
      # never updated; excluding them keeps the mapping between the two intact.
      ordinary_cols <- setdiff(all_new_cols, c(key_cols, alternate_key_cols))
      update_assign_expr <- glue::glue_collapse(
        glue::glue('"{ordinary_cols}" = new_data_v."{ordinary_cols}"'),
        sep = ",\n "
      )

      rows <- private$with_retry(function() {
        self$execute(glue::glue(
          r"{
          merge into {private$table_ref(table_name)}
          using new_data_v
          using ({cols_to_select_expr(key_cols)}) -- natural join
          when matched then update set {update_assign_expr}
          when not matched then insert by name
          }"
        ))
      })

      private$write_metadata(table_name, metadata)

      rows
    },

    # DuckLake supports no constraints, keys or indexes, and MERGE silently
    # inserts duplicates when the source itself has duplicate keys. Both gaps
    # have to be closed before the merge runs.
    check_source_uniqueness = function(table_name, key_cols, alternate_key_cols) {
      for (cols in list(key_cols, alternate_key_cols)) {
        if (length(cols) == 0L) {
          next
        }
        select_expr <- cols_to_select_expr(cols)
        duplicates <- self$get_query(glue::glue(
          "select count(*) - count(distinct ({select_expr})) from new_data_v"
        ))[[1]]
        if (duplicates > 0) {
          stop(glue::glue(
            "Duplicate key found in data to commit to `{table_name}`\n",
            "  columns: {toString(cols)}\n",
            "  duplicate rows: {duplicates}"
          ))
        }
      }

      if (length(alternate_key_cols) == 0L || length(key_cols) == 0L) {
        return(invisible(NULL))
      }

      # An alternate key already held by a different primary key would be
      # inserted as a duplicate, because the merge joins on the primary key only
      key_differs <- glue::glue_collapse(
        glue::glue('t."{key_cols}" is distinct from n."{key_cols}"'),
        sep = " or "
      )
      stolen_keys <- self$get_query(glue::glue(
        r"{
        select count(*)
        from {private$table_ref(table_name)} t
        join new_data_v n using ({cols_to_select_expr(alternate_key_cols)})
        where {key_differs}
        }"
      ))[[1]]

      if (stolen_keys > 0) {
        stop(glue::glue(
          "Duplicate key found in data to commit to `{table_name}`\n",
          "  {stolen_keys} row(s) reuse an existing {toString(alternate_key_cols)} ",
          "under a different {toString(key_cols)}"
        ))
      }

      invisible(NULL)
    },

    # register new_data_v view. If x is string, simply alias an in-memory DB object. If
    # x is data.table, optionally convert to MAP columns
    register_new_data_v = function(x, map_cols = character(0)) {
      if (is.character(x)) {
        self$execute(glue::glue("create view new_data_v as from {x}"))
        names <- self$get_query(glue::glue("select column_name from (describe {x})"))[[1]]
        return(names)
      }

      # DuckLake has no ENUM type, so factors are stored as strings; the
      # as_<table>_t() constructors cast them back on the way out
      factor_cols <- names(x)[vapply(x, is.factor, logical(1))]

      if (length(map_cols) == 0 && length(factor_cols) == 0) {
        # No conversion needed - register directly
        duckdb::duckdb_register(self$connection, "new_data_v", x)
        return(names(x))
      }

      x <- data.table::copy(x)
      for (col in factor_cols) {
        data.table::set(x, j = col, value = as.character(x[[col]]))
      }

      if (length(map_cols) == 0) {
        duckdb::duckdb_register(self$connection, "new_data_v", x)
        return(names(x))
      }

      # Convert list columns to key-value dataframes
      x <- convert_list_cols(x, map_cols, list_to_kv_df)

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

    ### Column and Metadata Resolution ----

    # Which columns are keys, maps or partitions is declared in the as_<table>_t()
    # constructor, so an empty prototype answers the question without a round-trip
    # through storage. Data committed to a table without a constructor falls back
    # to attributes on the data, or to what the table was created with.
    col_specs = function(table_name, x = NULL) {
      prototype_fn <- paste0("as_", table_name)
      prototype <- if (exists(prototype_fn, mode = "function")) {
        get(prototype_fn, mode = "function")()
      }

      stored <- if (is.null(prototype) && table_name %in% self$list_tables()) {
        self$get_table_metadata(table_name)
      } else {
        list()
      }

      lapply(
        stats::setNames(
          nm = c("key_cols", "alternate_key_cols", "map_cols", "partition_cols")
        ),
        function(spec) {
          cols <-
            attr(prototype, spec) %||%
            attr(x, spec) %||%
            stored[[spec]]
          if (is.null(cols)) character(0) else cols
        }
      )
    },

    # Merge the atomic attributes of `x` into the metadata a table already
    # carries. Existing metadata wins, because it cannot be safely overwritten
    # by a partial commit; non-atomic values are dropped with a warning.
    resolve_metadata = function(x, existing = list()) {
      new_metadata <- attributes(x)

      names_to_add <- setdiff(
        names(new_metadata),
        c(
          names(existing),
          # exclude data.table attributes
          "class",
          "names",
          ".internal.selfref",
          "row.names",
          "sorted",
          "index"
        )
      )

      out <- c(existing, new_metadata[names_to_add])

      for (key in names(out)[!vapply(out, is.atomic, logical(1))]) {
        warning(glue::glue(
          "Metadata key '{key}' has non-atomic value; dropping metadata"
        ))
      }
      out <- Filter(is.atomic, out)

      if (inherits(x, "parquet_db_t")) {
        out[["parquet_db_t_class"]] <- class(x)[1L]
      }

      out
    },

    write_metadata = function(table_name, metadata) {
      comment <- serialize_metadata(metadata)

      private$with_retry(function() {
        self$execute(glue::glue(
          "comment on table {private$table_ref(table_name)} is {quoted}",
          quoted = if (nzchar(comment)) {
            paste0("'", gsub("'", "''", comment), "'")
          } else {
            "NULL"
          }
        ))
      })
    }
  )
)
