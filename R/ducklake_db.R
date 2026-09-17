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

# A plain reference to a table in the attached catalog. Deliberately not a
# method: `$get_read_expr()` is the overridable way to *read* a table, and
# evoland_db overrides it with a subquery that subsets by run lineage. Writes,
# and the reads documented as ignoring the active run, need the table itself,
# so they call this and are not affected by what a subclass does to reading.
table_ref <- function(table_name) {
  glue::glue('{CATALOG_ALIAS}."{table_name}"')
}

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
#' Passing `catalog` and/or `data_path` puts either half somewhere else, such
#' as a shared PostgreSQL catalog or a bucket.
#'
#' @examples
#' # needs a reachable PostgreSQL catalog and bucket credentials, so not run
#' \dontrun{
#' ducklake_db$new(
#'   path = "scratch",
#'   catalog = "postgres:dbname=evoland host=catalog.example.org",
#'   data_path = "s3://evoland/lake/"
#' )
#' }
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

    #' @field retry_wait Numeric, base wait in seconds for the retry backoff,
    #' which doubles per attempt up to 64 times this
    retry_wait = 0.1,

    #' @description
    #' Initialize a new ducklake_db object
    #' @param path Character string. Path to the data folder.
    #' @param read_only Logical. If true, the catalog is attached read-only.
    #' @param extensions Character vector of additional DuckDB extensions to load
    #' @param catalog Character string. DuckLake catalog connection; defaults to
    #' SQLite at `<path>/catalog.sqlite`. Anything DuckLake accepts works, e.g.
    #' `"postgres:dbname=evoland host=..."`.
    #' @param data_path Character string. Where DuckLake writes its data files;
    #' defaults to `<path>/data/`. May be remote, e.g. `"s3://bucket/prefix/"`.
    #' @param expire_older_than,delete_older_than Retention for `$maintain()`,
    #' as an interval DuckDB reads, e.g. `"7 days"`: how old a snapshot must be
    #' before it is expired, and how old an unreferenced file must be before it
    #' is deleted. Both are stored in the catalog, so they only need passing
    #' once, and are left at DuckLake's defaults -- which discard nothing --
    #' when not given.
    #'
    #' @return A new `ducklake_db` object
    initialize = function(
      path,
      read_only = FALSE,
      extensions = character(0),
      catalog = NULL,
      data_path = NULL,
      expire_older_than = NULL,
      delete_older_than = NULL
    ) {
      self$path <- path
      if (is.null(catalog) || is.null(data_path)) {
        # only needed for the halves that actually get stored there
        ensure_dir(path)
      }
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

      # A catalog that does not exist yet has no options set on it
      catalog_is_new <- !file.exists(sub("^(sqlite|duckdb):", "", self$catalog))

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

      # The option is persisted in the catalog, so setting it once is enough.
      # Skipping it on later opens keeps a catalog *write* off the path every
      # process takes to merely open the database.
      if (!read_only && catalog_is_new) {
        self$execute(
          glue::glue("call {CATALOG_ALIAS}.set_option('parquet_compression', 'zstd')")
        )
      }

      # persisted too, but write them whenever they are passed, so that naming
      # them against an existing database is not silently ignored
      retention <- c(
        expire_older_than = expire_older_than,
        delete_older_than = delete_older_than
      )
      for (option in names(retention)) {
        stopifnot("database is attached read-only" = !read_only)
        self$execute(glue::glue(
          "call {CATALOG_ALIAS}.set_option('{option}', '{retention[[option]]}')"
        ))
      }

      invisible(self)
    },

    ### Core Database Methods ----

    #' @description
    #' Execute a SQL statement. Retried on catalog lock contention, except
    #' inside a `$transaction()`, which is retried as a whole instead.
    #' @param statement A SQL statement
    #' @return Number of rows affected by statement
    execute = function(statement) {
      private$with_retry(function() {
        DBI::dbExecute(self$connection, statement)
      })
    },

    #' @description
    #' Execute a SQL query and return results. Reading contends for the catalog
    #' lock too -- a read landing on a SQLite catalog mid-commit fails with
    #' "Failed to query most recent snapshot for DuckLake: ... database is
    #' locked" -- so queries are retried on the same terms as writes.
    #' @param statement A SQL query statement
    #' @return A data.table with query results
    get_query = function(statement) {
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
        "select count(*) from {table_ref(table_name)}"
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
        'select max("{column_name}") from {table_ref(table_name)}'
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

      metadata <- private$read_metadata(table_name)
      map_cols <- private$col_specs(NULL, metadata)[["map_cols"]]
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

      private$read_metadata(table_name)
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

      self$execute(glue::glue(
        "delete from {table_ref(table_name)} {where_clause}"
      ))
    },

    #' @description
    #' Commit data using overwrite, append, or upsert modes. Handles partitioning,
    #' key identity columns, and list-to-MAP conversion. Which columns serve which
    #' purpose is read from `x`'s attributes, and otherwise from the metadata the
    #' target table carries -- so a character `x` inherits the target's specs,
    #' and a table first created from one has none.
    #' @param x If data.table, the data to commit. If character, treated as an
    #' in-DuckDB-memory table or view name.
    #' @param table_name Target table name to commit to.
    #' @param method Character, one of "overwrite", "append", "upsert" (upsert being an
    #' update for existing rows, and insert for new rows). Only "overwrite"
    #' changes an existing table's schema; the others reject columns the table
    #' does not have.
    #' @return Number of rows written
    commit = function(
      x,
      table_name,
      method = c("overwrite", "append", "upsert")
    ) {
      method <- match.arg(method)
      stopifnot("database is attached read-only" = !self$read_only)

      table_exists <- table_name %in% self$list_tables()

      stored <- if (table_exists) private$read_metadata(table_name) else list()
      specs <- private$col_specs(x, stored)
      metadata <- private$resolve_metadata(x, stored)

      on.exit(private$cleanup_new_data_v(), add = TRUE)
      all_new_cols <- private$register_new_data_v(x, specs[["map_cols"]])

      if (table_exists && method != "overwrite") {
        private$check_target_columns(table_name, all_new_cols)
      }

      rows <- if (method == "overwrite" || !table_exists) {
        private$commit_overwrite(table_name, specs)
      } else if (method == "append" || length(specs[["key_cols"]]) == 0L) {
        # if there are no key columns to join on, upsert becomes append
        if (
          length(specs[["key_cols"]]) &&
            getOption("evoland.ducklake_db_append_warning", TRUE)
        ) {
          warning(
            "!! No uniqueness checks are performed when appending.\n",
            "  Only use if you need high speed _and_ know you're not introducing duplicates\n",
            "  Use upsert to be safe.\n",
            "  Set option 'evoland.ducklake_db_append_warning' to FALSE to disable this warning."
          )
        }
        # "by name" tolerates columns missing from the new data
        self$execute(glue::glue(
          "insert into {table_ref(table_name)} by name (from new_data_v)"
        ))
      } else {
        private$commit_upsert(table_name, all_new_cols, specs)
      }

      # `create or replace` drops the comment, so overwrite always rewrites it;
      # otherwise it is still whatever we just read
      private$write_metadata(
        table_name,
        metadata,
        current = if (method == "overwrite" || !table_exists) list() else stored
      )

      rows
    },

    #' @description
    #' Evaluate `expr` as a single DuckLake transaction, so that several writes
    #' either all land or none do. Without one, a failure part-way through
    #' leaves the database holding half of the change.
    #'
    #' Contention is retried by re-evaluating the whole block, so `expr` must be
    #' safe to run more than once -- it re-reads whatever it derived its writes
    #' from, which is the point. Calls nest: an inner `transaction()` joins the
    #' one already open rather than starting its own.
    #' @param expr Code to evaluate inside the transaction
    #' @return The value of `expr`
    transaction = function(expr) {
      stopifnot("database is attached read-only" = !self$read_only)

      code <- substitute(expr)
      envir <- parent.frame()

      if (private$in_transaction) {
        return(eval(code, envir))
      }

      private$with_retry(function() {
        self$execute("begin transaction")
        private$in_transaction <- TRUE

        # unwinds on error and on interrupt, so that a retry -- which
        # re-enters this function -- starts from a clean connection
        on.exit({
          try(self$execute("rollback"), silent = TRUE)
          private$in_transaction <- FALSE
        })

        result <- eval(code, envir)

        # the commit is the statement that actually contends for the catalog;
        # it runs bare, and a failure sends the whole block round again
        self$execute("commit")
        on.exit(private$in_transaction <- FALSE)

        result
      })
    },

    #' @description
    #' Run DuckLake's `CHECKPOINT`, which flushes inlined data, expires
    #' snapshots, merges adjacent files, rewrites files with many deletes, and
    #' removes the files left unreferenced.
    #'
    #' Every commit adds a snapshot and leaves the files it superseded in place,
    #' and DuckLake reclaims neither on its own, so a long run grows without
    #' bound until this is called.
    #'
    #' How much is discarded is governed by the `expire_older_than` and
    #' `delete_older_than` options, set on the database (see `$new()`). Left
    #' unset they keep everything, so checkpointing an unconfigured database
    #' compacts but reclaims nothing. Expiring a snapshot gives up time travel
    #' to it, and anything still reading at one loses the files under it, so
    #' set a retention that covers the readers you expect.
    #' @return Snapshot counts before and after, invisibly
    maintain = function() {
      stopifnot("database is attached read-only" = !self$read_only)

      count_snapshots <- function() {
        self$get_query(glue::glue(
          "select count(*) from ducklake_snapshots({CATALOG_ALIAS})"
        ))[[1]]
      }

      snapshots_before <- count_snapshots()
      self$execute(glue::glue("checkpoint {CATALOG_ALIAS}"))

      invisible(c(
        snapshots_before = snapshots_before,
        snapshots_after = count_snapshots()
      ))
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

    #' @description Get SQL expression to read a table. Public because the
    #' domain functions compose their own SQL around it; they are bound with
    #' `create_method_binding()` and only receive `self`.
    #' @param table_name Character string table name
    #' @return Character string SQL expression
    get_read_expr = function(table_name) {
      table_ref(table_name)
    }
  ),

  ## Private Methods ----
  private = list(
    # R6 hook called on gc(). Deliberately only closes the connection:
    #
    # - inlined rows are not flushed. Inlining small writes into the catalog is
    #   the point of the feature, and flushing would put a catalog write on the
    #   teardown of every object, including read-only ones and the short-lived
    #   ones parallel workers open. `$maintain()` flushes, which is where a
    #   caller who wants that can ask for it.
    # - the NULL guard is load-bearing: finalize() also runs for an object
    #   whose initialize() failed before connecting, and DBI has no
    #   dbDisconnect method for NULL, so without it the finalizer itself errors.
    # - duckdb_shutdown() takes the *driver*, which is constructed inline above
    #   and never kept. Its equivalent here is dbDisconnect(shutdown = TRUE),
    #   and over 200 open/close cycles that reclaims the same memory to within
    #   0.1 MB, so it buys nothing.
    finalize = function() {
      if (!is.null(self$connection)) {
        DBI::dbDisconnect(self$connection)
        self$connection <- NULL
      }
    },

    # DuckLake itself does not pull in the extensions that back the catalog and
    # the data files, so derive them from where those were pointed
    backend_extensions = function() {
      catalog_ext <- switch(
        sub(":.*$", "", self$catalog),
        sqlite = "sqlite",
        postgres = , # fallthrough
        postgresql = "postgres",
        # duckdb-file catalogs and anything unrecognised need nothing extra;
        # a caller wanting one of those can name it in `extensions`
        NULL
      )

      remote_data <- grepl("^(s3|gcs|r2|az|abfss?|https?)://", self$data_path)

      c(catalog_ext, if (remote_data) "httpfs")
    },

    # whether a transaction() is open on this connection. Both of the things
    # that have to know are downstream of this one fact: a nested transaction()
    # joins the open one rather than starting another, since DuckDB has no
    # nested transactions, and a statement is retried alone only when it is not
    # part of one.
    in_transaction = FALSE,

    # Retry a catalog operation in the face of lock contention. DuckLake's own
    # `ducklake_max_retry_count` covers logical snapshot conflicts, but not
    # contention on the catalog itself; uncoordinated writers need this wrapper
    # to all get through. Only transient errors are retried, with exponential
    # backoff and jitter; anything else is re-raised immediately.
    with_retry = function(fn) {
      # A statement of an open transaction must not be retried on its own,
      # because replaying one statement of an aborted transaction would not
      # redo the rest. It runs bare and transaction() replays the whole block.
      if (private$in_transaction) {
        return(fn())
      }

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

        # The doubling stops at 64x the base wait: unchecked, the last of
        # retry_max attempts would be hours apart, but the wait still has to
        # grow enough to sit out a large commit holding the catalog lock.
        Sys.sleep(stats::runif(1L, 0, self$retry_wait * 2^min(attempt - 1L, 6L)))
      }
    },

    ### Commit Methods ----

    # replace table_name wholesale with pre-registered data from new_data_v
    commit_overwrite = function(table_name, specs) {
      target <- table_ref(table_name)

      # one transaction, so that a concurrent reader never observes the table
      # in its intermediate, empty state
      self$transaction({
        # create the table empty first, so that partitioning is already in
        # effect for the initial batch of rows
        self$execute(glue::glue(
          "create or replace table {target} as from new_data_v limit 0"
        ))

        # partitioning is a pruning hint only; set it once, at table creation
        if (length(specs[["partition_cols"]])) {
          self$execute(glue::glue(
            "alter table {target}
             set partitioned by ({cols_to_select_expr(specs[['partition_cols']])})"
          ))
        }

        self$execute(glue::glue(
          "insert into {target} by name (from new_data_v)"
        ))
      })
    },

    commit_upsert = function(table_name, all_new_cols, specs) {
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

      self$execute(glue::glue(
        r"{
        merge into {table_ref(table_name)}
        using new_data_v
        using ({cols_to_select_expr(key_cols)}) -- natural join
        when matched then update set {update_assign_expr}
        when not matched then insert by name
        }"
      ))
    },

    # A table's schema is fixed once created. Deleting every row no longer
    # drops the table, so the old "delete, then insert with an extra column"
    # route to a schema change is gone; say so rather than letting the insert
    # fail on a column the caller has to spot for themselves.
    check_target_columns = function(table_name, all_new_cols) {
      target_cols <- self$get_query(glue::glue(
        "select column_name from (describe {table_ref(table_name)})"
      ))[[1]]

      unknown_cols <- setdiff(all_new_cols, target_cols)
      if (length(unknown_cols) == 0L) {
        return(invisible(NULL))
      }

      stop(glue::glue(
        "Cannot commit columns that `{table_name}` does not have: ",
        "{toString(unknown_cols)}\n",
        "  table has: {toString(target_cols)}\n",
        '  use method = "overwrite" to replace the table and its schema'
      ))
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
        from {table_ref(table_name)} t
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
        # temp, because a transaction that has written to the catalog may not
        # also write to `memory`, and a plain view would land there
        self$execute(glue::glue("create or replace temp view new_data_v as from {x}"))
        return(self$get_query(glue::glue("select column_name from (describe {x})"))[[1]])
      }

      # DuckLake has no ENUM type, so factors are stored as strings; the
      # as_<table>_t() constructors cast them back on the way out
      factor_cols <- names(x)[vapply(x, is.factor, logical(1))]

      if (length(factor_cols) || length(map_cols)) {
        x <- data.table::copy(x)
        for (col in factor_cols) {
          data.table::set(x, j = col, value = as.character(x[[col]]))
        }
      }

      if (length(map_cols) == 0) {
        duckdb::duckdb_register(self$connection, "new_data_v", x)
        return(names(x))
      }

      # duckdb_register() cannot turn an R named list into a MAP, so pass it
      # through a key/value intermediate and rebuild the MAP in SQL
      x <- convert_list_cols(x, map_cols, list_to_kv_df)
      duckdb::duckdb_register(self$connection, "new_data_raw", x)

      select_expr <- glue::glue_collapse(
        c(
          setdiff(names(x), map_cols),
          glue::glue("map_from_entries({map_cols}) as {map_cols}")
        ),
        sep = ", "
      )
      self$execute(glue::glue(
        "create or replace temp table new_data_v as select {select_expr} from new_data_raw"
      ))

      names(x)
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

    # Which columns are keys, maps or partitions comes from the data being
    # committed, where as_ducklake_db_t() put it, and otherwise from what the
    # table already carries -- which is the same declaration, stored by the
    # commit that created it. The live object wins, so changing a spec in a
    # constructor takes effect on the next commit rather than being pinned by
    # whatever the table was created with.
    col_specs = function(x, stored) {
      lapply(
        stats::setNames(
          nm = c("key_cols", "alternate_key_cols", "map_cols", "partition_cols")
        ),
        function(spec) attr(x, spec) %||% stored[[spec]] %||% character(0)
      )
    },

    # The stored metadata, without get_table_metadata()'s existence check, for
    # callers that have already established that the table is there
    read_metadata = function(table_name) {
      comment <- self$get_query(glue::glue(
        "select comment from duckdb_tables()
         where database_name = '{CATALOG_ALIAS}' and table_name = '{table_name}'"
      ))[[1]]

      deserialize_metadata(comment)
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

      if (inherits(x, "ducklake_db_t")) {
        out[["ducklake_db_t_class"]] <- class(x)[1L]
      }

      out
    },

    # Writing the comment is a catalog change, and so costs a snapshot. Most
    # commits do not touch the metadata at all, so only write when it differs
    # from what the table already carries.
    write_metadata = function(table_name, metadata, current = list()) {
      comment <- serialize_metadata(metadata)

      if (identical(comment, serialize_metadata(current))) {
        return(invisible(NULL))
      }

      self$execute(glue::glue(
        "comment on table {table_ref(table_name)} is {quoted}",
        quoted = if (nzchar(comment)) {
          paste0("'", gsub("'", "''", comment), "'")
        } else {
          "NULL"
        }
      ))
    }
  )
)
