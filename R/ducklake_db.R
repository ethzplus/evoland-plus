# Contention shows up differently per catalog backend: the first two come from
# SQLite and DuckDB-file catalogs, the last from DuckLake's own commit path.
# A server-backed catalog reports its own; add them here when one is adopted.
TRANSIENT_CATALOG_ERRORS <- paste(
  "database is locked",
  "Could not set lock on file",
  "Failed to commit DuckLake transaction",
  sep = "|"
)

# regex to identify remote URIs
REMOTE_URI_PREFIX <- "^(s3|gcs|r2|az|abfss?|https?)://"

# Where `$next_id()` keeps the next value of each id it hands out. One row per
# id, so it stays far inside DuckLake's inlining limit and costs a catalog row
# rather than a data file.
ID_ALLOC_TABLE <- "ducklake_db_id_alloc"

# Tables this class keeps for itself, which `$list_tables()` hides unless
# asked, so that they are not mistaken for a caller's data
INTERNAL_TABLES <- ID_ALLOC_TABLE

collapse_path_separators <- function(path) {
  scheme <- sub(paste0("^((", substring(REMOTE_URI_PREFIX, 2L), ")?).*$"), "\\1", path)
  paste0(scheme, gsub("/{2,}", "/", substring(path, nchar(scheme) + 1L)))
}

# A plain reference to a table in the attached catalog. Deliberately not a
# method: `$get_read_expr()` is the overridable way to *read* a table, and
# evoland_db overrides it with a subquery that subsets by run lineage. Writes,
# and the reads documented as ignoring the active run, need the table itself,
# so they call this and are not affected by what a subclass does to reading.
table_ref <- function(table_name) {
  # DBI::SQL, so that interpolating this into a glue_sql() statement inserts the
  # reference rather than quoting it as a string literal
  DBI::SQL(glue::glue('dl_db."{table_name}"'))
}

#' R6 Base Class for DuckLake-Backed Storage
#'
#' @description
#' A domain-agnostic R6 class that provides an interface to a folder-based data storage system. An
#' in-memory [DuckDB](https://duckdb.org/) instance attaches a [DuckLake](https://ducklake.select/)
#' catalog, so that writes are atomic and readers see snapshot-isolated data even while other
#' processes are writing. This class can be inherited by domain-specific database classes.
#'
#' By default a database is one self-contained folder: the catalog lives in SQLite at
#' `<path>/catalog.sqlite` and the parquet data files at `<path>/data/`. Passing `catalog` and/or
#' `data_path` puts either half somewhere else, such as a shared PostgreSQL catalog or an S3 bucket.
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

    #' @field catalog Character string, the DuckLake catalog connection, e.g.
    #' `"sqlite:<path>/catalog.sqlite"`
    catalog = NULL,

    #' @field data_path Character string, where DuckLake writes its data files
    data_path = NULL,

    #' @field read_only If true, the catalog is attached read-only
    read_only = NULL,

    #' @field retry_timeout Numeric, how long in seconds to keep retrying a
    #' contended catalog operation before giving up. This, not `retry_max`, is
    #' the limit that normally applies: how many attempts fit in the budget
    #' depends on how long each contended attempt blocks, which is not
    #' something a caller can predict.
    retry_timeout = 300,

    #' @field retry_wait Numeric, base wait in seconds for the retry backoff,
    #' which doubles per attempt up to 64 times this
    retry_wait = 0.1,

    #' @field retry_max Integer, maximum number of attempts in case something retries
    #' faster than `retry_timeout` can measure.
    retry_max = 2000L,

    #' @description
    #' Initialize a new ducklake_db object
    #' @param path Character string. Path to the data folder. Only needed for
    #' the halves of the database that are derived from it, so it may be
    #' omitted when both `catalog` and `data_path` are given.
    #' @param read_only Logical. If true, the catalog is attached read-only.
    #' @param extensions Character vector of additional DuckDB extensions to load
    #' @param catalog Character string. DuckLake catalog connection; defaults to SQLite
    #' at `<path>/catalog.sqlite`. Anything DuckLake accepts works, e.g.
    #' `"postgres:dbname=evoland host=..."`. See [Choosing a catalog
    #' database](https://ducklake.select/docs/stable/duckdb/usage/choosing_a_catalog_database)
    #' @param data_path Character string. Where DuckLake writes its data files; defaults
    #' to `<path>/data/`. May be remote, e.g. `"s3://bucket/prefix/"`. See [Choosing
    #' storage](https://ducklake.select/docs/stable/duckdb/usage/choosing_storage)
    #' @param sqlite_journal_mode Character string, the journal mode to put a SQLite
    #' catalog in; [`wal`](https://www.sqlite.org/wal.html) by default, because the
    #' default rollback journal lets one writer block every other process's *reads* of
    #' the catalog. `NULL` leaves the catalog as it is. Ignored for catalogs that are
    #' not SQLite or when attaching read-only. Readers adapt to whatever journal mode
    #' they find.
    #' @param expire_older_than,delete_older_than Retention for `$checkpoint()`, as an
    #' interval DuckDB reads, e.g. `"7 days"`: how old a snapshot must be before it is
    #' expired, and how old an unreferenced file must be before it is deleted. Stored in
    #' the catalog but can be overwritten by passing. Default to NULL, discarding
    #' nothing.
    #'
    #' @return A new `ducklake_db` object
    initialize = function(
      path = NULL,
      read_only = FALSE,
      extensions = character(0),
      catalog = NULL,
      data_path = NULL,
      sqlite_journal_mode = "wal",
      expire_older_than = NULL,
      delete_older_than = NULL
    ) {
      # `path` is only the place the defaults are derived from, so naming both
      # halves leaves nothing for it to do
      derives_from_path <- is.null(catalog) || is.null(data_path)
      stopifnot(
        "`path` is required unless both `catalog` and `data_path` are given" = !derives_from_path ||
          (is.character(path) && length(path) == 1L)
      )

      if (derives_from_path) {
        ensure_dir(path)
      }
      self$catalog <- collapse_path_separators(
        catalog %||% glue::glue("sqlite:{file.path(path, 'catalog.sqlite')}")
      )
      self$data_path <- collapse_path_separators(
        data_path %||% paste0(ensure_dir(file.path(path, "data")), "/")
      )

      self$read_only <- read_only

      # in-memory duckdb connection; shared_home=TRUE pins extension and secret storage
      # to ~/.duckdb and silences startup noise
      self$connection <- DBI::dbConnect(duckdb::duckdb(shared_home = TRUE))

      # backend_extensions needed for catalog (sqlite, postgres...)
      for (ext in unique(c("ducklake", private$backend_extensions(), extensions))) {
        self$execute(glue::glue("install {ext}; load {ext};"))
      }

      # build attach statement
      journal_option <- if (
        !read_only &&
          !is.null(sqlite_journal_mode) &&
          grepl("^sqlite:.*$", self$catalog)
      ) {
        glue::glue("METADATA_PARAMETERS MAP{{'journal_mode': '{sqlite_journal_mode}'}}")
      }

      self$execute(glue::glue(
        "attach 'ducklake:{self$catalog}' as dl_db ({options_str})",
        options_str = glue::glue_collapse(
          c(
            glue::glue("DATA_PATH '{self$data_path}'"),
            journal_option,
            if (read_only) "READ_ONLY"
          ),
          sep = ", "
        )
      ))

      # options are persisted, but easiest just in case to set like this
      if (!read_only) {
        options <- c(
          parquet_compression = "zstd",
          expire_older_than = expire_older_than,
          delete_older_than = delete_older_than
        )
        for (option in names(options)) {
          self$execute(glue::glue(
            "call dl_db.set_option('{option}', '{options[[option]]}')"
          ))
        }
      }

      invisible(self)
    },

    ### Core Database Methods ----

    #' @description
    #' Execute a SQL statement. Retried on catalog lock contention, except inside a
    #' `$transaction()`, which is retried as a whole instead.
    #' @param statement A SQL statement. A plain string is interpolated with
    #' [glue::glue_sql()] in the caller's environment, so `{value}` is quoted as a
    #' value, ``{`name`}`` as an identifier, and a [DBI::SQL()] object -- such as
    #' `table_ref()` returns -- inserted as it is. A statement already built with
    #' [glue::glue()] or [glue::glue_sql()] is passed through untouched.
    #' @param ... Values for the interpolation, as with [glue::glue_sql()].
    #' @param .open,.close Interpolation delimiters, for a statement whose own
    #' syntax needs the braces.
    #' @return Number of rows affected by statement
    execute = function(statement, ..., .open = "{", .close = "}") {
      statement <- private$interpolate(
        statement, ..., .open = .open, .close = .close, .envir = parent.frame()
      )
      private$with_retry(function() {
        DBI::dbExecute(self$connection, statement)
      })
    },

    #' @description
    #' Execute a SQL query and return results. Reading contends for the catalog lock
    #' too; queries are retried on the same terms as writes.
    #' @param statement A SQL query statement, interpolated as in `$execute()`.
    #' @param as_atomic Logical. If true, return the single column the query selects
    #' as a plain vector rather than a one-column data.table.
    #' @param ... Values for the interpolation, as with [glue::glue_sql()].
    #' @param .open,.close Interpolation delimiters, as in `$execute()`.
    #' @return A data.table with query results, or a vector if `as_atomic`
    get_query = function(statement, ..., as_atomic = FALSE, .open = "{", .close = "}") {
      statement <- private$interpolate(
        statement, ..., .open = .open, .close = .close, .envir = parent.frame()
      )
      result <- private$with_retry(function() {
        DBI::dbGetQuery(self$connection, statement)
      })

      if (as_atomic) {
        stopifnot(
          "`as_atomic` needs a query selecting exactly one column" = length(result) == 1L
        )
        return(result[[1L]])
      }

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
    #' Get row count for a table; returns 0 if table does not exist
    #' @param table_name Character string. Name of the table to query.
    #' @return Integer number of rows
    row_count = function(table_name) {
      if (!table_name %in% self$list_tables()) {
        return(0L)
      }

      self$get_query(
        "select count(*) from {table_ref(table_name)}",
        as_atomic = TRUE
      )
    },

    #' @description
    #' List all tables in storage
    #' @param include_internal Logical. If true, also list auxiliary tables this class
    #' constructs.
    #' @return Character vector of table names
    list_tables = function(include_internal = FALSE) {
      tables <- self$get_query(
        "select table_name from information_schema.tables
         where table_catalog = 'dl_db'
         order by table_name",
        as_atomic = TRUE
      )

      if (include_internal) tables else setdiff(tables, INTERNAL_TABLES)
    },

    #' @description
    #' Fetch data from a table
    #' @param table_name Character string. Name of the table to query.
    #' @param cols SQL column selection string (e.g., "col1, col2" or "*")
    #' @param where Character string. Optional WHERE clause for the SQL query (e.g.
    #' `id_pred in (0,4,5)`)
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
        stop("Table `", table_name, "` does not exist in `", self$catalog, "`")
      }

      metadata <- self$get_table_metadata(table_name)
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
    #' Get table metadata, stored as a comment on the catalog table. A table with no
    #' metadata, and one that does not exist, both have none, so both give an empty
    #' list -- the callers that need a missing table to be an error, `$fetch()` and
    #' `$commit()`, say so themselves and more usefully.
    #' @param table_name Character string. Name of the table to query.
    #' @return Named list
    get_table_metadata = function(table_name) {
      comment <- self$get_query(
        "select comment from duckdb_tables()
         where database_name = 'dl_db' and table_name = {table_name}",
        as_atomic = TRUE
      )

      deserialize_metadata(comment)
    },

    #' @description
    #' Delete rows from a table
    #' @param table_name Character string. Name of the table to delete from.
    #' @param where Character string. Optional WHERE clause; if NULL, deletes all rows.
    #' @return Number of rows deleted
    delete_from = function(table_name, where = NULL) {
      stopifnot("database is attached read-only" = !self$read_only)

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
    #' @param x If data.table, the data to commit; use [as_ducklake_db_t] for setting
    #' constraints. If character, treated as an in-DuckDB-memory table or view name.
    #' @param table_name Target table name to commit to.
    #' @param method Character, one of "overwrite", "append", "upsert" (upsert being an
    #' update for existing rows, and insert for new rows). Only "overwrite" can change a
    #' table's schema; even if a table is empty, the others reject columns the table
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

      stored <- if (table_exists) self$get_table_metadata(table_name) else list()
      specs <- private$col_specs(x, stored)
      metadata <- private$resolve_metadata(x, stored)

      on.exit(private$cleanup_new_data_v(), add = TRUE)
      all_new_cols <- private$register_new_data_v(x, specs[["map_cols"]])

      if (table_exists && method != "overwrite") {
        private$check_target_columns(table_name, all_new_cols) # friendly errors
      }

      # `as_<table>_t()` rejects duplicates in the key and alternate key columns
      # the object itself declares, so re-checking those in SQL scans the whole
      # source for an answer already known. Only what the object declared is
      # covered: col_specs() falls back to the target table's stored spec, and
      # nothing validated the data against that.
      validated_cols <- if (inherits(x, "ducklake_db_t")) {
        Filter(length, list(attr(x, "key_cols"), attr(x, "alternate_key_cols")))
      } else {
        list()
      }

      rows <- if (method == "overwrite" || !table_exists) {
        private$commit_overwrite(table_name, specs, replace = method == "overwrite")
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
        private$commit_upsert(table_name, all_new_cols, specs, validated_cols)
      }

      # `create or replace` drops the comment, so overwrite always passes an empty list
      # to rewrite it
      private$write_metadata(
        table_name,
        metadata,
        current = if (method == "overwrite" || !table_exists) list() else stored
      )

      rows
    },

    #' @description
    #' Evaluate `expr` as a single DuckLake transaction, so that a block of reads/writes
    #' either all land or none do. On failure part-way through, DB state is rolled back
    #' and no harm is done. Contention is retried by re-evaluating the whole block, so
    #' `expr` is safe to run more than once -- it re-reads whatever it derived its
    #' writes from.
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

        # the commit is the statement that actually contends for the catalog; failure
        # sends the whole block round again
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
    #' and DuckLake deletes neither on its own, so a long run grows without
    #' bound until this is called.
    #'
    #' How much is discarded is governed by the `expire_older_than` and
    #' `delete_older_than` options, set on the database (see `$new()`). Left
    #' unset they keep everything, so checkpointing an unconfigured database
    #' compacts but deletes nothing. Expiring a snapshot gives up time travel
    #' to it, and anything still reading at one loses the files under it, so
    #' set a retention that covers the readers you expect.
    #' @return Snapshot counts before and after, invisibly
    checkpoint = function() {
      stopifnot("database is attached read-only" = !self$read_only)

      count_snapshots <- function() {
        self$get_query(
          "select count(*) from ducklake_snapshots(dl_db)",
          as_atomic = TRUE
        )
      }

      snapshots_before <- count_snapshots()
      self$execute(glue::glue("checkpoint dl_db"))

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

    #' @description
    #' Use inside `$transaction()`: Safely hand out the next value of an integer id,
    #' such that two processes allocating at once cannot both get the same one. Is not a
    #' DuckLake native feature; employs an auxiliary table for row-level locks to ensure
    #' uniqueness.
    #' @param table_name Character string, the table the id belongs to.
    #' @param column_name Character string, the id column.
    #' @param n Integer, how many consecutive ids to reserve.
    #' @return Integer vector of length `n`, the allocated ids. Side effect: increment
    #' counter in DB.
    next_id = function(table_name, column_name, n = 1L) {
      n <- as.integer(n)
      stopifnot(
        "database is attached read-only" = !self$read_only,
        "`n` must be a positive count" = length(n) == 1L && !is.na(n) && n >= 1L,
        # outside a transaction the update is committed on its own, so a
        # caller whose write then fails has taken an id nothing uses, and two
        # callers racing are not made to retry -- the failure this exists to
        # prevent
        "`$next_id()` only allocates safely inside `$transaction()`" = private$in_transaction
      )

      id_name <- paste(table_name, column_name, sep = ".")
      alloc_ref <- table_ref(ID_ALLOC_TABLE)

      # Seeding from the table itself in case database already has ids in it.
      seed <- function() {
        if (!table_name %in% self$list_tables()) {
          return(1L)
        }

        # coalesce, because max() over a table that exists but is empty is NULL,
        # and allocating from NA fails well away from here
        self$get_query(
          "select coalesce(max({`column_name`}), 0) from {table_ref(table_name)}",
          as_atomic = TRUE
        ) +
          1L
      }

      self$execute(
        "create table if not exists {alloc_ref} (id_name varchar, next_id integer)"
      )

      allocated <- self$get_query(
        "select next_id from {alloc_ref} where id_name = {id_name}",
        as_atomic = TRUE
      )

      # More than one row for a key means two processes seeded it at once:
      # inserts of different rows do not conflict, so nothing stopped them.
      # Collapsing them is a write to both rows, which does conflict, so the
      # collapse itself is safe -- and taking the maximum cannot hand back an
      # id already in use.
      if (length(allocated) > 1L) {
        allocated <- max(allocated, seed())
        self$execute("delete from {alloc_ref} where id_name = {id_name}")
        self$execute("insert into {alloc_ref} values ({id_name}, {allocated})")
      } else if (length(allocated) == 0L) {
        allocated <- seed()
        self$execute("insert into {alloc_ref} values ({id_name}, {allocated})")
      }

      allocated <- as.integer(allocated)

      self$execute(
        "update {alloc_ref} set next_id = {allocated + n} where id_name = {id_name}"
      )

      seq.int(allocated, length.out = n)
    },

    #' @description Get SQL expression to read a table, potentially with additional
    #' filtering applied. [evoland_db] uses it to inject its own state based filter.
    #' @param table_name Character string table name
    #' @return Character string SQL expression
    get_read_expr = function(table_name) {
      table_ref(table_name)
    }
  ),

  ## Private Methods ----
  private = list(
    # R6 hook called on gc(). Deliberately only closes the connection, no maintenance;
    # that is for checkpoint()
    finalize = function() {
      if (!is.null(self$connection)) {
        # in case the connection wasn't started yet
        DBI::dbDisconnect(self$connection)
        self$connection <- NULL
      }
    },

    # DuckLake does not automatically pull in the extensions; derive from catalog and
    # data_path
    backend_extensions = function() {
      scheme <- sub(":.*$", "", self$catalog)
      catalog_ext <- switch(
        scheme,
        sqlite = "sqlite",
        postgres = , # fallthrough
        postgresql = "postgres",
        # duckdb-file catalogs and anything unrecognised need nothing extra;
        # a caller wanting one of those can name it in `extensions`
        NULL
      )

      remote_data <- grepl(REMOTE_URI_PREFIX, self$data_path)

      c(catalog_ext, if (remote_data) "httpfs")
    },

    # whether a transaction() is open on this connection
    in_transaction = FALSE,

    # Interpolate a SQL statement in its caller's environment, with values
    # quoted by glue_sql() rather than pasted in.
    #
    # Only a plain string is interpolated. glue() and glue_sql() return classed
    # objects, so a statement a caller has already interpolated is passed
    # through untouched -- which both keeps the many pre-glued call sites
    # working and stops a second pass tripping over braces that arrived in the
    # data, in a serialised comment or a predictor name.
    interpolate = function(statement, ..., .open, .close, .envir) {
      if (inherits(statement, c("glue", "SQL"))) {
        stopifnot(
          "cannot interpolate into an already interpolated statement" = ...length() == 0L
        )
        return(statement)
      }

      glue::glue_sql(
        statement,
        ...,
        .con = self$connection,
        .open = .open,
        .close = .close,
        .envir = .envir
      )
    },

    # Retry a catalog operation in the face of lock contention. DuckLake's own
    # `ducklake_max_retry_count` covers snapshot conflicts, but not contention on the
    # catalog itself; uncoordinated writers need this wrapper to all get through. Only
    # transient errors are retried, with exponential backoff and jitter; anything else
    # is re-raised immediately.
    with_retry = function(fn) {
      if (private$in_transaction) {
        # in a transaction, the statement must not be retried on its own
        return(fn())
      }

      started <- Sys.time()
      elapsed <- function() as.numeric(difftime(Sys.time(), started, units = "secs"))

      for (attempt in seq_len(self$retry_max)) {
        result <- try(fn(), silent = TRUE)

        if (!inherits(result, "try-error")) {
          return(result)
        }

        condition <- attr(result, "condition")

        # Not contention: re-raise untouched, so the original class and call
        # survive for whoever was expecting them.
        if (!grepl(TRANSIENT_CATALOG_ERRORS, conditionMessage(condition))) {
          stop(condition)
        }

        # Contention we could not outlast.
        if (attempt == self$retry_max || elapsed() > self$retry_timeout) {
          stop(
            glue::glue(
              "Gave up after {attempt} attempts over {round(elapsed(), 1)}s ",
              "waiting for the DuckLake catalog. Either the writers sharing it ",
              "hold it for longer together than `$retry_timeout`, or something ",
              "outside this session is -- another R session with the database ",
              "open, or a stale lock.\n",
              "  {conditionMessage(condition)}"
            ),
            call. = FALSE
          )
        }

        # The doubling stops at 2^6 = 64x the base wait
        Sys.sleep(stats::runif(1L, 0, self$retry_wait * 2^min(attempt - 1L, 6L)))
      }
    },

    ### Commit Methods ----

    # commit_append is now a one-line clause in commit()

    # commit_overwrite also used if table is missing
    commit_overwrite = function(table_name, specs, replace = TRUE) {
      target <- table_ref(table_name)
      create_expr <- if (replace) {
        # replace only if explicit overwrite is required
        glue::glue("create or replace table {target}")
      } else {
        # if two (or more) writers find the table missing at the same time, they race
        # each other - with the "create or replace" clause, the last one to arrive drops
        # what the others wrote.
        # "create table if not exists" makes the loser of the race a no-op.
        glue::glue("create table if not exists {target}")
      }

      # concurrent readers observe the state _before_ the overwrite is committed
      self$transaction({
        # create empty table so partitioning can be applied from the start
        self$execute(glue::glue(
          "{create_expr} as from new_data_v limit 0"
        ))
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

    commit_upsert = function(table_name, all_new_cols, specs, validated_cols = list()) {
      key_cols <- specs[["key_cols"]]
      alternate_key_cols <- specs[["alternate_key_cols"]]

      private$check_source_uniqueness(
        table_name, key_cols, alternate_key_cols, validated_cols
      )

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

    # Friendly warning if an additional column has been provided
    check_target_columns = function(table_name, all_new_cols) {
      target_cols <- self$get_query(
        "select column_name from (describe {table_ref(table_name)})",
        as_atomic = TRUE
      )

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
    # inserts duplicates when the source itself has duplicate keys. `validated_cols`
    # names the column sets a constructor has already checked, which need no scan.
    check_source_uniqueness = function(
      table_name,
      key_cols,
      alternate_key_cols,
      validated_cols = list()
    ) {
      already_validated <- function(cols) {
        any(vapply(validated_cols, identical, logical(1), cols))
      }

      for (cols in list(key_cols, alternate_key_cols)) {
        if (length(cols) == 0L || already_validated(cols)) {
          next
        }
        select_expr <- cols_to_select_expr(cols)
        duplicates <- self$get_query(
          "select count(*) - count(distinct ({select_expr})) from new_data_v",
          as_atomic = TRUE
        )
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
        return(self$get_query("select column_name from (describe {`x`})", as_atomic = TRUE))
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

      # CREATE TABLE AS materialises, so the registration is dead from here and
      # holding it would pin the R copy for the rest of the commit.
      # cleanup_new_data_v() still unregisters, for the paths that error first.
      duckdb::duckdb_unregister(self$connection, "new_data_raw")

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

    # Which columns are keys, maps or partitions comes from the attributes of the data
    # being committed; otherwise use what the table already carries
    col_specs = function(x, stored) {
      lapply(
        stats::setNames(
          nm = c("key_cols", "alternate_key_cols", "map_cols", "partition_cols")
        ),
        function(spec) attr(x, spec) %||% stored[[spec]] %||% character(0)
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
