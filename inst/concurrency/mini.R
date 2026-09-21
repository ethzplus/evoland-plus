# A faithful-enough miniature of evoland-plus R/ducklake_db.R: same retry
# wrapper, same transaction() semantics, same commit paths. Instrumented so
# every catalog attempt lands in a JSONL log we can replay afterwards.

CATALOG_ALIAS <- "dl"

TRANSIENT_CATALOG_ERRORS <- paste(
  "database is locked",
  "Could not set lock on file",
  "Failed to commit DuckLake transaction",
  # postgres MVCC surfaces contention as a serialisation failure
  "could not serialize access",
  "deadlock detected",
  sep = "|"
)

LOG_PATH <- Sys.getenv("LAKELAB_LOG", "")
WORKER <- Sys.getenv("LAKELAB_WORKER", "w?")
T0 <- as.numeric(Sys.time())

jlog <- function(event, ...) {
  if (!nzchar(LOG_PATH)) return(invisible(NULL))
  fields <- list(...)
  parts <- c(
    sprintf('"t":%.4f', as.numeric(Sys.time()) - T0),
    sprintf('"wall":%.4f', as.numeric(Sys.time())),
    sprintf('"worker":"%s"', WORKER),
    sprintf('"event":"%s"', event)
  )
  for (nm in names(fields)) {
    v <- fields[[nm]]
    parts <- c(parts, if (is.numeric(v)) {
      sprintf('"%s":%.4f', nm, v)
    } else {
      sprintf('"%s":"%s"', nm, gsub('[\r\n"\\\\]', " ", as.character(v)))
    })
  }
  cat(paste0("{", paste(parts, collapse = ","), "}\n"),
      file = LOG_PATH, append = TRUE)
  invisible(NULL)
}

mini_db <- R6::R6Class(
  "mini_db",
  public = list(
    connection = NULL,
    catalog = NULL,
    data_path = NULL,
    retry_max = 20L,
    retry_wait = 0.1,
    backoff = "current", # current | decorrelated | flat | capped
    retry_timeout = 1e9,
    threads = NULL,

    attach_opts = NULL,

    initialize = function(catalog, data_path, retry_max = 20L,
                          retry_wait = 0.1, backoff = "current",
                          threads = NULL, attach_opts = NULL,
                          retry_timeout = 1e9) {
      self$attach_opts <- attach_opts
      self$retry_timeout <- retry_timeout
      self$catalog <- catalog
      self$data_path <- data_path
      self$retry_max <- retry_max
      self$retry_wait <- retry_wait
      self$backoff <- backoff
      self$threads <- threads

      self$connection <- DBI::dbConnect(
        duckdb::duckdb(shared_home = TRUE), dbdir = ":memory:"
      )
      ext <- c("ducklake", if (grepl("^sqlite:", catalog)) "sqlite",
               if (grepl("^postgres", catalog)) "postgres")
      for (e in unique(ext)) self$execute(sprintf("install %s; load %s;", e, e))
      if (!is.null(threads)) self$execute(sprintf("set threads=%d", threads))
      il <- Sys.getenv("LAKELAB_INLINE_LIMIT", "")
      if (nzchar(il)) {
        self$execute(sprintf("set ducklake_default_data_inlining_row_limit=%s", il))
      }

      jlog("attach_begin")
      opts <- paste(c(sprintf("DATA_PATH '%s'", self$data_path),
                      if (!is.null(attach_opts) && nzchar(attach_opts)) attach_opts),
                    collapse = ", ")
      self$execute(sprintf("attach 'ducklake:%s' as %s (%s)",
                           self$catalog, CATALOG_ALIAS, opts))
      jlog("attach_end")
      invisible(self)
    },

    execute = function(statement, label = NULL) {
      private$with_retry(function() DBI::dbExecute(self$connection, statement),
                         label = label %||% substr(statement, 1, 60))
    },

    get_query = function(statement, label = NULL) {
      r <- private$with_retry(function() DBI::dbGetQuery(self$connection, statement),
                              label = label %||% substr(statement, 1, 60))
      data.table::setDT(r)
      r
    },

    list_tables = function() {
      self$get_query(sprintf(
        "select table_name from information_schema.tables
         where table_catalog = '%s' order by table_name", CATALOG_ALIAS
      ), label = "list_tables")[[1]]
    },

    column_max = function(table_name, column_name) {
      if (!table_name %in% self$list_tables()) return(0L)
      self$get_query(sprintf('select max("%s") from %s.%s',
                             column_name, CATALOG_ALIAS, table_name),
                     label = "column_max")[[1]]
    },

    transaction = function(expr) {
      code <- substitute(expr)
      envir <- parent.frame()
      if (private$in_transaction) return(eval(code, envir))

      private$with_retry(function() {
        self$execute("begin transaction", label = "begin")
        private$in_transaction <- TRUE
        on.exit({
          try(self$execute("rollback"), silent = TRUE)
          private$in_transaction <- FALSE
        })
        result <- eval(code, envir)
        jlog("txn_commit_begin")
        self$execute("commit", label = "commit")
        jlog("txn_commit_end")
        on.exit(private$in_transaction <- FALSE)
        result
      }, label = "transaction")
    },

    # upsert `x` (a data.table) into table_name on key_cols, creating the
    # table if absent. Mirrors ducklake_db$commit(method = "upsert").
    upsert = function(x, table_name, key_cols) {
      duckdb::duckdb_register(self$connection, "new_data_v", x)
      on.exit(try(duckdb::duckdb_unregister(self$connection, "new_data_v"),
                  silent = TRUE), add = TRUE)
      target <- sprintf("%s.%s", CATALOG_ALIAS, table_name)

      if (!table_name %in% self$list_tables()) {
        return(self$transaction({
          # `create or replace` is what ducklake_db does here, and it is a race:
          # a concurrent first writer's replace drops the table this one just
          # committed, rows and all, with no conflict reported. `if not exists`
          # makes the loser's create a no-op so its insert lands in the winner's
          # table.
          head <- if (identical(Sys.getenv("LAKELAB_CREATE_MODE", "replace"), "if_not_exists")) {
            sprintf("create table if not exists %s", target)
          } else {
            sprintf("create or replace table %s", target)
          }
          self$execute(sprintf("%s as from new_data_v limit 0", head), label = "create")
          self$execute(sprintf("insert into %s by name (from new_data_v)", target),
                       label = "insert_initial")
        }))
      }

      # source-uniqueness check, as the real class does
      key_expr <- paste(sprintf('"%s"', key_cols), collapse = ", ")
      dups <- self$get_query(sprintf(
        "select count(*) - count(distinct (%s)) from new_data_v", key_expr
      ), label = "check_uniqueness")[[1]]
      if (dups > 0) stop("duplicate keys in source: ", dups)

      ordinary <- setdiff(names(x), key_cols)
      set_expr <- paste(sprintf('"%s" = new_data_v."%s"', ordinary, ordinary),
                        collapse = ", ")
      jlog("merge_begin", table = table_name, rows = nrow(x))
      r <- self$execute(sprintf(
        "merge into %s using new_data_v using (%s)
         when matched then update set %s
         when not matched then insert by name",
        target, key_expr, set_expr
      ), label = paste0("merge_", table_name))
      jlog("merge_end", table = table_name, rows = nrow(x))
      r
    },

    append = function(x, table_name) {
      duckdb::duckdb_register(self$connection, "new_data_v", x)
      on.exit(try(duckdb::duckdb_unregister(self$connection, "new_data_v"),
                  silent = TRUE), add = TRUE)
      target <- sprintf("%s.%s", CATALOG_ALIAS, table_name)
      if (!table_name %in% self$list_tables()) {
        return(self$transaction({
          head <- if (identical(Sys.getenv("LAKELAB_CREATE_MODE", "replace"), "if_not_exists")) {
            sprintf("create table if not exists %s", target)
          } else {
            sprintf("create or replace table %s", target)
          }
          self$execute(sprintf("%s as from new_data_v limit 0", head), label = "create")
          self$execute(sprintf("insert into %s by name (from new_data_v)", target),
                       label = "insert_initial")
        }))
      }
      jlog("append_begin", table = table_name, rows = nrow(x))
      r <- self$execute(sprintf("insert into %s by name (from new_data_v)", target),
                        label = paste0("append_", table_name))
      jlog("append_end", table = table_name, rows = nrow(x))
      r
    },

    close = function() {
      if (!is.null(self$connection)) {
        try(DBI::dbDisconnect(self$connection), silent = TRUE)
        self$connection <- NULL
      }
    }
  ),

  private = list(
    in_transaction = FALSE,

    with_retry = function(fn, label = "?") {
      if (private$in_transaction) return(fn())

      started <- Sys.time()
      elapsed <- function() as.numeric(difftime(Sys.time(), started, units = "secs"))
      for (attempt in seq_len(self$retry_max)) {
        a0 <- Sys.time()
        result <- try(fn(), silent = TRUE)
        took <- as.numeric(difftime(Sys.time(), a0, units = "secs"))

        if (!inherits(result, "try-error")) {
          if (attempt > 1L) {
            jlog("retry_success", label = label, attempt = attempt,
                 attempt_s = took,
                 total_s = as.numeric(difftime(Sys.time(), started, units = "secs")))
          }
          return(result)
        }

        condition <- attr(result, "condition")
        msg <- conditionMessage(condition)
        if (!grepl(TRANSIENT_CATALOG_ERRORS, msg)) {
          jlog("hard_error", label = label, attempt = attempt, msg = msg)
          stop(condition)
        }

        jlog("transient", label = label, attempt = attempt, attempt_s = took,
             msg = substr(msg, 1, 160))

        if (attempt == self$retry_max || elapsed() > self$retry_timeout) {
          total <- as.numeric(difftime(Sys.time(), started, units = "secs"))
          jlog("gave_up", label = label, attempt = attempt, total_s = total, msg = substr(msg, 1, 160))
          stop(sprintf("Gave up after %d attempts over %.1fs: %s", attempt, total, msg),
               call. = FALSE)
        }

        wait <- switch(self$backoff,
          current = stats::runif(1L, 0, self$retry_wait * 2^min(attempt - 1L, 6L)),
          # decorrelated jitter: next wait drawn from [base, 3*prev], capped
          decorrelated = {
            prev <- private$last_wait %||% self$retry_wait
            w <- stats::runif(1L, self$retry_wait, min(max(self$retry_wait, prev * 3), self$retry_wait * 64))
            private$last_wait <- w
            w
          },
          flat = stats::runif(1L, 0, self$retry_wait * 8),
          # what the patched ducklake_db does: ramp, then flatten at 8x base,
          # with wall time rather than the attempt count as the real limit
          capped = stats::runif(1L, 0, self$retry_wait * 2^min(attempt - 1L, 3L))
        )
        jlog("backoff", label = label, attempt = attempt, wait_s = wait)
        Sys.sleep(wait)
      }
    },
    last_wait = NULL
  )
)

`%||%` <- function(a, b) if (is.null(a)) b else a
