#' Parquet database utility functions
#'
#' @description
#' A set of utility functions for working with [parquet_db].
#'
#' @name parquet_db_utils
NULL

#' @describeIn parquet_db_utils Coerce to parquet_db_t subclass. It coerces an object to a
#' data.table and adds attributes that [parquet_db] relies on for database-like
#' operations. See the paramaters for details.
#' @param class_name Optional class name to prepend before "parquet_db_t". Used
#'        to create more specific subclasses.
#' @param key_cols Optional character vector. Used for upsert operations to identify
#'        rows. May be missing if identities are not yet known, but must be
#'        present for upsert operations.
#' @param autoincrement_cols Optional character vector. Used to create unique
#'        identifiers, i.e. automatically incrementing integers.
#' @param map_cols Optional character vector. Used to coerce columns to DuckDB's
#'        MAP type, used to store named unnested lists from R.
#' @param partition_cols Optional character vector. Used to specify columns for
#'        hive style parquet file partitioning.
#' @export
as_parquet_db_t <- function(
  x,
  class_name = character(),
  key_cols = NULL,
  autoincrement_cols = NULL,
  map_cols = NULL,
  partition_cols = NULL
) {
  data.table::setDT(x)

  # key cols may be missing if identities are not yet known
  keycols_present <- intersect(key_cols, names(x))
  if (length(keycols_present) > 0) {
    data.table::setkeyv(x, keycols_present)
  }

  distinct_classes <- unique(c(class_name, "parquet_db_t", class(x)))
  data.table::setattr(x, "class", distinct_classes)

  # without effect if NULL
  data.table::setattr(x, "autoincrement_cols", autoincrement_cols)
  data.table::setattr(x, "map_cols", map_cols)
  data.table::setattr(x, "partition_cols", partition_cols)
  data.table::setattr(x, "key_cols", key_cols)

  validate(x)
}

#' @export
validate.parquet_db_t <- function(x, ...) {
  attrs_to_check <- setdiff(
    names(attributes(x)),
    c(
      # data.table internal attrs, not committed to parquet kv metadata
      "names",
      "row.names",
      "class",
      ".internal.selfref",
      "index"
    )
  )

  stopifnot(
    "needs to be a data.table" = inherits(x, "data.table"),
    "attributes need to be atomic" = all(
      vapply(
        attrs_to_check,
        function(y) is.atomic(attr(x, y)),
        logical(1)
      )
    )
  )
  invisible(x)
}


#' @describeIn parquet_db_utils Resolves which columns to use for autoincrement, map,
#' key: pre-existing schema takes precedence.
#' @param x The data to be committed.
#' @param metadata The metadata to be committed, as returned by [parquet_db] `$get_table_metadata()`
#' @param attr The column function to resolve, e.g. "autoincrement_cols", "map_cols", "key_cols".
#' @keywords internal
resolve_cols <- function(x, metadata = list(), attr = character(1)) {
  cols_metadata <- metadata[[attr]] # comes as joint csv string
  cols_attr <- attr(x, attr)

  if (!is.null(cols_metadata)) {
    if (!is.null(cols_attr) && !setequal(cols_metadata, cols_attr)) {
      warning(glue::glue(
        "{attr} on disk ({toString(cols_metadata)}) takes precedence over ",
        "attributes ({toString(cols_attr)}) for this commit"
      ))
    }
    return(cols_metadata)
  } else if (!is.null(cols_attr)) {
    return(cols_attr)
  }

  character(0)
}

#' @describeIn parquet_db_utils Compose partitioning clause.
#' @keywords internal
resolve_partition_clause <- function(x, metadata) {
  cols <- resolve_cols(x, metadata, "partition_cols")

  if (length(cols) == 0) {
    return("")
  }

  paste0(
    ", partition_by (",
    toString(glue::glue('"{cols}"')),
    ")"
  )
}

#' @describeIn parquet_db_utils Compose metadata clause, which includes any new
#' metadata from the data's attributes, as well as any existing metadata from
#' the database; existing metadata cannot be safely overwritten. New metadata
#' takes precedence over existing metadata for this commit. Any atomic vector is
#' coerced to a quoted comma separated list of values, which are recovered in
#' [parquet_db] `$get_table_metadata()`. Non-atomic metadata values are dropped
#' with a warning.
#' @keywords internal
resolve_metadata_clause <- function(x, metadata) {
  new_metadata <- attributes(x)

  names_to_add <- setdiff(
    names(new_metadata),
    c(
      names(metadata),
      # exclude data.table attributes
      "class",
      "names",
      ".internal.selfref",
      "row.names",
      "sorted",
      "index"
    )
  )

  # nothing to do
  if (length(names_to_add) == 0 && length(metadata) == 0L) {
    return("")
  }

  # Filter to atomic only and serialize e.g. c("a", "b") -> "\"a\", \"b\""
  out <- c(metadata, new_metadata[names_to_add])

  for (key in names(out)) {
    val <- out[[key]]
    if (is.atomic(val)) {
      out[[key]] <- paste0('"', val, '"', collapse = ", ")
    } else {
      warning(glue::glue(
        "Metadata key '{key}' has non-atomic value; dropping metadata"
      ))
      out[[key]] <- NULL
    }
  }

  if (inherits(x, "parquet_db_t")) {
    out[["parquet_db_t_class"]] <- class(x)[1L]
  }

  kv_str <- glue::glue_collapse(
    glue::glue("{names(out)}: '{out}'"),
    sep = ",\n  "
  )

  glue::glue(", kv_metadata {\n  <kv_str>\n}", .open = "<", .close = ">")
}

#' @describeIn parquet_db_utils Convert list columns by applying `fn`
#' @param cols The list columns to convert.
#' @param fn The function to apply to each element of the list columns
#' @keywords internal
convert_list_cols <- function(x, cols, fn) {
  for (col in cols) {
    if (!col %in% names(x)) {
      warning(glue::glue("Column '{col}' not found in data; skipping conversion"))
      next
    }
    x[[col]] <- lapply(x[[col]], fn)
  }
  x
}

#' @describeIn parquet_db_utils Convert a named list to a data.frame with "key" and
#' "value" columns.
#' @keywords internal
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

#' @describeIn parquet_db_utils Convert a data.frame with "key" and "value" columns to a
#' named list.
#' @keywords internal
kv_df_to_list <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(NULL)
  }

  out <- list()

  for (row in seq_len(nrow(x))) {
    key <- x$key[row]
    val <- utils::type.convert(x$value[row], as.is = TRUE)
    out[[key]] <- val
  }

  out
}

#' @describeIn parquet_db_utils Create a binding function for a table, which can be used
#' to fetch or commit data to that table. The active binding either returns the table
#' (missing argument), or upserts to it (assignment operation)
#' @param table_name The name of the table to bind to.
create_table_binding <- function(table_name, mode = c("write_once", "upsert", "append")) {
  force(table_name)
  mode <- match.arg(mode)
  # Use bquote to "bake in" the table_name value directly into the function body,
  # so it doesn't depend on the enclosing environment (which R6 will replace).
  fn <- eval(bquote(
    function(x) {
      tbl <- .(table_name)
      md <- .(mode)

      if (missing(x)) {
        fetched <- self$fetch(table_name = tbl)
        as_fn <- paste0("as_", tbl) |> get()
        return(as_fn(fetched))
      }

      if (md == "write_once" && file.exists(self$get_table_path(tbl))) {
        warning(
          glue::glue("Table '{tbl}' is read-only; delete manually!"),
          .call = FALSE
        )
        return(NULL)
      } else if (md == "write_once") {
        md <- "overwrite"
      }
      stopifnot(inherits(x, tbl))
      self$commit(x, table_name = tbl, method = md)
    }
  ))
  fn
}

#' @describeIn parquet_db_utils Helper function to bind a function as a method to an R6
#' generator. Simply passes the R6 method's arguments as-is to the underlying "pure"
#' function, passing the R6 reference to `self`, but not to `private`.
#' @keywords internal
#' @param fun The underlying function to bind as an R6 method, which must have a `self` argument
#' @param with_private Whether to also pass the R6 reference to `private` as an argument
#' parameter as its first argument.
create_method_binding <- function(fun, with_private = FALSE) {
  # Capture the original call (e.g., obj$method(arg1 = 1)); expands ... arguments
  cl <- match.call(definition = sys.function(-1), call = sys.call(-1))

  # Modify 'obj$method' to 'fun' instead
  cl[[1]] <- fun

  # Inject 'self' as named arg. Use get() to retrieve R6 instance's self from calling env
  cl[["self"]] <- get("self", envir = parent.frame())
  if (with_private) {
    cl[["private"]] <- get("private", envir = parent.frame())
  }

  # Evaluate in original environment. Preserves lazy evaluation of arguments.
  eval(cl, envir = parent.frame(2))
}
