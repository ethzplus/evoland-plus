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
#' @param key_cols Optional character vector. Used for data.table's [data.table::setkey]
#'        and used by [parquet_db] to determine which columns to use for upsert
#'        operations. Together with `alternate_key_cols`, these are used to define
#'        uniqueness constraints.
#' @param alternate_key_cols Optional character vector. These are used in
#'        conjunction with `key_cols` to define uniqueness constraints, but are
#'        not used for upsert operations. Example: numeric primary key whose
#'        distinctness is identical to a character primary key column, but
#'        upsert operations should be based on the character key, leaving the
#'        character<->numeric mapping intact.
#' @param map_cols Optional character vector. Used to coerce columns to DuckDB's
#'        MAP type, used to store named unnested lists from R.
#' @param partition_cols Optional character vector. Used to specify columns for
#'        hive style parquet file partitioning.
#' @export
as_parquet_db_t <- function(
  x,
  class_name = character(),
  key_cols = NULL,
  alternate_key_cols = NULL,
  map_cols = NULL,
  partition_cols = NULL
) {
  data.table::setDT(x)

  # subclassses for more specific validation methods
  distinct_classes <- unique(c(class_name, "parquet_db_t", class(x)))
  data.table::setattr(x, "class", distinct_classes)

  # uniqueness, matching, distinctness can be derived from key cols
  data.table::setkeyv(x, key_cols)
  data.table::setattr(x, "key_cols", key_cols)
  data.table::setattr(x, "alternate_key_cols", alternate_key_cols)

  # list cols to coerce to MAP
  data.table::setattr(x, "map_cols", map_cols)

  # partition_cols for hive style partitioning
  data.table::setattr(x, "partition_cols", partition_cols)

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
    "all attributes need to be atomic" = all(
      vapply(
        attrs_to_check,
        function(y) is.atomic(attr(x, y)),
        logical(1)
      )
    ),
    "key_cols, alternate_key_cols, map_cols, and partition_cols must be present" = all(
      c(
        attr(x, "key_cols"),
        attr(x, "alternate_key_cols"),
        attr(x, "map_cols"),
        attr(x, "partition_cols")
      ) %in%
        names(x)
    )
  )

  for (col in names(x)) {
    if (
      col %in%
        c(
          attr(x, "key_cols"),
          attr(x, "alternate_key_cols"),
          attr(x, "partition_cols")
        )
    ) {
      if (anyNA(x[[col]])) {
        stop(glue::glue(
          "Column '{col}' cannot contain NA values"
        ))
      }
    } else if (col %in% attr(x, "map_cols")) {
      for (val in x[[col]]) {
        is_valid_mapcol <- function(v) {
          # allow NULLs
          is.null(v) ||
            # allow empty lists
            (is.list(v) && length(v) == 0) ||
            # allow named lists with atomic values
            (is.list(v) && !is.null(names(v)) && all(vapply(v, is.atomic, logical(1))))
        }

        # negation makes the above condition easier to read
        if (Negate(is_valid_mapcol)(val)) {
          stop(glue::glue(
            "Column '{col}' specified as map_cols must be a list of ",
            "named lists with atomic values"
          ))
        }
      }
    } else if (is.list(x[[col]])) {
      distinct_classes <-
        lapply(x[[col]], \(y) if (is.null(y)) NULL else class(y)) |>
        unique() |>
        Filter(Negate(is.null), x = _) # drop NULLs
      if (length(distinct_classes) > 1) {
        stop(glue::glue(
          "All elements of list column '{col}' must have the same class"
        ))
      }
    }
  }

  if (
    length(attr(x, "key_cols")) &&
      anyDuplicated(x, by = attr(x, "key_cols"))
  ) {
    stop(glue::glue(
      "Duplicates found in key_cols\n",
      "  key_cols: {toString(attr(x, 'key_cols'))}"
    ))
  }
  if (
    length(attr(x, "alternate_key_cols")) &&
      anyDuplicated(x, by = attr(x, "alternate_key_cols"))
  ) {
    stop(glue::glue(
      "Duplicates found in alternate_key_cols\n",
      "  alternate_key_cols: {toString(attr(x, 'alternate_key_cols'))}"
    ))
  }

  x
}


#' @describeIn parquet_db_utils Serialize a named list of atomic vectors into the
#' single string that [parquet_db] stores as a comment on the catalog table. Each
#' entry becomes one line, `key: "value1", "value2"`.
#' @param metadata Named list of atomic vectors.
#' @keywords internal
serialize_metadata <- function(metadata) {
  if (length(metadata) == 0L) {
    return("")
  }

  values <- vapply(
    metadata,
    function(value) paste0('"', value, '"', collapse = ", "),
    character(1)
  )

  paste0(names(metadata), ": ", values, collapse = "\n")
}

#' @describeIn parquet_db_utils Recover the named list written by
#' [serialize_metadata()]. Values are type-converted, so that a numeric
#' attribute survives the round-trip as a number rather than as a string.
#' @param comment Character scalar, or NA for a table without metadata.
#' @keywords internal
deserialize_metadata <- function(comment) {
  if (length(comment) == 0L || is.na(comment) || !nzchar(comment)) {
    return(list())
  }

  entries <- strsplit(comment, "\n", fixed = TRUE)[[1]]
  keys <- sub(": .*$", "", entries)
  values <- sub("^[^:]*: ", "", entries)

  parsed <- lapply(values, function(value) {
    strsplit(value, ", ", fixed = TRUE)[[1]] |>
      gsub('^"|"$', "", x = _) |>
      utils::type.convert(as.is = TRUE)
  })

  stats::setNames(parsed, keys)
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
#' @param mode The mode of the binding, which determines the behavior when
#' committing data. Options are: "write_once" (default, only allows writing if
#' table doesn't exist), "upsert", "append", and "overwrite".
create_table_binding <- function(
  table_name,
  mode = c("write_once", "upsert", "append", "overwrite")
) {
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

      stopifnot(inherits(x, tbl))

      if (md == "overwrite" && interactive() && tbl %in% self$list_tables()) {
        confirm_overwrite <- utils::menu(
          c("Yes", "No"),
          title = glue::glue(
            "Table '{tbl}' already exists. Are you sure you want to overwrite it?"
          )
        )
        if (confirm_overwrite != 1) {
          return(NULL)
        }
      }

      if (md == "write_once") {
        if (tbl %in% self$list_tables()) {
          warning(
            glue::glue(
              "Table '{tbl}' is write-once! Delete manually and write ",
              "anew ONLY if you know what you are doing (-> foreign keys)!"
            ),

            call. = FALSE
          )
          return(NULL)
        }
        md <- "overwrite"
      }

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
#' @param with_super Whether to also pass the R6 reference to `super` as an argument
create_method_binding <- function(fun, with_private = FALSE, with_super = FALSE) {
  # Capture the original call (e.g., obj$method(arg1 = 1)); expands ... arguments
  cl <- match.call(definition = sys.function(-1), call = sys.call(-1))

  # Modify 'obj$method' to 'fun' instead
  cl[[1]] <- fun

  # Inject 'self' as named arg. Use get() to retrieve R6 instance's self from calling env
  cl[["self"]] <- get("self", envir = parent.frame())
  if (with_private) {
    cl[["private"]] <- get("private", envir = parent.frame())
  }
  if (with_super) {
    cl[["super"]] <- get("super", envir = parent.frame())
  }

  # Evaluate in original environment. Preserves lazy evaluation of arguments.
  eval(cl, envir = parent.frame(2))
}

#' @describeIn parquet_db_utils Paste vector of escaped column names into a SQL
#' select statement, with optional table name prefix.
#' @param cols The columns to select.
cols_to_select_expr <- function(cols, table_name) {
  if (!missing(table_name)) {
    prefix <- paste0('"', table_name, '"."')
    suffix <- '"'
  } else {
    prefix <- suffix <- '"'
  }
  paste0(prefix, cols, suffix, collapse = ", ")
}
