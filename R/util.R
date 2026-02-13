#' evoland utility functions
#'
#' These are mostly used to validate evoland S3 objects.
#'
#' @name util
NULL

#' @describeIn util Provides an S3 validation generic
#'
#' @param x Object to validate.
#' @param ... Optional.
#' @export
validate <- function(x, ...) {
  UseMethod("validate")
}

#' @export
validate.default <- function(x, ...) {
  stop("No validate method defined for class ", class(x))
}

# TODO move this to a new parquet_db_utils.R file and add more parquet_db
# specific validation functions, e.g. for checking that attributes can be stored.
#' @export
validate.parquet_db_t <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  invisible(x)
}

#' @describeIn util Coerce to parquet_db_t subclass. It coerces an object to a
#' data.table and adds attributes that [parquet_db] relies on for database-like
#' operations. See the paramaters for details.
#' @param class_name Optional class name to prepend before "parquet_db_t". Used
#'        to create more specific subclasses that still work with evoland's
#'        parquet_db methods.
#' @param key_cols Optional character vector. Used for upsert operations to identify
#'        rows. May be missing if identities are not yet known, but must be
#'        present for upsert operations.
#' @param autoincrement_cols Optional character vector. Used to create unique
#'        identifiers, i.e. automatically incrementing integers.
#' @param map_cols Optional character vector. Used to coerce columns to DuckDB's
#'        MAP type, used to store named unnested lists from R.
#' @param partition_cols Optional character vector. Used to specify columns for
#'        hive style parquet file partitioning.
as_parquet_db_t <- function(
  x,
  class_name = character(),
  key_cols = character(),
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

  validate(x)
}

#' @describeIn util Null coalescing operator
#' @param x Left-hand side value
#' @param y Right-hand side value (fallback)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @describeIn util Pluck with wildcard support
#' @param lst The list to index into
#' @param ... Index arguments. Use NA to match all elements at that level
#' @return The indexed result, which may be a single element or a list of elements

pluck_wildcard <- function(lst, ...) {
  indices <- list(...)
  n_indices <- length(indices)

  if (n_indices == 0L) {
    return(lst)
  }

  # Recursive function to process indices
  process_level <- function(current_data, level) {
    if (level > n_indices) {
      return(current_data)
    }

    current_index <- indices[[level]]

    # If current index is na, then apply to all children
    if (is.na(current_index)) {
      # Apply remaining indices to all elements at this level
      if (is.list(current_data)) {
        result <- lapply(current_data, process_level, level = level + 1L)
        return(result)
      } else {
        # If current_data is not a list, we can't descend further
        return(current_data)
      }
    } else {
      # Normal indexing
      if (is.list(current_data) && !is.null(current_data[[current_index]])) {
        return(process_level(current_data[[current_index]], level + 1L))
      } else {
        return(NULL)
      }
    }
  }

  return(process_level(lst, level = 1L))
}

#' @describeIn util Ensure that a directory exists; return its argument for pipeability
ensure_dir <- function(dir) {
  if (dir.exists(dir)) {
    return(invisible(dir))
  }
  if (file.exists(dir)) {
    # dir.exists only returns true if there is a directory
    stop("There is already a file")
  }

  withCallingHandlers(
    dir.create(dir, recursive = TRUE),
    warning = function(w) {
      stop(w$message, call. = FALSE)
    }
  )

  invisible(dir)
}

#' @describeIn util Print a dataframe in a row-wise yaml style
#' @export
print_rowwise_yaml <- function(df) {
  for (i in seq_len(nrow(df))) {
    cat(sprintf("- row %d:\n", i))
    for (col in names(df)) {
      value <- df[[col]][i]
      if (is.raw(value[[1]]) || is.raw(value)) {
        value <- "<raw vector>"
      }
      if (is.character(value) && grepl("\n", value)) {
        value <- paste0(sub("\n.*", "", value), " [...truncated multiline string]")
      }
      cat(sprintf("  %s: %s\n", col, value))
    }
    cat("\n")
  }
}

#' @describeIn util Cast a data.table column; invisibly returns x
#' @param colname Name of the column
#' @param type one of "int", "float", "bool", "factor"
cast_dt_col <- function(x, colname, type) {
  predicate_fn <- switch(
    type,
    float = is.double,
    int = is.integer,
    bool = is.logical,
    factor = is.factor,
    date = \(x) is(x, "Date")
  )
  if (predicate_fn(x[[colname]])) {
    return(invisible(x))
  }

  coercion_fn <- switch(
    type,
    float = as.double,
    int = as.integer,
    bool = as.logical,
    factor = as.factor,
    date = as.Date
  )
  data.table::set(
    x = x,
    j = colname,
    value = coercion_fn(x[[colname]])
  )
  invisible(x)
}
