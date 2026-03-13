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
#' @param levels Optional character vector of factor levels (only used when type = "factor")
cast_dt_col <- function(x, colname, type, levels = NULL) {
  # TODO rename predicates to R types
  predicate_fn <- switch(
    type,
    float = is.double,
    int = is.integer,
    bool = is.logical,
    factor = is.factor,
    char = is.character,
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
    char = as.character,
    factor = {
      if (!is.null(levels)) {
        function(col) {
          result <- factor(col, levels = levels)
          if (anyNA(result) && !anyNA(col)) {
            bad_values <- unique(col[is.na(result)])
            stop(glue::glue(
              "Casting to factor produced NAs.",
              "The following values are not in the provided levels: {toString(bad_values)}"
            ))
          }
          result
        }
      } else {
        as.factor
      }
    },
    date = as.Date
  )
  data.table::set(
    x = x,
    j = colname,
    value = coercion_fn(x[[colname]])
  )
  invisible(x)
}
