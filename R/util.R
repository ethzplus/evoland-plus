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

#' @export
validate.evoland_t <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  invisible(x)
}

#' @describeIn util Add evoland_t class
#' @param class_name The class name to attach before "evoland_t"
#' @param keycols The columns to be set as key, see [data.table::setkey()]
new_evoland_table <- function(x, class_name, keycols) {
  data.table::setDT(x)

  # key cols may be missing if identities are not yet known
  if (!missing(keycols)) {
    keycols_present <- intersect(keycols, names(x))
    if (length(keycols_present) > 0) data.table::setkeyv(x, keycols_present)
  }

  data.table::setattr(
    x,
    "class",
    unique(c(
      class_name,
      "evoland_t",
      class(x)
    ))
  )
  validate(x)
}

#' @describeIn util Check that all required names are present
#' @param x A named object
#' @param required_names Vector of required names
#' @return NULL, called for side effect
check_missing_names <- function(x, required_names) {
  missing_names <- setdiff(required_names, names(x))
  if (length(missing_names) > 0) {
    stop(glue::glue(
      "missing required names: {paste(missing_names, collapse = ', ')}"
    ))
  }
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
      cat(sprintf("  %s: %s\n", col, value))
    }
    cat("\n")
  }
}

#' @describeIn util Cast a data.table column; invisibly returns x
cast_dt_col <- function(x, colname, type) {
  predicate_fn <- switch(
    type,
    float = is.numeric,
    int = is.integer,
    bool = is.logical,
    factor = is.factor
  )
  if (predicate_fn(x[[colname]])) {
    return(invisible(x))
  }

  coercion_fn <- switch(
    type,
    float = as.numeric,
    int = as.integer,
    bool = as.logical,
    factor = as.factor
  )
  data.table::set(
    x = x,
    j = colname,
    value = coercion_fn(x[[colname]])
  )
  invisible(x)
}
