#' Create LULC Metadata Table from Configuration
#'
#' Creates an lulc_meta_t table based on the LULC classes specification in an
#' evoland_config object. This function uses the lulc_classes field to create
#' metadata entries for each land use/land cover class.
#'
#' @name lulc_meta_t
#'
#' @param config An [evoland_config] instance
#'
#' @return A data.table of class "lulc_meta_t" with columns:
#'   - `id_lulc`: Unique ID for each land use class
#'   - `name`: Name for use in code and queries
#'   - `pretty_name`: Long name for plots/output
#'   - `description`: Long description / operationalisation
#' @export
create_lulc_meta_t <- function(config) {
  lulc_classes <- config[["lulc_classes"]]

  if (!is.list(lulc_classes) || length(lulc_classes) == 0) {
    stop("lulc_classes must be a non-empty list")
  }

  # Extract class names
  class_names <- names(lulc_classes)
  if (is.null(class_names) || any(class_names == "")) {
    stop("All lulc_classes must have names")
  }

  # Create the data.table
  x <- data.table::data.table(
    id_lulc = seq_along(class_names),
    name = class_names,
    pretty_name = vapply(
      lulc_classes,
      function(cls) cls[["pretty_name"]] %||% cls[["name"]] %||% NA_character_,
      character(1)
    ),
    description = vapply(
      lulc_classes,
      function(cls) cls[["description"]] %||% NA_character_,
      character(1)
    )
  )

  data.table::setkey(x, "id_lulc")

  new_evoland_table(x, "lulc_meta_t")
}

#' @export
validate.lulc_meta_t <- function(x, ...) {
  # Check that it's a data.table
  if (!inherits(x, "data.table")) {
    stop("lulc_meta_t must inherit from data.table")
  }

  # Check required columns
  check_missing_names(x, c("id_lulc", "name", "pretty_name", "description"))

  # Check column types
  if (!is.integer(x[["id_lulc"]])) {
    id_ints <- as.integer(x[["id_lulc"]])
    if (anyNA(id_ints)) {
      stop("id_lulc must be int or coercible to it")
    }
    data.table::set(x, j = "id_lulc", value = id_ints)
  }

  if (!is.character(x[["name"]])) {
    stop("name must be character")
  }

  if (!is.character(x[["pretty_name"]])) {
    stop("pretty_name must be character")
  }

  if (!is.character(x[["description"]])) {
    stop("description must be character")
  }

  # if empty, don't run soft checks
  if (nrow(x) == 0L) {
    return(x)
  }

  # Check for unique id_lulc values
  if (anyDuplicated(x[["id_lulc"]])) {
    stop("id_lulc values must be unique")
  }

  # Check for unique names
  if (anyDuplicated(x[["name"]])) {
    stop("name values must be unique")
  }

  # Check for non-empty names
  if (any(x[["name"]] == "")) {
    stop("name values cannot be empty strings")
  }

  return(x)
}

#' @export
#' @describeIn lulc_meta_t Print an lulc_meta_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.lulc_meta_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    cat(glue::glue(
      "LULC Metadata Table\n",
      "Number of classes: {nrow(x)}\n\n"
    ))
  } else {
    cat("LULC Metadata Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
