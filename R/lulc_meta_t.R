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
as_lulc_meta_t <- function(x) {
  new_evoland_table(
    x,
    "lulc_meta_t",
    "id_lulc"
  )
}

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

  as_lulc_meta_t(x)
}

#' @export
validate.lulc_meta_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_lulc",
      "name",
      "pretty_name",
      "description"
    )
  )

  stopifnot(
    is.integer(x[["id_lulc"]]),
    is.character(x[["name"]]),
    is.character(x[["pretty_name"]]),
    is.character(x[["description"]]),
    !anyDuplicated(x[["id_lulc"]]),
    !anyDuplicated(x[["name"]]),
    !any(x[["name"]] == "")
  )

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
