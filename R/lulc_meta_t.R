#' Create LULC Metadata Table
#'
#' Create a `lulc_meta_t` table or rows thereof that meet the schema requirements. These
#' metadata describe the characteristics of land use classes.
#'
#' @name lulc_meta_t
#'
#' @return A data.table of class "lulc_meta_t" with columns:
#'   - `id_lulc`: Unique ID for each land use class
#'   - `name`: Name for use in code and queries
#'   - `pretty_name`: Long name for plots/output
#'   - `description`: Long description / operationalisation
#' @export
as_lulc_meta_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_lulc = integer(0),
      name = character(0),
      pretty_name = character(0),
      description = character(0),
      src_classes = list()
    )
  }
  new_evoland_table(
    x,
    "lulc_meta_t",
    "id_lulc"
  )
}

#' @describeIn lulc_meta_t Creates a lulc_meta_t table from intervention specifications
#' @param lulc_class_spec A list of intervention specifications, see examples
#' @examples create_lulc_meta_t(list(
#'   forest = list(
#'     pretty_name = "Dense Forest",
#'     description = "Mature, dense forest cover",
#'     src_classes = c(1L, 2L)
#'   ),
#'   arable = list(
#'     pretty_name = "Arable land",
#'     description = "Non-permanent cropland",
#'     src_classes = 3:6
#'   )
#' ))
#' @export
create_lulc_meta_t <- function(lulc_class_spec) {
  if (!is.list(lulc_class_spec) || length(lulc_class_spec) == 0) {
    stop("lulc_classes must be a non-empty list")
  }

  # Extract class names
  class_names <- names(lulc_class_spec)
  if (is.null(class_names) || any(class_names == "")) {
    stop("All lulc_classes must have names")
  }

  # Create the data.table
  x <- data.table::data.table(
    id_lulc = seq_along(class_names),
    name = class_names,
    pretty_name = vapply(
      lulc_class_spec,
      function(cls) cls[["pretty_name"]] %||% cls[["name"]] %||% NA_character_,
      character(1)
    ),
    description = vapply(
      lulc_class_spec,
      function(cls) cls[["description"]] %||% NA_character_,
      character(1)
    ),
    src_classes = pluck_wildcard(lulc_class_spec, NA, "src_classes")
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
      "description",
      "src_classes"
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
