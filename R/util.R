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

#' @describeIn util Add evoland_t class
#' @param class_name The class name to attach before "evoland_t"
new_evoland_table <- function(x, class_name) {
  stopifnot(inherits(x, "data.table"))
  class(x) <- unique(c(
    class_name,
    "evoland_t",
    class(x)
  ))
  validate(x)
}

#' @describeIn util Check that all required names are present
#' @param x A named object
#' @param required_names Vector of required names
check_missing_names <- function(x, required_names) {
  missing_names <- setdiff(required_names, names(x))
  if (length(missing_names) > 0) {
    stop(glue::glue(
      "missing required names: {paste(missing_names, collapse = ', ')}"
    ))
  }
}
