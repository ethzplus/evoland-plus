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
new_evoland_table <- function(x) {
  stopifnot(inherits(x, "data.table"))
  class(x) <- unique(c(
    "evoland_t",
    class(x)
  ))
}
