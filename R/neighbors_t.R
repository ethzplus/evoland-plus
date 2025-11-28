#' Create Neighbors Table
#'
#' Creates a `neighbors_t` table and validates that it matches the schema.
#'
#' @name neighbors_t
#'
#' @param x An object that can be passed to [data.table::setDT()]
#'
#' @return A data.table of class "neighbors_t" with columns:
#'   - `id_coord_origin`: Foreign key to coords_t (origin coordinate)
#'   - `id_coord_neighbor`: Foreign key to coords_t (neighbor coordinate)
#'   - `distance`: Numeric distance between coordinates
#'   - `distance_class`: Optional factor representing distance intervals
#' @export
as_neighbors_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_coord_origin = integer(0),
      id_coord_neighbor = integer(0),
      distance = numeric(0)
    )
  }
  cast_dt_col(x, "id_coord_origin", as.integer)
  cast_dt_col(x, "id_coord_neighbor", as.integer)
  if ("distance_class" %in% names(x)) {
    cast_dt_col(x, "distance_class", as.factor)
  }
  new_evoland_table(
    x,
    "neighbors_t",
    c("id_coord_origin", "id_coord_neighbor")
  )
}

#' @describeIn neighbors_t Validate a neighbors_t object
#' @export
validate.neighbors_t <- function(x, ...) {
  NextMethod()

  required_cols <- c("id_coord_origin", "id_coord_neighbor", "distance")

  data.table::setcolorder(x, required_cols)

  stopifnot(
    is.integer(x[["id_coord_origin"]]),
    is.integer(x[["id_coord_neighbor"]]),
    is.numeric(x[["distance"]])
  )

  return(x)
}

#' @describeIn neighbors_t Print a neighbors_t object
#' @param nrow Maximum number of rows to print. See [data.table::print.data.table]
#' @param ... Passed to [data.table::print.data.table]
#' @export
print.neighbors_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_origins <- data.table::uniqueN(x[["id_coord_origin"]])
    n_neighbors <- data.table::uniqueN(x[["id_coord_neighbor"]])
    total_pairs <- nrow(x)

    extra_info <- ""
    if ("distance_class" %in% names(x)) {
      n_classes <- data.table::uniqueN(x[["distance_class"]])
      extra_info <- glue::glue(", Distance classes: {n_classes}")
    }

    cat(glue::glue(
      "Neighbors Table\n",
      "Neighbor pairs: {total_pairs}\n",
      "Origin coordinates: {n_origins}, ",
      "Neighbor coordinates: {n_neighbors}",
      "{extra_info}\n\n"
    ))
  } else {
    cat("Neighbors Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
