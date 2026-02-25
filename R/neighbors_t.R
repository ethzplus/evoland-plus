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

  data.table::setDT(x) |>
    cast_dt_col("id_coord_origin", "int") |>
    cast_dt_col("id_coord_neighbor", "int")

  if ("distance_class" %in% names(x)) {
    cast_dt_col(x, "distance_class", "factor")
  }
  as_parquet_db_t(
    x,
    class_name = "neighbors_t",
    key_cols = c("id_coord_origin", "id_coord_neighbor")
  )
}

#' @describeIn neighbors_t Compute neighboring coordinates within specified distances.
#' This uses a spatial hash map for efficiency.
#' @param max_distance Maximum distance to search for neighbors (in same units as
#' coordinates)
#' @param distance_breaks Optional numeric vector defining distance class boundaries.
#'   If NULL, no distance classification is performed.
#'   If provided, must have at least 2 elements defining interval breaks.
#' @return A data.table with columns:
#'   - id_coord_origin: ID of the origin coordinate
#'   - id_coord_neighbor: ID of the neighboring coordinate
#'   - distance: Distance between origin and neighbor
#'   - distance_class: Factor indicating distance class (if distance_breaks provided)
#' @export
create_neighbors_t <- function(
  coords_t,
  max_distance,
  distance_breaks = NULL,
  quiet = FALSE
) {
  # Validate inputs
  if (!inherits(coords_t, "coords_t")) {
    stop("coords_t must be a coords_t object")
  }

  if (!is.numeric(max_distance) || length(max_distance) != 1 || max_distance <= 0) {
    stop("max_distance must be a positive scalar numeric")
  }

  if (!is.null(distance_breaks)) {
    if (!is.numeric(distance_breaks) || length(distance_breaks) < 2) {
      stop("distance_breaks must be NULL or a numeric vector with at least 2 elements")
    }
  }

  # Call C++ function
  dt <- distance_neighbors_cpp(
    coords_t = coords_t,
    max_distance = max_distance,
    quiet = quiet
  )

  data.table::setkeyv(dt, c("id_coord_origin", "id_coord_neighbor"))
  data.table::setalloccol(dt)

  # Add distance class if breaks provided
  if (!is.null(distance_breaks)) {
    dt[,
      distance_class := cut(
        distance,
        breaks = distance_breaks,
        right = FALSE,
        include.lowest = TRUE
      )
    ]
  }

  as_neighbors_t(dt)
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
    total_pairs <- format(
      nrow(x),
      big.mark = "_",
      scientific = FALSE
    )

    extra_info <- ""
    if ("distance_class" %in% names(x)) {
      extra_info <- paste("Distance classes:", toString(levels(x[["distance_class"]])))
    }

    cat(glue::glue(
      "Neighbors Table\n",
      "Neighbor pairs: {total_pairs}\n",
      "{extra_info}\n\n"
    ))
  } else {
    cat("Neighbors Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}

#' @describeIn neighbors_t Compute neighbor relationships between coordinates
#' and store in `self$neighbors_t`. This uses a spatial hash map for efficiency
#' and can produce a very large table depending on max_distance.
#' @param self An evoland_db object
#' @param chunksize Number of rows to write per chunk when inserting into the database
#' to avoid memory issues (default: 1e8)
set_neighbors <- function(
  self,
  max_distance = 1000,
  distance_breaks = c(0, 100, 500, 1000),
  overwrite = FALSE,
  quiet = FALSE,
  chunksize = 1e8
) {
  if (!overwrite && "neighbors_t" %in% self$list_tables()) {
    message("neighbors_t already exists. Use overwrite = TRUE to recompute.")
    return(invisible(self))
  }

  coords_minimal <- self$coords_minimal

  # may produce a very large table
  # cannot chunk here, because we cannot subset efficiently without spatial index
  neighbors <- distance_neighbors_cpp(
    coords_minimal,
    max_distance = max_distance,
    quiet = quiet
  )
  data.table::setkeyv(neighbors, c("id_coord_origin", "id_coord_neighbor"))
  data.table::setalloccol(neighbors)

  # Add distance class if breaks provided
  if (!is.null(distance_breaks)) {
    neighbors[,
      distance_class := cut(
        distance,
        breaks = distance_breaks,
        right = FALSE,
        include.lowest = TRUE
      )
    ]
  }

  # chunked insert to avoid memory issues (each chunk gets copied when registering to DB)
  n_neighbors <- nrow(neighbors)
  chunksize <- min(chunksize, n_neighbors)

  if (n_neighbors > 0) {
    # Use a temporary prefix for the chunked files
    temp_prefix <- "neighbors_t_temp"
    n_chunks <- ceiling(n_neighbors / chunksize)

    for (i in seq_len(n_chunks)) {
      slice_start <- (i - 1) * chunksize + 1
      slice_end <- min(i * chunksize, n_neighbors)

      # Write each chunk to a separate parquet file using overwrite
      self$commit(
        as_neighbors_t(neighbors[slice_start:slice_end, ]),
        table_name = paste0(temp_prefix, "_", i),
        method = "overwrite"
      )
    }

    # Remove the large object from memory and collect garbage
    rm(neighbors)
    gc()

    # Gather all temporary parquet files into the final large file using DuckDB
    self$execute(glue::glue(
      "copy (
        select * from read_parquet('{self$path}/{temp_prefix}_*.parquet')
      ) to '{self$path}/neighbors_t.parquet' ({self$writeopts})"
    ))

    unlink(list.files(self$path, pattern = temp_prefix, full.names = TRUE))
  }

  message(glue::glue("Computed {n_neighbors} neighbor relationships"))
  invisible(self)
}

#' @describeIn neighbors_t Generates neighbor predictors based on neighbor land
#' use counts by distance class, e.g. "10 coordinate points within 200-500m are
#' of class forest". This requires that `neighbors_t`, `lulc_data_t`,
#' `lulc_meta_t`, and `pred_meta_t` are all present in the database. The
#' generated predictors are stored in `pred_data_t` and metadata in
#' `pred_meta_t`.
generate_neighbor_predictors <- function(self) {
  try(neighbors_sample <- self$fetch("neighbors_t", limit = 0L))
  tables_present <- self$list_tables()
  stopifnot(
    "No neighbor data found. Run $set_neighbors() first." = {
      "neighbors_t" %in% tables_present
    },
    "No LULC metadata found. Add lulc_meta_t before generating neighbor predictors." = {
      "lulc_meta_t" %in% tables_present
    },
    "No LULC data found. Add lulc_data_t before generating neighbor predictors." = {
      "lulc_data_t" %in% tables_present
    },
    "No predictor metadata found; while not strictly necessary, the current logic relies on this" = {
      "lulc_data_t" %in% tables_present
    },
    "neighbors_t does not have distance_class column.
    Run $create_neighbors_t() with distance_breaks" = {
      "distance_class" %in% names(neighbors_sample)
    }
  )

  neighbors_read_expr <- self$get_read_expr("neighbors_t")
  lulc_data_read_expr <- self$get_read_expr("lulc_data_t")
  lulc_meta_read_expr <- self$get_read_expr("lulc_meta_t")
  pred_meta_read_expr <- self$get_read_expr("pred_meta_t")

  # Generate metadata rows based on all distinct distance class / id_lulc permutations
  n_predictors <- self$execute(glue::glue(
    r"{
    create or replace temp table pred_meta_neighbors_t as
    with
      all_distance_classes as (select distinct distance_class from {neighbors_read_expr})
    select
      NULL as id_pred,
      concat('id_lulc_', l.id_lulc, '_dist_', c.distance_class) as name,
      concat(
        'Count of ', l.pretty_name,
        ' within distance class ', c.distance_class
        ) as pretty_name,
      'Number of neighbors by land use class and distance interval' as description,
      c.distance_class,
      l.id_lulc
    from
      {lulc_meta_read_expr} l
    cross join
      all_distance_classes c
    }"
  ))
  # Subset to just those columns we can upsert to the existing pred_meta_t
  m <- self$get_query("select id_pred, name, pretty_name, description from pred_meta_neighbors_t")
  m[["orig_format"]] <- "land use coordinate data"
  m[["sources"]] <- list(NULL)
  m[["unit"]] <- "number of neighbors"
  m[["data_type"]] <- "int"
  m[["factor_levels"]] <- list(NULL)
  self$pred_meta_t <- as_pred_meta_t(m)

  # Set the id_pred in pred_meta_neighbors_t based on the autoincremented IDs in pred_meta_t
  self$execute(glue::glue(
    r"{
    update pred_meta_neighbors_t
    set id_pred = m.id_pred
    from {pred_meta_read_expr} m
    where pred_meta_neighbors_t.name = m.name
    }"
  ))

  # Count the number of neighbours per origin, period, id_lulc and distance_class
  n_data_points <- self$execute(glue::glue(
    r"{
    create temp table pred_neighbors_t as
    select
      {self$id_run} as id_run,
      p.id_pred,
      n.id_coord_origin as id_coord,
      t.id_period,
      count(n.id_coord_neighbor) as value
    from
     {neighbors_read_expr} n,
     {lulc_data_read_expr} t,
     pred_meta_neighbors_t p
    where
      n.id_coord_neighbor = t.id_coord
      and p.id_lulc = t.id_lulc
      and p.distance_class = n.distance_class
    group by
      n.id_coord_origin,
      n.distance_class,
      t.id_period,
      t.id_lulc,
      p.id_pred
    }"
  ))

  self$commit("pred_neighbors_t", "pred_data_t", method = "append")

  message(glue::glue(
    "Appended {n_predictors} neighbor predictor variables with ",
    "{n_data_points} data points"
  ))

  invisible(self)
}
