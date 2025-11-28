#' Neighbor analysis methods for evoland_db
#'
#' @description
#' This file adds neighbor analysis methods to the `evoland_db` class using R6's `$set()` method.
#' These methods compute neighbor relationships and generate neighbor-based predictors.
#'
#' @section Methods Added:
#'
#' - `compute_neighbors(max_distance, distance_breaks, resolution, overwrite)` -
#'   Computes neighbor relationships between coordinates.
#'   - `max_distance`: Maximum distance for neighbors (default: 1000)
#'   - `distance_breaks`: Vector of breaks for distance classes (default: c(0, 100, 500, 1000))
#'   - `resolution`: Grid resolution for distance calculations (default: 100)
#'   - `overwrite`: Whether to overwrite existing neighbors_t (default: FALSE)
#' - `generate_neighbor_predictors()` - Generates predictor variables based on neighbor
#'   land use counts by distance class. Requires neighbors_t with distance_class column
#'   (compute_neighbors with distance_breaks).
#'
#' @name evoland_db_neighbors
#' @include evoland_db.R
NULL

evoland_db$set(
  "public",
  "compute_neighbors",
  function(
    max_distance = 1000,
    distance_breaks = c(0, 100, 500, 1000),
    resolution = 100,
    overwrite = FALSE
  ) {
    if (!overwrite && self$row_count("neighbors_t") > 0) {
      message("neighbors_t already exists. Use overwrite = TRUE to recompute.")
      return(invisible(self))
    }

    coords <- self$coords_t

    neighbors <- compute_neighbors(
      coords,
      max_distance = max_distance,
      distance_breaks = distance_breaks,
      resolution = resolution
    )

    self$commit_overwrite(
      as_neighbors_t(neighbors),
      table_name = "neighbors_t"
    )

    message(glue::glue("Computed {nrow(neighbors)} neighbor relationships"))
    invisible(self)
  }
)

evoland_db$set("public", "generate_neighbor_predictors", function() {
  if (self$row_count("neighbors_t") == 0) {
    stop("No neighbor data found. Run $compute_neighbors() first.")
  }

  if (self$row_count("lulc_meta_t") == 0) {
    stop("No LULC metadata found. Add lulc_meta_t before generating neighbor predictors.")
  }

  if (self$row_count("lulc_data_t") == 0) {
    stop("No LULC data found. Add lulc_data_t before generating neighbor predictors.")
  }

  neighbors_sample <- self$fetch("neighbors_t", limit = 1)
  if (!"distance_class" %in% names(neighbors_sample)) {
    stop(
      "neighbors_t does not have distance_class column. Run $compute_neighbors() with distance_breaks parameter."
    )
  }

  self$attach_table("neighbors_t")
  self$attach_table("lulc_data_t")
  self$attach_table("lulc_meta_t")

  has_pred_meta <- self$row_count("pred_meta_t") > 0
  if (has_pred_meta) {
    self$attach_table("pred_meta_t")
  }

  on.exit({
    self$detach_table("neighbors_t")
    self$detach_table("lulc_data_t")
    self$detach_table("lulc_meta_t")
    if (has_pred_meta) self$detach_table("pred_meta_t")
  })

  max_id_query <- if (has_pred_meta) {
    "(select coalesce(max(id_pred), 0) as max_pred from pred_meta_t)"
  } else {
    "(select 0 as max_pred)"
  }

  self$execute(glue::glue(
    r"{
    create temp table pred_meta_neighbors_t as
    with
      all_distance_classes as (select distinct distance_class from neighbors_t),
      max_id as }",
    max_id_query,
    r"{
    select
      row_number() over () + (select max_pred from max_id) as id_pred,
      concat('id_lulc_', l.id_lulc, '_dist_', c.distance_class) as name,
      concat('Count of ', l.pretty_name, ' within distance class ', c.distance_class) as pretty_name,
      'Number of neighbors by land use class and distance interval' as description,
      'land use coordinate data' as orig_format,
      NULL as sources,
      'number of neighbors' as unit,
      NULL as factor_levels,
      c.distance_class,
      l.id_lulc
    from
      lulc_meta_t l
    cross join
      all_distance_classes c
    }"
  ))

  pred_meta_neighbors <- self$get_query(
    "select id_pred, name, pretty_name, description, orig_format, unit
     from pred_meta_neighbors_t"
  )

  pred_meta_neighbors$sources <- lapply(1:nrow(pred_meta_neighbors), function(i) list())
  pred_meta_neighbors$factor_levels <- lapply(1:nrow(pred_meta_neighbors), function(i) list())

  self$pred_meta_t <- as_pred_meta_t(pred_meta_neighbors)

  self$execute(
    r"{
    create temp table pred_neighbors_t as
    select
      p.id_pred,
      n.id_coord_origin as id_coord,
      t.id_period,
      count(n.id_coord_neighbor) as value
    from
     neighbors_t n,
     lulc_data_t t,
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
  )

  pred_neighbors <- self$get_query("select * from pred_neighbors_t")
  pred_neighbors$id_pred <- as.integer(pred_neighbors$id_pred)
  pred_neighbors$id_coord <- as.integer(pred_neighbors$id_coord)
  pred_neighbors$id_period <- as.integer(pred_neighbors$id_period)
  pred_neighbors$value <- as.integer(pred_neighbors$value)

  self$pred_data_t_int <- as_pred_data_t(pred_neighbors, type = "int")

  message(glue::glue(
    "Generated {nrow(pred_meta_neighbors)} neighbor predictor variables with ",
    "{nrow(pred_neighbors)} data points"
  ))

  invisible(self)
})
