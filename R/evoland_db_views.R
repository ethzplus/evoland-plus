#' Views on the evoland-plus data model
#'
#' @description
#' Functions to generate views on the database
#'
#' @name evoland_db_views
NULL

#' @describeIn evoland_db_views Retrieve a table of distinct predictor urls and their
#' md5sum
make_pred_sources_v <- function(self, private, where = NULL) {
  self$attach_table("pred_meta_t")
  on.exit(self$detach_table("pred_meta_t"))

  where_clause <- if (!is.null(where)) paste("AND", where) else ""

  self$get_query(glue::glue(
    r"{
    select distinct
      unnest(sources).url as url,
      unnest(sources).md5sum as md5sum
    from pred_meta_t
    where sources is not null {where_clause}
    }"
  ))
}


#' @describeIn evoland_db_views Return a `lulc_meta_long_v` instance, i.e. unrolled `lulc_meta_t`.
make_lulc_meta_long_v <- function(self, private, where = NULL) {
  self$attach_table("lulc_meta_t")
  on.exit(self$detach_table("lulc_meta_t"))

  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""

  self$get_query(glue::glue(
    r"{
    select
      id_lulc,
      name,
      unnest(src_classes) as src_class
    from 
      lulc_meta_t
    {where_clause}
    }"
  ))
}

#' @describeIn evoland_db_views Minimal coordinate representation (id_coord, lon, lat)
make_coords_minimal <- function(self, private, where = NULL) {
  self$attach_table("coords_t", c("id_coord", "lon", "lat"))
  on.exit(self$detach_table("coords_t"))

  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""

  self$get_query(glue::glue(
    r"{
    select id_coord, lon, lat 
    from coords_t
    {where_clause}
    }"
  )) |>
    cast_dt_col("id_coord", as.integer) |>
    data.table::setkeyv("id_coord")
}

#' @describeIn evoland_db_views Returns the extent of the coords_t as terra::SpatExtent
make_extent_db <- function(self, private) {
  self$attach_table("coords_t", c("lon", "lat"))
  on.exit(self$detach_table("coords_t"))

  self$get_query(glue::glue(
    r"{
        SELECT
          min(lon) as xmin,
          max(lon) as xmax,
          min(lat) as ymin,
          max(lat) as ymax
        FROM
          coords_t
        }"
  )) |>
    unlist() |>
    terra::ext()
}

#' @describeIn evoland_db_views Returns transitions based on lulc_data_t
make_transitions_v <- function(self, private, where = NULL) {
  self$attach_table("lulc_data_t")
  on.exit(self$detach_table("lulc_data_t"))

  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""

  self$get_query(glue::glue(
    r"{
    SELECT
      curr.id_period,
      prev.id_lulc as id_lulc_anterior,
      curr.id_lulc as id_lulc_posterior,
      curr.id_coord
    FROM
      lulc_data_t as curr
    INNER JOIN
      lulc_data_t as prev
    ON
      curr.id_coord = prev.id_coord
      AND curr.id_period = prev.id_period + 1
    {where_clause}
    }"
  ))
}
