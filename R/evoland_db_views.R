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
  file_info <- private$get_file_path("pred_meta_t")

  if (!file_info$exists) {
    return(data.table::data.table(
      url = character(0),
      md5sum = character(0)
    ))
  }

  where_clause <- if (!is.null(where)) paste("AND", where) else ""

  self$get_query(glue::glue(
    r"{
    select distinct
      unnest(sources).url as url,
      unnest(sources).md5sum as md5sum
    from read_{file_info$format}('{file_info$path}')
    where sources is not null {where_clause}
    }"
  ))
}


#' @describeIn evoland_db_views Return a `lulc_meta_long_v` instance, i.e. unrolled `lulc_meta_t`.
make_lulc_meta_long_v <- function(self, private, where = NULL) {
  file_info <- private$get_file_path("lulc_meta_t")

  if (!file_info$exists) {
    return(data.table::data.table(
      id_lulc = integer(0),
      name = character(0),
      src_class = integer(0)
    ))
  }

  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""

  self$get_query(glue::glue(
    r"{
    select
      id_lulc,
      name,
      unnest(src_classes) as src_class
    from read_{file_info$format}('{file_info$path}')
    {where_clause}
    }"
  ))
}

#' @describeIn evoland_db_views Minimal coordinate representation (id_coord, lon, lat)
make_coords_minimal <- function(self, private, where = NULL) {
  file_info <- private$get_file_path("coords_t")

  if (!file_info$exists) {
    return(data.table::data.table(
      id_coord = integer(0),
      lon = numeric(0),
      lat = numeric(0)
    ))
  }

  where_clause <- if (!is.null(where)) paste("WHERE", where) else ""

  sql <- glue::glue(
    r"{
    select id_coord, lon, lat 
    from read_{file_info$format}('{file_info$path}') {where_clause}
    }"
  )

  self$get_query(sql) |>
    cast_dt_col("id_coord", as.integer) |>
    data.table::setkeyv("id_coord")
}

#' @describeIn evoland_db_views Returns the extent of the coords_t as terra::SpatExtent
make_extent_db <- function(self, private) {
  file_info <- private$get_file_path("coords_t")

  self$get_query(glue::glue(
    r"{
        SELECT
          min(lon) as xmin,
          max(lon) as xmax,
          min(lat) as ymin,
          max(lat) as ymax
        FROM
          read_{file_info$format}('{file_info$path}')
        }"
  )) |>
    unlist() |>
    terra::ext()
}
