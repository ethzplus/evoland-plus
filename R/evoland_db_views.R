#' Views on the evoland-plus data model
#'
#' @description
#' Functions to generate views on the database
#'
#' @name evoland_db_views
NULL

#' @describeIn evoland_db_views Retrieve a table of distinct predictor urls and their
#' md5sum
make_pred_sources_v <- function(self, private) {
  file_info <- private$get_file_path("pred_meta_t")

  if (!file_info$exists) {
    return(data.table::data.table(
      url = character(0),
      md5sum = character(0)
    ))
  }

  self$get_query(glue::glue(
    r"{
    select distinct
      unnest(sources).url as url,
      unnest(sources).md5sum as md5sum
    from read_{file_info$format}('{file_info$path}')
    where sources is not null
    }"
  ))
}


#' @describeIn evoland_db_views Return a `lulc_meta_long_v` instance, i.e. unrolled `lulc_meta_t`.
make_lulc_meta_long_v <- function(self, private) {
  file_info <- private$get_file_path("lulc_meta_t")

  if (!file_info$exists) {
    return(data.table::data.table(
      id_lulc = integer(0),
      name = character(0),
      src_class = integer(0)
    ))
  }

  self$get_query(glue::glue(
    r"{
    select
      id_lulc,
      name,
      unnest(src_classes) as src_class
    from read_{file_info$format}('{file_info$path}')
    }"
  ))
}

#' @describeIn evoland_db_views Minimal coordinate representation (id_coord, lon, lat)
make_coords_minimal <- function(self, private) {
  file_info <- private$get_file_path("coords_t")

  if (!file_info$exists) {
    return(data.table::data.table(
      id_coord = integer(0),
      lon = numeric(0),
      lat = numeric(0)
    ))
  }

  sql <- glue::glue(
    "SELECT id_coord, lon, lat FROM read_{file_info$format}('{file_info$path}')"
  )

  self$get_query(sql) |>
    cast_dt_col("id_coord", as.integer) |>
    data.table::setkeyv("id_coord")
}
