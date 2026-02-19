#' Views on the evoland-plus data model
#'
#' @description
#' This file adds view active bindings and methods to the `evoland_db` class using R6's `$set()`
#' method. These provide computed views on the database without storing additional data.
#'
#' @section Active Bindings Added:
#'
#' - `lulc_meta_long_v` - Unrolled LULC metadata with one row per source class
#' - `pred_sources_v` - Distinct predictor URLs and their MD5 checksums
#' - `trans_v` - Land use transitions derived from lulc_data_t
#' - `extent` - Spatial extent of coords_t as terra::SpatExtent
#' - `coords_minimal` - Minimal coordinate representation (id_coord, lon, lat)
#'
#' @section Methods Added:
#'
#' - `trans_pred_data_v(id_trans)` - Returns wide table of transition results and predictor data for
#'   a specific transition. Used as input to covariance filtering.
#' - `trans_rates_dinamica_v(id_period)` - Returns transition rates formatted for Dinamica export
#'   for a specific period.
#'
#' @name evoland_db_views
#' @aliases lulc_meta_long_v pred_sources_v trans_v coords_minimal trans_pred_data_v
#' trans_rates_dinamica_v
#' @include evoland_db.R
NULL

evoland_db$set("active", "lulc_meta_long_v", function() {
  self$get_query(glue::glue(
    r"{
    select
      id_lulc,
      name,
      unnest(src_classes) as src_class
    from
      {self$get_read_expr("lulc_meta_t")}
    }"
  ))
})

evoland_db$set("active", "pred_sources_v", function() {
  self$get_query(glue::glue(
    r"{
    select distinct
      unnest(sources).url as url,
      unnest(sources).md5sum as md5sum
    from {self$get_read_expr("pred_meta_t")}
    where sources is not null
    }"
  ))
})

evoland_db$set("active", "trans_v", function() {
  lulc_read_expr <- self$get_read_expr("lulc_data_t")
  self$get_query(glue::glue(
    r"{
    select
      curr.id_period,
      prev.id_lulc as id_lulc_anterior,
      curr.id_lulc as id_lulc_posterior,
      curr.id_coord
    from
      {lulc_read_expr} as curr
    inner join
      {lulc_read_expr} as prev
    on
      curr.id_coord = prev.id_coord
      and curr.id_period = prev.id_period + 1
    }"
  ))
})

evoland_db$set("active", "extent", function() {
  coords_read_expr <- self$get_read_expr("coords_t")
  self$get_query(glue::glue(
    r"{
      select
        min(lon) as xmin,
        max(lon) as xmax,
        min(lat) as ymin,
        max(lat) as ymax
      from
        {coords_read_expr}
      }"
  )) |>
    unlist() |>
    terra::ext()
})

evoland_db$set("active", "coords_minimal", function() {
  coords_read_expr <- self$get_read_expr("coords_t")
  metadata <- self$get_table_metadata("coords_t")
  self$get_query(glue::glue(
    r"{
      select id_coord, lon, lat
      from {coords_read_expr}
      }"
  )) |>
    cast_dt_col("id_coord", "int") |>
    data.table::setkeyv("id_coord") |>
    data.table::setattr("epsg", metadata[["epsg"]]) |>
    data.table::setattr("xmin", metadata[["xmin"]]) |>
    data.table::setattr("xmax", metadata[["xmax"]]) |>
    data.table::setattr("ymin", metadata[["ymin"]]) |>
    data.table::setattr("ymax", metadata[["ymax"]]) |>
    data.table::setattr("resolution", metadata[["resolution"]])
})


# get transition rates formatted for Dinamica export
# id_period - integer period ID for which to export rates
evoland_db$set(
  "public",
  "trans_rates_dinamica_v",
  function(id_period) {
    stopifnot(
      "id_period must be a single integer" = {
        length(id_period) == 1L && id_period == as.integer(id_period)
      }
    )

    rates_read_expr <- self$get_read_expr("trans_rates_t")
    meta_read_expr <- self$get_read_expr("trans_meta_t")

    result <- self$get_query(glue::glue(
      r"{
      select
        m.id_lulc_anterior as \"from*\",
        m.id_lulc_posterior as \"to*\",
        r.rate as \"rate\"
      from
        {rates_read_expr} r,
        {meta_read_expr} m
      where
        r.id_trans = m.id_trans
        and r.id_period = {id_period}
        and m.is_viable
      }"
    ))

    result
  }
)
