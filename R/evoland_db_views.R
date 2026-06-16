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
#' - `adjusted_trans_pot_v(id_period_post)` - Returns allocation-ready transition potentials:
#'   column-scaled to match target transition rates, then row-closed so per-cell change
#'   probabilities sum to at most 1.
#' - `alloc_params_clumpy_v()` - Returns allocation parameters in CLUMPY format
#'   (area_mean, area_var, eccentricity per transition).
#'
#' @name evoland_db_views
#' @aliases lulc_meta_long_v pred_sources_v trans_v coords_minimal trans_rates_dinamica_v adjusted_trans_pot_v alloc_params_clumpy_v
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
  overwrite = TRUE,
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
        m.id_lulc_anterior as "From*",
        m.id_lulc_posterior as "To*",
        r.rate as "Rate"
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

# Return allocation-ready transition potentials for a given posterior period.
#
# The raw potentials stored in trans_pot_t are per-transition MLR3 model
# probabilities that are NOT calibrated to the target transition demand.
# This view applies two adjustments (c.f. Mazy, 2022, section 2.5):
#
#   1. Column scaling: each transition's raw potentials are multiplied by
#      rate / mean_potential so that the column mean matches the target
#      transition rate from trans_rates_t.
#
#   2. Row closure: where the column-scaled probabilities for a cell sum to
#      more than 1, all values for that cell are divided by the row sum.
#      The implicit "no-change" probability is (1 - sum of stored values).
#
# id_period_post - integer posterior period ID
evoland_db$set(
  "public",
  "adjusted_trans_pot_v",
  overwrite = TRUE,
  function(id_period_post) {
    stopifnot(
      "id_period_post must be a single integer" = {
        length(id_period_post) == 1L && id_period_post == as.integer(id_period_post)
      }
    )

    pot_read_expr <- self$get_read_expr("trans_pot_t")
    rates_read_expr <- self$get_read_expr("trans_rates_t")

    self$get_query(glue::glue(
      r"{
      with raw as (
        select
          t.id_trans,
          t.id_coord,
          t.id_period_post,
          t.value,
          r.rate,
          avg(t.value) over (partition by t.id_trans) as mean_value
        from {pot_read_expr} t
        join {rates_read_expr} r
          on t.id_trans = r.id_trans
          and r.id_period = t.id_period_post
        where t.id_period_post = {id_period_post}
      ),
      scaled as (
        select
          id_trans,
          id_coord,
          id_period_post,
          case
            when mean_value > 0 then value * rate / mean_value
            else 0.0
          end as scaled_value
        from raw
      ),
      closed as (
        select
          id_trans,
          id_coord,
          id_period_post,
          case
            when sum(scaled_value) over (partition by id_coord) > 1.0
            then scaled_value / sum(scaled_value) over (partition by id_coord)
            else scaled_value
          end as value
        from scaled
      )
      select id_trans, id_coord, id_period_post, value
      from closed
      }"
    ))
  }
)

# Return allocation parameters in CLUMPY-compatible format.
#
# Maps the raw patch statistics stored in alloc_params_t to the three
# parameters consumed by the CLUMPY LogNormPatcher:
#   - area_mean      <- mean_patch_size   (mean cells per patch)
#   - area_var       <- patch_size_variance (variance, cell^2)
#   - eccentricity   <- patch_elongation  (1 - sqrt(lambda2/lambda1))
#
# Uses the active id_run / run lineage via get_read_expr.
evoland_db$set(
  "public",
  "alloc_params_clumpy_v",
  overwrite = TRUE,
  function() {
    params_read_expr <- self$get_read_expr("alloc_params_t")

    self$get_query(glue::glue(
      r"{
      select
        id_run,
        id_trans,
        mean_patch_size     as area_mean,
        patch_size_variance as area_var,
        patch_elongation    as eccentricity
      from {params_read_expr}
      }"
    ))
  }
)
