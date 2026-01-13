#' Views on the evoland-plus data model
#'
#' @description
#' This file adds view active bindings and methods to the `evoland_db` class using R6's `$set()` method.
#' These provide computed views on the database without storing additional data.
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
#' - `trans_pred_data_v(id_trans)` - Returns wide table of transition results and predictor data for a specific transition. Used as input to covariance filtering.
#' - `trans_rates_dinamica_v(id_period)` - Returns transition rates formatted for Dinamica export for a specific period.
#'
#' @name evoland_db_views
#' @include evoland_db.R
NULL

evoland_db$set("active", "lulc_meta_long_v", function() {
  self$with_tables("lulc_meta_t", function() {
    self$get_query(glue::glue(
      r"{
      select
        id_lulc,
        name,
        unnest(src_classes) as src_class
      from
        lulc_meta_t
      }"
    ))
  })
})

evoland_db$set("active", "pred_sources_v", function() {
  self$with_tables("pred_meta_t", function() {
    self$get_query(glue::glue(
      r"{
      select distinct
        unnest(sources).url as url,
        unnest(sources).md5sum as md5sum
      from pred_meta_t
      where sources is not null
      }"
    ))
  })
})

evoland_db$set("active", "trans_v", function() {
  self$with_tables("lulc_data_t", function() {
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
      }"
    ))
  })
})

evoland_db$set("active", "extent", function() {
  self$with_tables("coords_t", function() {
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
  })
})

evoland_db$set("active", "coords_minimal", function() {
  self$with_tables("coords_t", function() {
    self$get_query(glue::glue(
      r"{
      select id_coord, lon, lat
      from coords_t
      }"
    )) |>
      cast_dt_col("id_coord", "int") |>
      data.table::setkeyv("id_coord")
  })
})

# get transitions along with their predictor data in a wide data.table
# id_trans - integer transition ID
# id_pred - optional integer vector of predictor IDs to include (NULL = all predictors)
# id_period - optional integer vector of period IDs to include (NULL = all predictors)
# include_period_0 - optional bool, include the static non-period no. 0?
# na_value - if not NA, replace all NULL/NA predictor values with this value
evoland_db$set(
  "public",
  "trans_pred_data_v",
  function(
    id_trans,
    id_period = NULL,
    id_pred = NULL,
    na_value = NA,
    include_period_0 = TRUE
  ) {
    stopifnot(
      "id_trans must be a single integer" = length(id_trans) == 1L && is.numeric(id_trans),
      "id_period must be NULL or a numeric vector" = is.null(id_period) || is.numeric(id_period),
      "id_pred must be NULL or a numeric vector" = is.null(id_pred) || is.numeric(id_pred),
      "include_period_0 must be a logical" = is.logical(include_period_0) &&
        length(include_period_0) == 1L
    )

    all_tables <- self$list_tables()
    pred_tables <- c("pred_data_t_float", "pred_data_t_int", "pred_data_t_bool")
    existing_pred_tables <- intersect(pred_tables, all_tables)

    tables_to_attach <- c("trans_meta_t", "lulc_data_t", existing_pred_tables)

    self$with_tables(
      tables_to_attach,
      function() {
        trans_info <- self$get_query(glue::glue(
          "SELECT id_lulc_anterior, id_lulc_posterior
         FROM trans_meta_t
         WHERE id_trans = {id_trans}"
        ))

        if (nrow(trans_info) == 0L) {
          stop(glue::glue("Transition id_trans = {id_trans} not found in trans_meta_t"))
        }

        id_lulc_ant <- trans_info[["id_lulc_anterior"]]
        id_lulc_post <- trans_info[["id_lulc_posterior"]]

        period_filter <- ""
        if (!is.null(id_period)) {
          period_filter <- glue::glue(" AND curr.id_period IN ({toString(id_period)})")
        }

        ctes <- list()

        ctes$trans_result <- glue::glue(
          "trans_result AS (
            SELECT
              curr.id_coord,
              curr.id_period,
              CASE
                WHEN prev.id_lulc = {id_lulc_ant} AND curr.id_lulc = {id_lulc_post} THEN TRUE
                WHEN prev.id_lulc = {id_lulc_ant} AND curr.id_lulc != {id_lulc_post} THEN FALSE
                ELSE NULL
              END AS result
            FROM lulc_data_t AS curr
            INNER JOIN lulc_data_t AS prev
              ON curr.id_coord = prev.id_coord
              AND curr.id_period = prev.id_period + 1
            WHERE prev.id_lulc = {id_lulc_ant}
            {period_filter}
          )"
        )

        pred_filter <- ""
        if (!is.null(id_pred)) {
          pred_filter <- glue::glue(" AND id_pred IN ({toString(id_pred)})")
        }

        if ("pred_data_t_float" %in% existing_pred_tables) {
          if (include_period_0) {
            p0_union <- glue::glue(
              "
            UNION ALL
            SELECT p0.id_coord, periods.id_period, p0.id_pred, p0.value
            FROM pred_data_t_float AS p0
            CROSS JOIN (SELECT DISTINCT id_period FROM trans_result WHERE id_period >= 1) AS periods
            WHERE p0.id_period = 0
              {pred_filter}"
            )
          } else {
            p0_union <- ""
          }
          ctes$pred_float_combined <- glue::glue(
            "pred_float_combined AS (
            SELECT id_coord, id_period, id_pred, value
            FROM pred_data_t_float
            WHERE id_period >= 1
              {pred_filter}
            {p0_union}
            )"
          )

          ctes$pred_float_wide <- "pred_float_wide AS (
          PIVOT pred_float_combined ON id_pred USING FIRST(value) GROUP BY id_coord, id_period
        )"
        }

        if ("pred_data_t_int" %in% existing_pred_tables) {
          if (include_period_0) {
            p0_union <- glue::glue(
              "
            UNION ALL
            SELECT p0.id_coord, periods.id_period, p0.id_pred, p0.value
            FROM pred_data_t_int AS p0
            CROSS JOIN (SELECT DISTINCT id_period FROM trans_result WHERE id_period >= 1) AS periods
            WHERE p0.id_period = 0
              {pred_filter}"
            )
          } else {
            p0_union <- ""
          }
          ctes$pred_int_combined <- glue::glue(
            "pred_int_combined AS (
          SELECT id_coord, id_period, id_pred, value
          FROM pred_data_t_int
          WHERE id_period >= 1
            {pred_filter}
          {p0_union}
          )"
          )

          ctes$pred_int_wide <- "pred_int_wide AS (
          PIVOT pred_int_combined ON id_pred USING FIRST(value) GROUP BY id_coord, id_period
        )"
        }

        if ("pred_data_t_bool" %in% existing_pred_tables) {
          if (include_period_0) {
            p0_union <- glue::glue(
              "
            UNION ALL
            SELECT p0.id_coord, periods.id_period, p0.id_pred, p0.value
            FROM pred_data_t_bool AS p0
            CROSS JOIN (SELECT DISTINCT id_period FROM trans_result WHERE id_period >= 1) AS periods
            WHERE p0.id_period = 0
              {pred_filter}"
            )
          } else {
            p0_union <- ""
          }
          ctes$pred_bool_combined <- glue::glue(
            "pred_bool_combined AS (
          SELECT id_coord, id_period, id_pred, value
          FROM pred_data_t_bool
          WHERE id_period >= 1
            {pred_filter}
          {p0_union}
          )"
          )

          ctes$pred_bool_wide <- "pred_bool_wide AS (
          PIVOT pred_bool_combined ON id_pred USING FIRST(value) GROUP BY id_coord, id_period
        )"
        }

        select_cols <- "tr.id_coord, tr.id_period, tr.result"
        if ("pred_data_t_float" %in% existing_pred_tables) {
          select_cols <- paste0(select_cols, ", pf.* EXCLUDE (id_coord, id_period)")
        }
        if ("pred_data_t_int" %in% existing_pred_tables) {
          select_cols <- paste0(select_cols, ", pi.* EXCLUDE (id_coord, id_period)")
        }
        if ("pred_data_t_bool" %in% existing_pred_tables) {
          select_cols <- paste0(select_cols, ", pb.* EXCLUDE (id_coord, id_period)")
        }

        joins <- ""
        if ("pred_data_t_float" %in% existing_pred_tables) {
          joins <- paste0(
            joins,
            "\n LEFT JOIN
                pred_float_wide AS pf
              ON
                tr.id_coord = pf.id_coord
                AND tr.id_period = pf.id_period"
          )
        }
        if ("pred_data_t_int" %in% existing_pred_tables) {
          joins <- paste0(
            joins,
            "\n LEFT JOIN
                pred_int_wide AS pi
              ON
                tr.id_coord = pi.id_coord
                AND tr.id_period = pi.id_period"
          )
        }
        if ("pred_data_t_bool" %in% existing_pred_tables) {
          joins <- paste0(
            joins,
            "\n LEFT JOIN
                pred_bool_wide AS pb
              ON
                tr.id_coord = pb.id_coord
                AND tr.id_period = pb.id_period"
          )
        }

        cte_string <- paste(unlist(ctes), collapse = ",\n\n ")

        query <- glue::glue(
          "WITH {cte_string}

        SELECT {select_cols}
        FROM trans_result AS tr{joins}
        WHERE tr.result IS NOT NULL"
        )

        result <- self$get_query(query)

        old_names <- names(result)
        new_names <- old_names
        for (i in seq_along(old_names)) {
          if (old_names[i] != "result" && grepl("^\\d+$", old_names[i])) {
            new_names[i] <- paste0("id_pred_", old_names[i])
          }
        }
        data.table::setnames(result, old_names, new_names)

        if (!is.na(na_value)) {
          pred_cols <- setdiff(names(result), "result")
          for (col in pred_cols) {
            data.table::set(result, i = which(is.na(result[[col]])), j = col, value = na_value)
          }
        }

        result
      }
    )
  }
)

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

    self$with_tables(
      c("trans_rates_t", "trans_meta_t"),
      function() {
        result <- self$get_query(glue::glue(
          "SELECT
            m.id_lulc_anterior as \"From*\",
            m.id_lulc_posterior as \"To*\",
            r.rate as \"Rate\"
          FROM
            trans_rates_t r,
            trans_meta_t m
          WHERE
            r.id_trans = m.id_trans
            AND r.id_period = {id_period}"
        ))

        result
      }
    )
  }
)
