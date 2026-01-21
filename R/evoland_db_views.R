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
    include_period_0 = TRUE,
    ordered = FALSE
  ) {
    pred_types <-
      self$list_tables() |>
      grep("^pred_data_t_", x = _, value = TRUE) |>
      substr(nchar("pred_data_t_") + 1L, 30)

    stopifnot(
      "id_trans must be a single integer" = length(id_trans) == 1L && is.numeric(id_trans),
      "id_period must be NULL or a numeric vector" = is.null(id_period) || is.numeric(id_period),
      "id_pred must be NULL or a numeric vector" = is.null(id_pred) || is.numeric(id_pred),
      "include_period_0 must be a logical" = is.logical(include_period_0) &&
        length(include_period_0) == 1L,
      "no predictor tables in DB" = length(pred_types) > 0
    )

    self$with_tables(
      c("trans_meta_t", "lulc_data_t"), # do not pre-attach the predictors
      function() {
        trans_info <-
          self$get_query(glue::glue(
            "select id_lulc_anterior, id_lulc_posterior
            from trans_meta_t
            where id_trans = {id_trans}"
          ))

        if (nrow(trans_info) == 0L) {
          stop(glue::glue("Transition id_trans = {id_trans} not found in trans_meta_t"))
        }

        id_lulc_ant <- trans_info[["id_lulc_anterior"]]
        id_lulc_post <- trans_info[["id_lulc_posterior"]]

        # safe because toString on NULL gives empty string
        period_filter <-
          if (is.null(id_period)) {
            ""
          } else {
            glue::glue(" and curr.id_period in ({toString(id_period)}) ")
          }
        pred_filter <-
          if (is.null(id_pred)) {
            ""
          } else {
            glue::glue(" and id_pred in ({toString(id_pred)})")
          }

        # gathers join expression
        joins <- list()
        # gathers select expression
        selects <- list("tr.id_coord", "tr.id_period", "tr.result")
        # gathers common table expressions
        ctes <- list()

        # get view of transitions (including explicit true / false)
        ctes[["trans_result"]] <- glue::glue(
          "trans_result as (
            select
              curr.id_coord,
              curr.id_period,
              case
                when prev.id_lulc = {id_lulc_ant} and curr.id_lulc = {id_lulc_post} then true
                when prev.id_lulc = {id_lulc_ant} and curr.id_lulc != {id_lulc_post} then false
                else null
              end as result
            from
              lulc_data_t as curr
            inner join
              lulc_data_t as prev on
                curr.id_coord = prev.id_coord
                and curr.id_period = prev.id_period + 1
            where
              prev.id_lulc = {id_lulc_ant}
              {period_filter}
          )"
        )

        # read predictor data - using read_expr to enable predicate + projection pushdown
        for (pred_type in pred_types) {
          read_expr <- private$get_read_expr(paste0("pred_data_t_", pred_type))
          # union with period == 0 data before pivot
          p0_union <-
            if (include_period_0) {
              glue::glue(
                "
                union all
                  select
                    p0.id_coord, periods.id_period, p0.id_pred, p0.value
                  from
                    {read_expr} as p0
                  cross join
                    (select distinct id_period from trans_result where id_period >= 1) as periods
                  where
                    p0.id_period = 0
                    {pred_filter}
                "
              )
            } else {
              ""
            }

          ctes[[paste0("pred_", pred_type, "_combined")]] <- glue::glue(
            "
            pred_{pred_type}_combined as (
              select
                id_coord, id_period, id_pred, value
              from
                {read_expr}
              where
                id_period >= 1
                {pred_filter}
            {p0_union}
            )"
          )

          ctes[[paste0("pred_", pred_type, "_wide")]] <- glue::glue(
            "
            pred_{pred_type}_wide as (
              pivot
                pred_{pred_type}_combined
              on
                id_pred
              using
                first(value)
              group by
                id_coord, id_period
            )"
          )

          joins[[paste0("join_", pred_type)]] <- glue::glue(
            "
            left join
              pred_{pred_type}_wide as p{pred_type}
            on
              tr.id_coord = p{pred_type}.id_coord
              and tr.id_period = p{pred_type}.id_period
            "
          )

          selects[[paste0("select_", pred_type)]] <- glue::glue(
            "p{pred_type}.* exclude (id_coord, id_period)"
          )
        }

        cte_string <- paste(unlist(ctes), collapse = ",\n\n ")
        join_string <- paste(unlist(joins), collapse = "\n")
        select_string <- paste(unlist(selects), collapse = ", \n")

        order_clause <-
          if (ordered) {
            "order by tr.id_period, tr.id_coord"
          } else {
            ""
          }

        query <- glue::glue(
          "
          with
            {cte_string}
          select
            {select_string}
          from
            trans_result as tr
            {join_string}
          where
            tr.result is not null
          {order_clause}
          "
        )

        result <- self$get_query(query)

        old_names <- names(result) |> Filter(\(y) grepl("^\\d+$", y), x = _)
        new_names <- paste0("id_pred_", old_names)
        data.table::setnames(result, old_names, new_names)

        if (!is.na(na_value)) {
          for (col in new_names) {
            data.table::set(result, i = which(is.na(result[[col]])), j = col, value = na_value)
          }
        }

        result
      }
    )
  }
)

# describeIn evoland_db_views Get predictor data in a wide data.table for transition probability
#   prediction (cols id_coord, id_pred_{n})
# id_trans - integer transition ID
# id_period - integer of posterior period ID
# include_period_0 - optional bool, include data for static non-period no. 0
# na_value - if not NA, replace all NULL/NA predictor values with this value
evoland_db$set(
  "public",
  "pred_data_wide_v",
  function(
    id_trans,
    id_period,
    na_value = NA,
    include_period_0 = TRUE
  ) {
    pred_types <-
      self$list_tables() |>
      grep("^pred_data_t_", x = _, value = TRUE) |>
      substr(nchar("pred_data_t_") + 1L, 30)

    stopifnot(
      "id_trans must be a single integer" = length(id_trans) == 1L && is.numeric(id_trans),
      "id_period must be a single integer" = length(id_period) == 1L && is.numeric(id_period),
      "include_period_0 must be a logical" = length(include_period_0) == 1L &&
        is.logical(include_period_0),
      "no predictor tables in DB" = length(pred_types) > 0
    )

    trans_info <- self$trans_meta_t[.id == id_trans, env = list(.id = id_trans)]

    if (nrow(trans_info) == 0L) {
      stop(glue::glue("Transition id_trans = {id_trans} not found in trans_meta_t"))
    }

    id_lulc_ant <- trans_info[["id_lulc_anterior"]]
    id_lulc_post <- trans_info[["id_lulc_posterior"]]

    trans_preds <- self$trans_preds_t[.id == id_trans, env = list(.id = id_trans)]
    pred_filter <- glue::glue(" and id_pred in ({toString(trans_preds$id_pred)})")

    # gathers join expression
    joins <- list()
    # gathers select expression
    selects <- list("a.id_coord")
    # gathers common table expressions
    ctes <- list()

    # get only those cells potentially involved in this transition
    lulc_read_expr <- private$get_read_expr("lulc_data_t")
    ctes[["anterior_cells"]] <- glue::glue(
      "anterior_cells as (
        select
          id_coord
        from
          {lulc_read_expr}
        where
          id_lulc = {id_lulc_ant}
          and id_period = {id_period - 1}
      )"
    )

    # read predictor data - using read_expr to enable predicate + projection pushdown
    for (pred_type in pred_types) {
      pred_read_expr <- private$get_read_expr(paste0("pred_data_t_", pred_type))
      periods_selected <- if (include_period_0) c(0, id_period) else id_period

      ctes[[paste0("pred_", pred_type, "_combined")]] <- glue::glue(
        "pred_{pred_type}_combined as (
          select
            id_coord, id_pred, value
          from
            {pred_read_expr}
          where
            id_period in ({periods_selected})
            {pred_filter}
        {p0_union}
        )"
      )

      ctes[[paste0("pred_", pred_type, "_wide")]] <- glue::glue(
        "pred_{pred_type}_wide as (
          pivot
            pred_{pred_type}_combined
          on
            id_pred
          using
            first(value)
          group by
            id_coord
        )"
      )

      joins[[paste0("join_", pred_type)]] <- glue::glue(
        "left join
          pred_{pred_type}_wide as p{pred_type}
        on
          a.id_coord = p{pred_type}.id_coord"
      )

      selects[[paste0("select_", pred_type)]] <- glue::glue(
        "  p{pred_type}.* exclude (id_coord)"
      )
    }

    cte_string <- paste(unlist(ctes), collapse = ",\n\n ")
    join_string <- paste(unlist(joins), collapse = "\n")
    select_string <- paste(unlist(selects), collapse = ", \n")

    query <- glue::glue(
      "
      with
        {cte_string}
      select
        {select_string}
      from
        anterior_cells as a
      {join_string}
      "
    )

    result <- self$get_query(query)

    # all cols with numbers as names are predictors
    old_names <- names(result) |> Filter(\(y) grepl("^\\d+$", y), x = _)
    new_names <- paste0("id_pred_", old_names)
    data.table::setnames(result, old_names, new_names)

    if (!is.na(na_value)) {
      for (col in new_names) {
        data.table::set(result, i = which(is.na(result[[col]])), j = col, value = na_value)
      }
    }

    result
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
            AND r.id_period = {id_period}
            AND m.is_viable"
        ))

        result
      }
    )
  }
)
