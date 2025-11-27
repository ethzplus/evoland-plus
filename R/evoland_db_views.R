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
  self$with_tables("pred_meta_t", function() {
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
  })
}


#' @describeIn evoland_db_views Return a `lulc_meta_long_v` instance, i.e. unrolled `lulc_meta_t`.
make_lulc_meta_long_v <- function(self, private, where = NULL) {
  self$with_tables("lulc_meta_t", function() {
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
  })
}

#' @describeIn evoland_db_views Minimal coordinate representation (id_coord, lon, lat)
make_coords_minimal <- function(self, private, where = NULL) {
  self$with_tables("coords_t", function() {
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
  })
}

#' @describeIn evoland_db_views Returns the extent of the coords_t as terra::SpatExtent
make_extent_db <- function(self, private) {
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
}

#' @describeIn evoland_db_views Returns transitions based on lulc_data_t
make_transitions_v <- function(self, private, where = NULL) {
  self$with_tables("lulc_data_t", function() {
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
  })
}

#' @describeIn evoland_db_views Returns wide table of transition results and predictor data
#' for a specific transition. Used as input to covariance filtering.
#'
#' @param id_trans Integer, the transition ID to generate data for
#' @return data.table with columns: result (0/1), id_pred_1, id_pred_2, ..., id_pred_N
make_trans_pred_data_v <- function(self, private, id_trans) {
  stopifnot(
    "id_trans must be a single integer" = length(id_trans) == 1L && is.numeric(id_trans)
  )

  # Determine which predictor tables exist
  all_tables <- self$list_tables()
  pred_tables <- c("pred_data_t_float", "pred_data_t_int", "pred_data_t_bool")
  existing_pred_tables <- intersect(pred_tables, all_tables)

  # Build list of tables to attach
  tables_to_attach <- c("trans_meta_t", "lulc_data_t", "pred_meta_t", existing_pred_tables)

  self$with_tables(
    tables_to_attach,
    function() {
      # Get transition metadata
      trans_info <- self$get_query(glue::glue(
        "SELECT id_lulc_anterior, id_lulc_posterior
         FROM trans_meta_t
         WHERE id_trans = {id_trans}"
      ))

      if (nrow(trans_info) == 0L) {
        stop(glue::glue("Transition id_trans = {id_trans} not found in trans_meta_t"))
      }

      id_lulc_ant <- trans_info$id_lulc_anterior
      id_lulc_post <- trans_info$id_lulc_posterior

      # Build CTEs dynamically based on which predictor tables exist
      ctes <- list()

      # Always include trans_result
      ctes$trans_result <- glue::glue(
        "trans_result AS (
          SELECT
            curr.id_coord,
            curr.id_period,
            CASE
              WHEN prev.id_lulc = {id_lulc_ant} AND curr.id_lulc = {id_lulc_post} THEN 1
              WHEN prev.id_lulc = {id_lulc_ant} AND curr.id_lulc != {id_lulc_post} THEN 0
              ELSE NULL
            END AS result
          FROM lulc_data_t AS curr
          INNER JOIN lulc_data_t AS prev
            ON curr.id_coord = prev.id_coord
            AND curr.id_period = prev.id_period + 1
          WHERE prev.id_lulc = {id_lulc_ant}
        )"
      )

      # Add predictor CTEs for each type that exists
      # UNION period-specific data (period >= 1) with cross-joined static data (period 0)
      if ("pred_data_t_float" %in% existing_pred_tables) {
        ctes$pred_float_combined <- "pred_float_combined AS (
          SELECT id_coord, id_period, id_pred, value
          FROM pred_data_t_float
          WHERE id_period >= 1
          UNION ALL
          SELECT p0.id_coord, periods.id_period, p0.id_pred, p0.value
          FROM pred_data_t_float AS p0
          CROSS JOIN (SELECT DISTINCT id_period FROM trans_result WHERE id_period >= 1) AS periods
          WHERE p0.id_period = 0
        )"

        ctes$pred_float_wide <- "pred_float_wide AS (
          PIVOT pred_float_combined ON id_pred USING FIRST(value) GROUP BY id_coord, id_period
        )"
      }

      if ("pred_data_t_int" %in% existing_pred_tables) {
        ctes$pred_int_combined <- "pred_int_combined AS (
          SELECT id_coord, id_period, id_pred, value
          FROM pred_data_t_int
          WHERE id_period >= 1
          UNION ALL
          SELECT p0.id_coord, periods.id_period, p0.id_pred, p0.value
          FROM pred_data_t_int AS p0
          CROSS JOIN (SELECT DISTINCT id_period FROM trans_result WHERE id_period >= 1) AS periods
          WHERE p0.id_period = 0
        )"

        ctes$pred_int_wide <- "pred_int_wide AS (
          PIVOT pred_int_combined ON id_pred USING FIRST(value) GROUP BY id_coord, id_period
        )"
      }

      if ("pred_data_t_bool" %in% existing_pred_tables) {
        ctes$pred_bool_combined <- "pred_bool_combined AS (
          SELECT id_coord, id_period, id_pred, value
          FROM pred_data_t_bool
          WHERE id_period >= 1
          UNION ALL
          SELECT p0.id_coord, periods.id_period, p0.id_pred, p0.value
          FROM pred_data_t_bool AS p0
          CROSS JOIN (SELECT DISTINCT id_period FROM trans_result WHERE id_period >= 1) AS periods
          WHERE p0.id_period = 0
        )"

        ctes$pred_bool_wide <- "pred_bool_wide AS (
          PIVOT pred_bool_combined ON id_pred USING FIRST(value) GROUP BY id_coord, id_period
        )"
      }

      # Build SELECT columns
      select_cols <- "tr.result"
      if ("pred_data_t_float" %in% existing_pred_tables) {
        select_cols <- paste0(select_cols, ", pf.* EXCLUDE (id_coord, id_period)")
      }
      if ("pred_data_t_int" %in% existing_pred_tables) {
        select_cols <- paste0(select_cols, ", pi.* EXCLUDE (id_coord, id_period)")
      }
      if ("pred_data_t_bool" %in% existing_pred_tables) {
        select_cols <- paste0(select_cols, ", pb.* EXCLUDE (id_coord, id_period)")
      }

      # Build JOINs
      joins <- ""
      if ("pred_data_t_float" %in% existing_pred_tables) {
        joins <- paste0(
          joins,
          "\n        LEFT JOIN pred_float_wide AS pf ON tr.id_coord = pf.id_coord AND tr.id_period = pf.id_period"
        )
      }
      if ("pred_data_t_int" %in% existing_pred_tables) {
        joins <- paste0(
          joins,
          "\n        LEFT JOIN pred_int_wide AS pi ON tr.id_coord = pi.id_coord AND tr.id_period = pi.id_period"
        )
      }
      if ("pred_data_t_bool" %in% existing_pred_tables) {
        joins <- paste0(
          joins,
          "\n        LEFT JOIN pred_bool_wide AS pb ON tr.id_coord = pb.id_coord AND tr.id_period = pb.id_period"
        )
      }

      # Combine everything into final query
      cte_string <- paste(unlist(ctes), collapse = ",\n\n        ")

      query <- glue::glue(
        "WITH {cte_string}

        SELECT {select_cols}
        FROM trans_result AS tr{joins}
        WHERE tr.result IS NOT NULL"
      )

      result <- self$get_query(query)

      # Rename columns to id_pred_{N} format
      old_names <- names(result)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (old_names[i] != "result" && grepl("^\\d+$", old_names[i])) {
          new_names[i] <- paste0("id_pred_", old_names[i])
        }
      }
      data.table::setnames(result, old_names, new_names)

      result
    }
  )
}
