#' Table active bindings for evoland_db
#'
#' @description
#' This file adds table active bindings to the `evoland_db` class using R6's `$set()` method.
#' These bindings provide read/write access to database tables with automatic validation.
#'
#' @section Active Bindings Added:
#'
#' - `coords_t` - Coordinates table. See [as_coords_t()]
#' - `periods_t` - Time periods table. See [as_periods_t()]
#' - `lulc_meta_t` - Land use/land cover metadata. See [as_lulc_meta_t()]
#' - `lulc_data_t` - Land use/land cover data. See [as_lulc_data_t()]
#' - `pred_data_t_float` - Float predictor data. See [as_pred_data_t()]
#' - `pred_data_t_int` - Integer predictor data. See [as_pred_data_t()]
#' - `pred_data_t_bool` - Boolean predictor data. See [as_pred_data_t()]
#' - `pred_meta_t` - Predictor metadata. See [as_pred_meta_t()]
#' - `trans_meta_t` - Transition metadata. See [as_trans_meta_t()]
#' - `trans_preds_t` - Transition-predictor relationships. See [as_trans_preds_t()]
#' - `intrv_meta_t` - Intervention metadata. See [as_intrv_meta_t()]
#' - `intrv_masks_t` - Intervention masks. See [as_intrv_masks_t()]
#' - `trans_models_t` - Transition models. See [as_trans_models_t()]
#' - `alloc_params_t` - Allocation parameters. See [as_alloc_params_t()]
#' - `neighbors_t` - Neighbor relationships. See [as_neighbors_t()]
#'
#' @name evoland_db_tables
#' @include evoland_db.R
NULL

create_table_binding <- function(
  self,
  table_name,
  as_fn,
  key_cols = NULL,
  autoincrement_cols = NULL,
  map_cols = NULL,
  format = NULL,
  ...
) {
  function(x) {
    if (missing(x)) {
      fetched <- self$fetch(table_name)

      if (!is.null(map_cols) && nrow(fetched) > 0) {
        fetched <- convert_list_cols(fetched, map_cols, kv_df_to_list)
      }

      return(as_fn(fetched, ...))
    }

    stopifnot(inherits(x, table_name))

    self$commit(
      x,
      table_name = table_name,
      key_cols = key_cols,
      autoincrement_cols = autoincrement_cols,
      map_cols = map_cols,
      method = "upsert",
      format = format
    )
  }
}

evoland_db$set("active", "coords_t", function(x) {
  create_table_binding(
    self,
    "coords_t",
    as_coords_t,
    key_cols = "id_coord"
  )(x)
})

evoland_db$set("active", "periods_t", function(x) {
  create_table_binding(
    self,
    "periods_t",
    as_periods_t,
    key_cols = "id_period"
  )(x)
})

evoland_db$set("active", "lulc_meta_t", function(x) {
  create_table_binding(
    self,
    "lulc_meta_t",
    as_lulc_meta_t,
    key_cols = "id_lulc",
    form = "json"
  )(x)
})

evoland_db$set("active", "lulc_data_t", function(x) {
  create_table_binding(
    self,
    "lulc_data_t",
    as_lulc_data_t,
    key_cols = c("id_coord", "id_period")
  )(
    x
  )
})

evoland_db$set("active", "pred_data_t_float", function(x) {
  create_table_binding(
    self,
    "pred_data_t_float",
    as_pred_data_t,
    key_cols = c("id_pred", "id_coord", "id_period"),
    type = "float"
  )(x)
})

evoland_db$set("active", "pred_data_t_int", function(x) {
  create_table_binding(
    self,
    "pred_data_t_int",
    as_pred_data_t,
    key_cols = c("id_pred", "id_coord", "id_period"),
    type = "int"
  )(x)
})

evoland_db$set("active", "pred_data_t_bool", function(x) {
  create_table_binding(
    self,
    "pred_data_t_bool",
    as_pred_data_t,
    key_cols = c("id_pred", "id_coord", "id_period"),
    type = "bool"
  )(x)
})

evoland_db$set("active", "pred_meta_t", function(x) {
  create_table_binding(
    self,
    "pred_meta_t",
    as_pred_meta_t,
    key_cols = "name",
    autoincrement_cols = "id_pred",
    format = "json"
  )(x)
})

evoland_db$set("active", "trans_meta_t", function(x) {
  create_table_binding(
    self,
    "trans_meta_t",
    as_trans_meta_t,
    key_cols = c("id_lulc_anterior", "id_lulc_posterior"),
    autoincrement_cols = "id_trans",
    format = "json"
  )(x)
})

evoland_db$set("active", "trans_preds_t", function(x) {
  create_table_binding(
    self,
    "trans_preds_t",
    as_trans_preds_t,
    key_cols = c("id_trans", "id_pred")
  )(x)
})

evoland_db$set("active", "intrv_meta_t", function(x) {
  create_table_binding(
    self,
    "intrv_meta_t",
    as_intrv_meta_t,
    key_cols = "id_intrv",
    map_cols = "params",
    format = "json"
  )(x)
})

evoland_db$set("active", "intrv_masks_t", function(x) {
  create_table_binding(
    self,
    "intrv_masks_t",
    as_intrv_masks_t,
    key_cols = c("id_coord", "id_intrv")
  )(x)
})

evoland_db$set("active", "trans_models_t", function(x) {
  create_table_binding(
    self,
    "trans_models_t",
    as_trans_models_t,
    key_cols = c("id_trans", "fit_call"),
    map_cols = c("model_params", "goodness_of_fit")
  )(x)
})

evoland_db$set("active", "alloc_params_t", function(x) {
  create_table_binding(
    self,
    "alloc_params_t",
    as_alloc_params_t,
    key_cols = c("id_trans", "id_period"),
    map_cols = c("alloc_params", "goodness_of_fit"),
    format = "json"
  )(x)
})

evoland_db$set("active", "neighbors_t", function(x) {
  create_table_binding(
    self,
    "neighbors_t",
    as_neighbors_t,
    key_cols = c("id_coord_origin", "id_coord_neighbor")
  )(x)
})
