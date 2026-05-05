#' @title `evoland_db` - R6 Class for lulc data management
#'
#' @description
#' An R6 class that provides an interface to a folder-based data storage system
#' for the evoland package. Each table is stored as a parquet (or JSON) file.
#' This class uses DuckDB for in-memory SQL operations while persisting data
#' to disk in parquet format for better compression.
#'
#' Inherits from [parquet_db] for generic database operations.
#'
#' @seealso
#' Additional methods and active bindings are added to this class in separate files:
#'
#' - [evoland_db_views] - View active bindings (lulc_meta_long_v, etc.) and methods
#'
#' @include parquet_db.R parquet_db_utils.R
#' @export

evoland_db <- R6::R6Class(
  classname = "evoland_db",
  inherit = parquet_db,

  ## Public Methods ----
  public = list(
    #' @description
    #' Initialize a new evoland_db object
    #' @param path Character string. Path to the data folder.
    #' @param id_run Atomic integer run ID, defaults to 0. Can be set to NULL
    #' @param read_only Logical. Whether to update the reporting table upon
    #' initialization; if TRUE, only parallel-safe appends are allowed.
    #' @param ... passed on to `set_report`
    #'
    #' @return A new `evoland_db` object
    initialize = function(
      path,
      id_run = 0L,
      read_only = FALSE,
      ...
    ) {
      super$initialize(path = path, read_only = read_only, extensions = "spatial")
      if (!read_only) {
        self$set_report(...)
        self$commit(as_runs_t(), "runs_t", method = "upsert")
      }
      # ensure there is a minimal runs_t with base case
      self$id_run <- id_run
      invisible(self)
    },

    #' @description
    #' Get SQL expression to read a table, respecting active run hierarchy. For each
    #' data slice, returns data from the closest ancestor run that has it.
    #' @param table_name Character string table name
    #' @return Character string SQL expression (a composable subquery)
    get_read_expr = function(table_name) {
      create_method_binding(get_evoland_db_read_expr, with_super = TRUE)
    },

    #' @description Print method for evoland_db
    #' @param ... passed to super$print
    print = function(...) {
      super$print(
        subheaders = c(
          paste("Active Run:", self$id_run),
          paste("Lineage:", self$run_lineage)
        )
      )
    },

    ### Setter methods ----
    #' @description Set reporting metadata, see [db_set_report()]
    #' @param ... each named argument is entered into the table with the argument name
    #' as its key
    set_report = function(...) {
      create_method_binding(db_set_report)
    },

    #' @description Set the neighbors table, see [set_neighbors()]
    #' @param max_distance Maximum distance
    #' @param distance_breaks Numeric vector of distance breaks
    #' @param overwrite Logical, whether to overwrite existing neighbors_t table
    #' @param quiet Logical, whether to suppress messages about progress
    #' @param chunksize Integer, chunksize for writing; keeps down memory pressure
    set_neighbors = function(
      max_distance = 1000,
      distance_breaks = NULL,
      overwrite = FALSE,
      quiet = FALSE,
      chunksize = 1e8
    ) {
      create_method_binding(set_neighbors)
    },

    #' @description Generate neighbor prediction table, i.e. "how many neighbors within
    #' distance X are of type Y", see [generate_neighbor_predictors()]
    generate_neighbor_predictors = function() {
      create_method_binding(generate_neighbor_predictors)
    },

    #' @description Append new neighbors to predictor for a given period; depends on
    #' generate_neighbor_predictors() having been run.
    #' @param id_period Period to calculate predictors for
    upsert_new_neighbors = function(id_period) {
      create_method_binding(upsert_new_neighbors)
    },

    #' @description Get transitions along with their predictor data in a wide
    #' data.table, see [trans_pred_data_v()]
    #' @param id_trans Integer transition ID, see [trans_meta_t]
    #' @param id_pred Optional integer vector of predictor IDs to include
    #' @param ordered - if TRUE, order output by `id_coord` & `id_period`
    trans_pred_data_v = function(id_trans, id_pred, ordered = FALSE) {
      create_method_binding(trans_pred_data_v)
    },

    #' @description Retrieve a wide view of the predictor data, see [pred_data_wide_v()]
    #' @param id_trans Integer transition ID, see [trans_meta_t]
    #' @param id_period_anterior Integer ID of period to retrieve data for
    pred_data_wide_v = function(id_trans, id_period_anterior) {
      create_method_binding(pred_data_wide_v)
    },

    ### Allocation methods ---
    #' @description Runs a path-dependent Monte Carlo simulation using Dinamica
    #' EGO, see [alloc_dinamica()]
    #' @param id_periods Integer vector of period IDs to include in the simulation.
    #' @param gof_criterion Which goodness-of-fit metric to use for model selection (e.g., "auc")
    #' @param gof_maximize Maximize (TRUE) or minimize (FALSE) the gof_criterion?
    #' @param work_dir Character path for Dinamica working directory. Default "dinamica_rundir".
    #' @param keep_intermediate Logical, keep intermediate Dinamica files? Default FALSE.
    alloc_dinamica = function(
      id_periods,
      gof_criterion,
      gof_maximize,
      work_dir = "dinamica_rundir",
      keep_intermediate = FALSE
    ) {
      create_method_binding(alloc_dinamica)
    },

    #' @description
    #' Evaluates allocation parameters in dinamica, see [eval_alloc_params_t()]
    #' @param gof_criterion Which goodness-of-fit metric to use for model selection (e.g., "auc")
    #' @param gof_maximize Maximize (TRUE) or minimize (FALSE) the gof_criterion?
    #' @param work_dir Character path for Dinamica working directory. Default "dinamica_rundir".
    #' @param keep_intermediate Logical, keep intermediate Dinamica files? Default FALSE.
    eval_alloc_params_t = function(
      gof_criterion,
      gof_maximize,
      work_dir = "dinamica_rundir",
      keep_intermediate = FALSE
    ) {
      create_method_binding(eval_alloc_params_t)
    },

    #' @description
    #' Computes allocation parameters for all viable transitions, see [create_alloc_params_t()]
    #' @param n_perturbations Number of perturbed parameter sets to generate, each
    #' assigned to a in id_run in [runs_t]
    #' @param sd Standard deviation for random perturbation of frac_expander
    create_alloc_params_t = function(n_perturbations = 5L, sd = 0.05) {
      create_method_binding(create_alloc_params_t)
    },

    #' @description Retrieve LULC data as a SpatRaster object for a given
    #' period. See [lulc_data_as_rast()]
    #' @param id_period Optional integer vector of period IDs to include. If
    #'   NULL (default), all periods are included.
    lulc_data_as_rast = function(id_period = NULL) {
      create_method_binding(lulc_data_as_rast)
    },

    #' @description
    #' Fit full models (trained on the complete dataset) for each viable transition,
    #' see [fit_full_models()]. Two mutually exclusive modes: pass `learner` to train
    #' directly, or pass `select_score` to pick the best partial model by score.
    #' @param learner An mlr3 `Learner` or `AutoTuner` for direct-learner mode (`NULL`
    #'   when `select_score` is used).
    #' @param select_score Measure ID string for score-select mode, e.g. `"classif.auc"`
    #'   (`NULL` when `learner` is used).
    #' @param select_maximize Logical; maximize (`TRUE`) or minimize (`FALSE`) the score.
    #' @param cluster Optional cluster object for parallel processing
    fit_full_models = function(
      learner = NULL,
      select_score = NULL,
      select_maximize = TRUE,
      cluster = NULL
    ) {
      create_method_binding(fit_full_models)
    },

    #' @description Fit partial models for each viable transition using stratified
    #' sampling. Models are trained on a subsample and evaluated on held-out data, see
    #' [fit_partial_models()] for details.
    #' @param learner An mlr3 `Learner` or `AutoTuner` R6 object.
    #' @param measures A list of mlr3 `Measure` objects for scoring the held-out split.
    #' @param sample_frac Fraction in \(0, 1\) for stratified sampling.
    #' @param seed Random seed for reproducible sampling
    #' @param cluster Optional cluster object for parallel processing
    fit_partial_models = function(
      learner,
      measures,
      sample_frac = 0.7,
      seed = NULL,
      cluster = NULL
    ) {
      create_method_binding(fit_partial_models)
    },

    #' @description
    #' Get cross-validation plots for stored predictions, see [get_crossval_plots()]
    #' @param id_run Optional integer; filter by run ID.
    #' @param id_trans Optional integer; filter by transition ID.
    get_crossval_plots = function(id_run = NULL, id_trans = NULL) {
      create_method_binding(get_crossval_plots)
    },

    #' @description
    #' Set an initial full set of transition / predictor relations, see [set_full_trans_preds()]
    #' @param overwrite Logical, whether to overwrite existing `trans_preds_t` table. Default FALSE.
    set_full_trans_preds = function(overwrite = FALSE) {
      create_method_binding(set_full_trans_preds)
    },

    #' @description Add filter scores to predictors for each `id_run, id_trans`.
    #' See [get_pred_filter_score()].
    #' @param filter Character passed to [mlr3filters::flt] or
    #' [mlr3filters::Filter] object specifying the filter method to use for
    #' feature selection.
    #' @param cluster Optional cluster object for parallel processing
    #' @param ... Additional arguments passed to `flt`.
    get_pred_filter_score = function(
      filter = "correlation",
      cluster = NULL,
      ...
    ) {
      create_method_binding(get_pred_filter_score)
    },

    #' @description
    #' Predict the transition potential for a given period, see [trans_pot_t()]
    #' @param id_period_post Integerish, posterior period of the transition potential interval
    #' @param gof_criterion Which goodness-of-fit metric to use for model selection (e.g., "auc")
    #' @param gof_maximize Maximize (TRUE) or minimize (FALSE) the gof_criterion?
    predict_trans_pot = function(id_period_post, gof_criterion, gof_maximize) {
      create_method_binding(predict_trans_pot)
    },

    #' @description Get the transition rates that were observed, see [trans_rates_t]
    get_obs_trans_rates = function() {
      create_method_binding(get_obs_trans_rates)
    }
  ),

  # Active Bindings ----
  active = list(
    #' @field coords_t Get or upsert [coords_t]
    coords_t = create_table_binding("coords_t", "write_once"),
    #' @field periods_t Get or upsert [periods_t]
    periods_t = create_table_binding("periods_t", "write_once"),
    #' @field lulc_meta_t Get or upsert [lulc_meta_t]
    lulc_meta_t = create_table_binding("lulc_meta_t", "upsert"),
    #' @field lulc_data_t Get or upsert [lulc_data_t]
    lulc_data_t = create_table_binding("lulc_data_t", "upsert"),
    #' @field pred_data_t Get or upsert [pred_data_t]
    pred_data_t = create_table_binding("pred_data_t", "upsert"),
    #' @field pred_meta_t Get or upsert [pred_meta_t]
    pred_meta_t = create_table_binding("pred_meta_t", "upsert"),
    #' @field trans_meta_t Get or upsert [trans_meta_t]
    trans_meta_t = create_table_binding("trans_meta_t", "upsert"),
    #' @field trans_preds_t Get or upsert [trans_preds_t]
    trans_preds_t = create_table_binding("trans_preds_t", "overwrite"),
    #' @field trans_rates_t Get or upsert [trans_rates_t]
    trans_rates_t = create_table_binding("trans_rates_t", "upsert"),
    #' @field intrv_meta_t Get or upsert [intrv_meta_t]
    intrv_meta_t = create_table_binding("intrv_meta_t", "upsert"),
    #' @field intrv_masks_t Get or upsert [intrv_masks_t]
    intrv_masks_t = create_table_binding("intrv_masks_t", "upsert"),
    #' @field trans_models_t Get or upsert [trans_models_t]
    trans_models_t = create_table_binding("trans_models_t", "upsert"),
    #' @field alloc_params_t Get or upsert [alloc_params_t]
    alloc_params_t = create_table_binding("alloc_params_t", "upsert"),
    #' @field neighbors_t Get or upsert [neighbors_t]
    neighbors_t = create_table_binding("neighbors_t", "write_once"),
    #' @field reporting_t Get or upsert [reporting_t]
    reporting_t = create_table_binding("reporting_t", "upsert"),
    #' @field runs_t Get or upsert [runs_t]
    runs_t = create_table_binding("runs_t", "upsert"),

    #' @field id_run Get or set active id_run, see [runs_t]
    id_run = function(x) create_method_binding(db_active_id_run, with_private = TRUE),
    #' @field run_lineage Get id_run, see [runs_t]
    run_lineage = function() {
      private$active_run_lineage
    }
  ),

  private = list(
    active_id_run = NULL,
    active_run_lineage = NULL
  )
)
