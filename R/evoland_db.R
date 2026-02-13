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
#' - [evoland_db_neighbors] - Neighbor analysis methods
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
    #' @param update_reporting Logical. Whether to update the reporting table upon initialization.
    #' Defaults to TRUE. Set to FALSE for read-only workers to avoid lock contention.
    #' @param ... passed on to `set_report`
    #'
    #' @return A new `evoland_db` object
    initialize = function(
      path,
      id_run = 0L,
      update_reporting = TRUE,
      ...
    ) {
      # Initialize parent class with spatial extension
      super$initialize(
        path = path,
        extensions = "spatial"
      )

      # Set evoland-specific reporting metadata
      if (update_reporting) {
        self$set_report(...)
      }

      # ensure there is a minimal runs_t
      self$commit(as_runs_t(), "runs_t", method = "upsert")
      # self$set_active_run(id_run)

      invisible(self)
    },

    #' @description
    #' Set the active run for read operations
    #' @param id_run Integer run ID. Must exist in runs_t.
    #' @return Invisibly returns self
    set_active_run = function(id_run) {
      stopifnot(
        "id_run must be single integerish" = length(id_run) == 1L && as.integer(id_run) == id_run
      )

      id_run <- as.integer(id_run)

      if (!"runs_t" %in% self$list_tables()) {
        stop("runs_t does not exist; cannot set active run")
      }

      lineage <- self$with_tables("runs_t", function() {
        self$get_query(glue::glue(
          r"{
          with recursive lineage as (
            select
              id_run,
              parent_id_run,
              0 as depth
            from
              runs_t
            where
              id_run = {id_run}
            union all
            select
              r.id_run,
              r.parent_id_run,
              l.depth + 1 as depth
            from
              runs_t r
            inner join
              lineage l
              on r.id_run = l.parent_id_run
          )
          select
            id_run
          from
            lineage
          order by
            depth
          }"
        ))
      })

      if (nrow(lineage) == 0L) {
        stop(glue::glue("id_run={id_run} not found in runs_t"))
      }

      ids <- lineage[["id_run"]]

      if (anyDuplicated(ids)) {
        stop(glue::glue("Detected cycle in runs_t lineage for id_run={id_run}"))
      }

      private$id_run <- id_run
      private$run_lineage <- ids

      invisible(self)
    },

    #' @description
    #' Get the active run ID
    #' @return Integer run ID
    get_active_run = function() {
      # TODO make active binding
      private$id_run
    },

    #' @description
    #' Get SQL expression to read a table, respecting active run hierarchy
    #' @param table_name Character string table name
    #' @return Character string SQL expression
    get_read_expr = function(table_name) {
      # instead of using the run scoping, just describe the table for an id_run

      lineage <- private$run_lineage
      if (!table_name %in% private$run_scoped_tables || length(lineage) == 1L) {
        return(super$get_read_expr(table_name))
      }

      path <- self$get_table_path(table_name)
      is_partitioned <- !is.null(self$partitioning[[table_name]]) || dir.exists(path)

      base_expr <- super$get_table_path(table_name)
      run_case <- paste0(
        "case id_run ",
        paste(
          sprintf("when %s then %s", lineage, seq_along(lineage)),
          collapse = " "
        ),
        " else 999999 end"
      )
      glue::glue(
        "(select * from {base_expr} where id_run = (",
        "select id_run from {base_expr} ",
        "where id_run in ({toString(lineage)}) ",
        "order by {run_case} limit 1",
        "))"
      )
    },

    ### Setter methods ----
    #' @description Set reporting metadata, see [db_set_report()]
    #' @param ... each named argument is entered into the table with the argument name
    #' as its key
    set_report = function(...) {
      create_method_binding(db_set_report)
    },

    ### Allocation methods ---
    #' @description Runs a path-dependent Monte Carlo simulation using Dinamica
    #' EGO, see [alloc_dinamica()]
    #' @inheritParams alloc_dinamica
    alloc_dinamica = function(
      id_periods,
      work_dir = "dinamica_rundir",
      keep_intermediate = FALSE
    ) {
      create_method_binding(alloc_dinamica)
    },

    #' @description
    #' Evaluates allocation parameters by running simulations over historical
    #' periods and comparing results against observed data using fuzzy similarity.
    #' For each perturbation:
    #' 1. Runs `alloc_dinamica()` using historical periods to produce simulated final period
    #' 2. Compares initial vs final observed and initial vs final simulated using fuzzy similarity
    #' 3. Returns `alloc_params_t` augmented with per-transition similarity metrics
    #' @param id_perturbations Integer vector of perturbation IDs to evaluate.
    #'   If NULL (default), evaluates all perturbations in `alloc_params_t`.
    #' @param work_dir Character path for Dinamica working directory. Default "dinamica_rundir".
    #' @param keep_intermediate Logical, keep intermediate Dinamica files? Default FALSE.
    eval_alloc_params_t = function(
      id_perturbations = NULL,
      work_dir = "dinamica_rundir",
      keep_intermediate = FALSE
    ) {
      create_method_binding(eval_alloc_params_t)
    },

    #' @description
    #' Computes allocation parameters for all viable transitions, aggregated across
    #' observed periods and then randomly perturbed N times. This is a method
    #' on `evoland_db` that analyzes patch dynamics to determine expansion vs. patcher
    #' behavior for the Dinamica allocation procedure.
    #'
    #' The method first computes parameters for all viable transitions across observed
    #' periods (where `id_period > 1` and `is_extrapolated == FALSE`), then aggregates
    #' (mean) these parameters by transition. Finally, it creates randomly perturbed
    #' versions of these aggregated parameters and returns them together with the original
    #' estimate, i.e. the result set size is (n viable transitions) * (m perturbations + 1)
    #'
    #' @details
    #' The workflow is:
    #' 1. For each transition and period pair:
    #'    - Create rasters for the anterior and posterior periods
    #'    - Identify transition cells (cells that changed from anterior to posterior class)
    #'    - Use focal operations to determine if transition cells are adjacent to existing
    #'      patches (expansion) or form new patches (patcher behavior)
    #'    - Compute patch statistics using internal C++ implementation
    #' 2. Aggregate parameters across periods (mean) for each transition
    #' 3. For each transition, create N randomly perturbed versions:
    #'    - Add random noise to frac_expander (normal distribution, mean=0, sd=sd)
    #'    - Clamp frac_expander to \[0, 1\]
    #'    - Recalculate frac_patcher as 1 - frac_expander
    #' 4. Store all perturbed versions in the `alloc_params_t` table
    #'
    #' @param n_perturbations Integer number of perturbed parameter sets to generate
    #'   per transition (default: 5)
    #' @param sd Standard deviation for random perturbation of frac_expander as a
    #'   fraction (default: 0.05)
    create_alloc_params_t = function(n_perturbations = 5L, sd = 0.05) {
      create_method_binding(create_alloc_params_t)
    },

    #' @description Retrieve LULC data as a SpatRaster object for a given
    #' period. See [lulc_data_as_rast()]
    #' @param id_period Optional integer vector of period IDs to include. If
    #'   NULL (default), all periods are included.
    lulc_data_as_rast = function(extent = NULL, id_period = NULL) {
      create_method_binding(lulc_data_as_rast)
    },

    #' @description
    #' Fit full models on complete data using the best partial model
    #' configuration for each transition.
    #' @param partial_models A trans_models_t table with partial models (from fit_partial_models)
    #' @param gof_criterion Which goodness-of-fit metric to use for model selection (e.g., "auc")
    #' @param maximize Logical. If TRUE, higher values of gof_criterion are better. If FALSE, lower
    #' is better.
    #' @param na_value Passed to db$trans_pred_data_v
    #' @param envir Environment in which to evaluate fit_call. Defaults to parent frame.
    fit_full_models = function(
      partial_models,
      gof_criterion,
      maximize = TRUE,
      na_value = NA,
      envir = parent.frame(),
      cluster = NULL
    ) {
      create_method_binding(fit_full_models)
    },

    #' @description Fit partial models for each viable transition using stratified
    #'    sampling. Models are trained on a subsample and evaluated on held-out data.
    #' @param fit_fun Function to fit models. Should accept (data, ...), where data
    #'    is a dataframe-like object with columns "result" a boolean, and predictor
    #'    columns labelled `id_pred_[0-9]+`. The function should return a fitted model
    #'    object that can be used with the [stats::predict()] generic. For GLMs,
    #'    quasibinomial family is recommended. If the butcher package is available,
    #'    consider using butcher::butcher() on the model before returning to reduce
    #'    memory footprint. The `model_family` attribute is returned.
    #' @param gof_fun Function to evaluate goodness of fit. Should accept (model,
    #'    test_data) and return a named list of metrics. `test_data` is of the same
    #'    format as the fit_fun `data` argument.
    #' @param sample_frac Fraction in \(0, 1\) for stratified sampling. This percentage of
    #'    each result group (TRUE/FALSE) will be used for training, the rest for
    #'    validation.
    #' @param seed Random seed for reproducible sampling
    #' @param na_value Passed to db$trans_pred_data_v - if not NA, replace all NA
    #'    predictor values with this value
    #' @param ... additional arguments passed to fit_fun
    fit_partial_models = function(
      fit_fun,
      sample_frac = 0.7,
      gof_fun,
      seed = NULL,
      na_value = NA,
      cluster = NULL,
      ...
    ) {
      create_method_binding(fit_partial_models)
    },

    #' @description
    #' Set an initial full set of transition / predictor relations
    #' @param overwrite Logical, whether to overwrite existing `trans_preds_t` table. Default FALSE.
    set_full_trans_preds = function(overwrite = FALSE) {
      create_method_binding(set_full_trans_preds)
    },

    #' @description
    #' Create a transition-predictor relation, i.e. records the
    #' result of a predictor selection step. Runs covariance filtering for each viable
    #' transition and stores the selected predictors.
    #' @param filter_fun Defaults to [covariance_filter()], but can be any function that returns a
    #' structure like [as_trans_preds_t()]
    #' @param na_value Passed to db$trans_pred_data_v - if not NA, replace all NA predictor values
    #' with this value
    #' @param cores Integer, number of cores to use for parallel processing. Defaults to 1.
    #' @param ... Additional arguments passed to rank_fun via [covariance_filter()]
    get_pruned_trans_preds_t = function(
      filter_fun = covariance_filter,
      na_value = NA,
      cluster = NULL,
      ...
    ) {
      create_method_binding(get_pruned_trans_preds_t)
    },

    #' @description
    #' Predict the transition potential for a given period, see [trans_pot_t()]
    #' @param id_period_post Integerish, period for which to predict
    predict_trans_pot = function(id_period) {
      create_method_binding(predict_trans_pot)
    },

    #' @description Get the transition rates that were observed, see [trans_rates_t]
    get_obs_trans_rates = function() {
      create_method_binding(get_obs_trans_rates)
    }
  ),

  # Active Bindings ----
  active = list(
    #' @field Get or upsert [coords_t]
    coords_t = create_table_binding("coords_t", "write_once"),
    #' @field Get or upsert [periods_t]
    periods_t = create_table_binding("periods_t", "write_once"),
    #' @field Get or upsert [lulc_meta_t]
    lulc_meta_t = create_table_binding("lulc_meta_t", "upsert"),
    #' @field Get or upsert [lulc_data_t]
    lulc_data_t = create_table_binding("lulc_data_t", "append"),
    #' @field Get or upsert [pred_data_t]
    pred_data_t = create_table_binding("pred_data_t", "append"),
    #' @field Get or upsert [pred_meta_t]
    pred_meta_t = create_table_binding("pred_meta_t", "upsert"),
    #' @field Get or upsert [trans_meta_t]
    trans_meta_t = create_table_binding("trans_meta_t", "upsert"),
    #' @field Get or upsert [trans_preds_t]
    trans_preds_t = create_table_binding("trans_preds_t", "write_once"),
    #' @field Get or upsert [trans_rates_t]
    trans_rates_t = create_table_binding("trans_rates_t", "upsert"),
    #' @field Get or upsert [intrv_meta_t]
    intrv_meta_t = create_table_binding("intrv_meta_t", "upsert"),
    #' @field Get or upsert [intrv_masks_t]
    intrv_masks_t = create_table_binding("intrv_masks_t", "upsert"),
    #' @field Get or upsert [trans_models_t]
    trans_models_t = create_table_binding("trans_models_t", "upsert"),
    #' @field Get or upsert [alloc_params_t]
    alloc_params_t = create_table_binding("alloc_params_t", "upsert"),
    #' @field Get or upsert [neighbors_t]
    neighbors_t = create_table_binding("neighbors_t", "write_once"),
    #' @field Get or upsert [reporting_t]
    reporting_t = create_table_binding("reporting_t", "upsert"),
    #' @field Get or upsert [runs_t]
    runs_t = create_table_binding("runs_t", "upsert")
  ),

  private = list(
    active_id_run = NULL,
    active_id_run_lineage = 0L
  )
)
