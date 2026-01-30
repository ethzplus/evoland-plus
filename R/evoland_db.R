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
#' - [evoland_db_tables] - Table active bindings (coords_t, lulc_data_t, etc.)
#' - [evoland_db_views] - View active bindings (lulc_meta_long_v, etc.) and methods
#' - [evoland_db_neighbors] - Neighbor analysis methods
#'
#' @include parquet_db.R alloc_dinamica.R lulc_data_t.R trans_preds_t.R trans_pot_t.R
#' @export

evoland_db <- R6::R6Class(
  classname = "evoland_db",
  inherit = parquet_db,

  ## Public Methods ----
  public = list(
    #' @description
    #' Initialize a new evoland_db object
    #' @param path Character string. Path to the data folder.
    #' @param update_reporting Logical. Whether to update the reporting table upon initialization.
    #' Defaults to TRUE. Set to FALSE for read-only workers to avoid lock contention.
    #' @param ... passed on to `set_report`
    #'
    #' @return A new `evoland_db` object
    initialize = function(
      path,
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

      invisible(self)
    },

    #' @description
    #' Set the active run for read operations
    #' @param id_run Integer run ID. Must exist in runs_t.
    #' @return Invisibly returns self
    set_active_run = function(id_run) {
      stopifnot(
        "id_run must be a single integer" = length(id_run) == 1L && is.numeric(id_run)
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
              parent_id_run
            from
              runs_t
            where
              id_run = {id_run}
            union all
            select
              r.id_run,
              r.parent_id_run
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

      private$active_run_id <- id_run
      private$run_lineage <- ids

      invisible(self)
    },

    #' @description
    #' Get the active run ID
    #' @return Integer run ID
    get_active_run = function() {
      private$active_run_id
    },

    ### Setter methods ----
    #' @description
    #' Set reporting metadata
    #' @param ... each named argument is entered into the table with the argument name
    #' as its key
    set_report = function(...) {
      bind_helper(db_set_report)
    },

    #' @description
    #' Set coordinates for DB. Cannot overwrite existing table (would mean cascading deletion)
    #' @param type string; which type of coordinates to set, see [coords_t]
    #' @param ... named arguments are passed to the appropriate coordinate creator function
    set_coords = function(type = c("square"), ...) {
      if (self$row_count("coords_t") > 0L) {
        warning("coords_t is not empty! Refusing to overwrite; start with fresh DB")
        return(invisible(NULL))
      }
      create_fun <- switch(
        type,
        square = create_coords_t_square,
        function(x) stop("Unsupported coordinate type specified.")
      )

      self$commit(create_fun(...), "coords_t", method = "overwrite")
    },

    #' @description
    #' Set periods for DB. See [`periods_t`]
    #' @param period_length_str ISO 8601 duration string specifying the length of each
    #' period (currently only accepting years, e.g., "P5Y" for 5 years)
    #' @param start_observed Start date of the observed data (YYYY-MM-DD)
    #' @param end_observed End date of the observed data (YYYY-MM-DD)
    #' @param end_extrapolated End date for extrapolation time range (YYYY-MM-DD)
    set_periods = function(
      period_length_str = "P10Y",
      start_observed = "1985-01-01",
      end_observed = "2020-01-01",
      end_extrapolated = "2060-01-01"
    ) {
      if (self$row_count("periods_t") > 0L) {
        warning("periods_t is not empty! Refusing to overwrite; start with fresh DB")
        return(invisible(NULL))
      }

      self$commit(
        do.call(create_periods_t, as.list(environment())),
        "periods_t"
      )
    },

    ### Adder methods ----
    #' @description
    #' Add a predictor to the database
    #' @param pred_spec List of predictor specification; see [create_pred_meta_t()]
    #' @param pred_data An object that can be coerced to [`pred_data_t`], but doesn't have an
    #' `id_pred`
    #' @param pred_type Passed to [as_pred_data_t()]; one of float, int, bool
    add_predictor = function(pred_spec, pred_data, pred_type) {
      stopifnot(length(pred_spec) == 1)

      self$pred_meta_t <- create_pred_meta_t(pred_spec)

      existing_meta <- self$fetch(
        "pred_meta_t",
        where = glue::glue("name = '{names(pred_spec)}'")
      )

      data.table::set(pred_data, j = "id_pred", value = as.integer(existing_meta[["id_pred"]]))
      data.table::setcolorder(pred_data, c("id_pred", "id_coord", "id_period", "value"))

      self$commit(
        as_pred_data_t(pred_data, pred_type),
        paste0("pred_data_t_", pred_type),
        method = "upsert"
      )
    },

    ### Allocation methods ---
    #' @description
    #' Runs a path-dependent Monte Carlo simulation using Dinamica EGO for land use
    #' allocation. Iterates through a sequence of contiguous periods, using the
    #' simulated output from one period as the input to the next. **Requirements**:
    #' - `trans_models_t` must have full models fitted
    #' - `alloc_params_t` must exist with specified `id_perturbation`
    #' - `periods_t` must contain all specified `id_periods`
    #' - Dinamica EGO must be installed and `DinamicaConsole` must be on PATH
    #'
    #' @param id_periods Integer vector of contiguous period IDs to simulate.
    #'   Must be in sequential order. The first period is used as the origin state.
    #' @param id_perturbation Integer, perturbation ID for selecting allocation
    #'   parameters from `alloc_params_t`
    #' @param work_dir Character, base directory for Dinamica runs. A subdirectory
    #'   will be created for this simulation. Default: "dinamica_rundir"
    #' @param keep_intermediate Logical, keep intermediate files after successful
    #'   completion? Default: FALSE
    alloc_dinamica = function(
      id_periods,
      id_perturbation,
      work_dir = "dinamica_rundir",
      keep_intermediate = FALSE
    ) {
      bind_helper(alloc_dinamica)
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
      bind_helper(eval_alloc_params_t)
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
      bind_helper(create_alloc_params_t)
    },

    #' @description
    #' Converts `lulc_data_t` to a terra `SpatRast` object by joining with `coords_t`
    #' and rasterizing the point data. Each period becomes a separate layer in the
    #' output raster.
    #'
    #' @param epsg Integer EPSG code for the coordinate reference system
    #' @param resolution Numeric scalar specifying the raster cell size (in units of the CRS)
    #' @param id_period Optional integer vector of period IDs to include. If NULL (default),
    #'   all periods are included.
    #'
    #' @return A terra `SpatRast` object with one layer per period. Layer names are
    #'   formatted as "period_X" where X is the period ID. Values are LULC class IDs.
    #'
    #' @details
    #' The method performs the following steps:
    #' If multiple points fall within a single raster cell, a warning is emitted and
    #' the first value is kept. This can happen when the resolution is too coarse
    #' relative to the point spacing, or when points are irregularly distributed.
    #'
    #' @examples
    #' \dontrun{
    #' db <- evoland_db$new("path/to/db")
    #' lulc_rast <- db$lulc_data_as_rast(epsg = 2056, resolution = 100)
    #' lulc_rast_subset <- db$lulc_data_as_rast(epsg = 2056, resolution = 100, id_period = c(1, 2))
    #' }
    #'
    #' @name lulc_data_as_rast
    lulc_data_as_rast = function(extent = NULL, resolution = NULL, id_period = NULL) {
      bind_helper(lulc_data_as_rast)
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
      cores = 1L
    ) {
      bind_helper(fit_full_models)
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
      cores = 1L,
      ...
    ) {
      bind_helper(fit_partial_models)
    },

    #' @description
    #' Set an initial full set of transition / predictor relations
    #' @param overwrite Logical, whether to overwrite existing `trans_preds_t` table. Default FALSE.
    set_full_trans_preds = function(overwrite = FALSE) {
      bind_helper(set_full_trans_preds)
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
      cores = 1L,
      ...
    ) {
      bind_helper(get_pruned_trans_preds_t)
    },

    #' @description
    #' Predict the transition potential for a given period, see [trans_pot_t()]
    #' @param id_period_post Integerish, period for which to predict
    predict_trans_pot = function(id_period) {
      bind_helper(predict_trans_pot)
    }
  ),

  private = list(
    active_run_id = 0L,
    run_lineage = 0L
  )
)


# Helper function to bind a function as a method to an R6 generator. Simply passes the
# R6 method's arguments as-is to the underlying "pure" function (that passes the R6
# object as the `self` parameter)
bind_helper <- function(fun) {
  # Capture the original call (e.g., obj$method(arg1 = 1)); expands ... arguments
  cl <- match.call(definition = sys.function(-1), call = sys.call(-1))

  # Modify 'obj$method' to 'fun' instead
  cl[[1]] <- fun

  # Inject 'self' as named arg. Use get() to retrieve R6 instance's self from calling env
  cl[["self"]] <- get("self", envir = parent.frame())

  # Evaluate in original environment. Preserves lazy evaluation of arguments.
  eval(cl, envir = parent.frame(2))
}
