# `evoland_db` - R6 Class for lulc data management

An R6 class that provides an interface to a folder-based data storage
system for the evoland package. Each table is stored as a parquet (or
JSON) file. This class uses DuckDB for in-memory SQL operations while
persisting data to disk in parquet format for better compression.

Inherits from
[parquet_db](https://ethzplus.github.io/evoland-plus/reference/parquet_db.md)
for generic database operations.

## See also

Additional methods and active bindings are added to this class in
separate files:

- [evoland_db_views](https://ethzplus.github.io/evoland-plus/reference/evoland_db_views.md) -
  View active bindings (lulc_meta_long_v, etc.) and methods

## Super class

[`evoland::parquet_db`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.md)
-\> `evoland_db`

## Active bindings

- `coords_t`:

  Get or upsert
  [coords_t](https://ethzplus.github.io/evoland-plus/reference/coords_t.md)

- `periods_t`:

  Get or upsert
  [periods_t](https://ethzplus.github.io/evoland-plus/reference/periods_t.md)

- `lulc_meta_t`:

  Get or upsert
  [lulc_meta_t](https://ethzplus.github.io/evoland-plus/reference/lulc_meta_t.md)

- `lulc_data_t`:

  Get or upsert
  [lulc_data_t](https://ethzplus.github.io/evoland-plus/reference/lulc_data_t.md)

- `pred_data_t`:

  Get or upsert
  [pred_data_t](https://ethzplus.github.io/evoland-plus/reference/pred_data_t.md)

- `pred_meta_t`:

  Get or upsert
  [pred_meta_t](https://ethzplus.github.io/evoland-plus/reference/pred_meta_t.md)

- `trans_meta_t`:

  Get or upsert
  [trans_meta_t](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md)

- `trans_preds_t`:

  Get or upsert
  [trans_preds_t](https://ethzplus.github.io/evoland-plus/reference/trans_preds_t.md)

- `trans_rates_t`:

  Get or upsert
  [trans_rates_t](https://ethzplus.github.io/evoland-plus/reference/trans_rates_t.md)

- `intrv_meta_t`:

  Get or upsert
  [intrv_meta_t](https://ethzplus.github.io/evoland-plus/reference/intrv_meta_t.md)

- `intrv_masks_t`:

  Get or upsert
  [intrv_masks_t](https://ethzplus.github.io/evoland-plus/reference/intrv_masks_t.md)

- `trans_models_t`:

  Get or upsert
  [trans_models_t](https://ethzplus.github.io/evoland-plus/reference/trans_models_t.md)

- `alloc_params_t`:

  Get or upsert
  [alloc_params_t](https://ethzplus.github.io/evoland-plus/reference/alloc_params_t.md)

- `neighbors_t`:

  Get or upsert
  [neighbors_t](https://ethzplus.github.io/evoland-plus/reference/neighbors_t.md)

- `reporting_t`:

  Get or upsert
  [reporting_t](https://ethzplus.github.io/evoland-plus/reference/reporting_t.md)

- `runs_t`:

  Get or upsert
  [runs_t](https://ethzplus.github.io/evoland-plus/reference/runs_t.md)

- `id_run`:

  Get or set active id_run, see
  [runs_t](https://ethzplus.github.io/evoland-plus/reference/runs_t.md)

- `run_lineage`:

  Get id_run, see
  [runs_t](https://ethzplus.github.io/evoland-plus/reference/runs_t.md)

## Methods

### Public methods

- [`evoland_db$trans_rates_dinamica_v()`](#method-evoland_db-trans_rates_dinamica_v)

- [`evoland_db$new()`](#method-evoland_db-new)

- [`evoland_db$get_read_expr()`](#method-evoland_db-get_read_expr)

- [`evoland_db$print()`](#method-evoland_db-print)

- [`evoland_db$set_report()`](#method-evoland_db-set_report)

- [`evoland_db$set_neighbors()`](#method-evoland_db-set_neighbors)

- [`evoland_db$generate_neighbor_predictors()`](#method-evoland_db-generate_neighbor_predictors)

- [`evoland_db$upsert_new_neighbors()`](#method-evoland_db-upsert_new_neighbors)

- [`evoland_db$trans_pred_data_v()`](#method-evoland_db-trans_pred_data_v)

- [`evoland_db$pred_data_wide_v()`](#method-evoland_db-pred_data_wide_v)

- [`evoland_db$alloc_dinamica()`](#method-evoland_db-alloc_dinamica)

- [`evoland_db$eval_alloc_params_t()`](#method-evoland_db-eval_alloc_params_t)

- [`evoland_db$create_alloc_params_t()`](#method-evoland_db-create_alloc_params_t)

- [`evoland_db$lulc_data_as_rast()`](#method-evoland_db-lulc_data_as_rast)

- [`evoland_db$fit_full_models()`](#method-evoland_db-fit_full_models)

- [`evoland_db$fit_partial_models()`](#method-evoland_db-fit_partial_models)

- [`evoland_db$set_full_trans_preds()`](#method-evoland_db-set_full_trans_preds)

- [`evoland_db$get_pruned_trans_preds_t()`](#method-evoland_db-get_pruned_trans_preds_t)

- [`evoland_db$predict_trans_pot()`](#method-evoland_db-predict_trans_pot)

- [`evoland_db$get_obs_trans_rates()`](#method-evoland_db-get_obs_trans_rates)

- [`evoland_db$clone()`](#method-evoland_db-clone)

Inherited methods

- [`evoland::parquet_db$column_max()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-column_max)
- [`evoland::parquet_db$commit()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-commit)
- [`evoland::parquet_db$delete_from()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-delete_from)
- [`evoland::parquet_db$execute()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-execute)
- [`evoland::parquet_db$fetch()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-fetch)
- [`evoland::parquet_db$get_query()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-get_query)
- [`evoland::parquet_db$get_table_metadata()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-get_table_metadata)
- [`evoland::parquet_db$get_table_path()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-get_table_path)
- [`evoland::parquet_db$list_tables()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-list_tables)
- [`evoland::parquet_db$row_count()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-row_count)

------------------------------------------------------------------------

### Method [`trans_rates_dinamica_v()`](https://ethzplus.github.io/evoland-plus/reference/evoland_db_views.md)

#### Usage

    evoland_db$trans_rates_dinamica_v(id_period)

------------------------------------------------------------------------

### Method `new()`

Initialize a new evoland_db object

#### Usage

    evoland_db$new(path, id_run = 0L, read_only = FALSE, ...)

#### Arguments

- `path`:

  Character string. Path to the data folder.

- `id_run`:

  Atomic integer run ID, defaults to 0. Can be set to NULL

- `read_only`:

  Logical. Whether to update the reporting table upon initialization; if
  TRUE, only parallel-safe appends are allowed.

- `...`:

  passed on to `set_report`

#### Returns

A new `evoland_db` object

------------------------------------------------------------------------

### Method `get_read_expr()`

Get SQL expression to read a table, respecting active run hierarchy. For
each data slice, returns data from the closest ancestor run that has it.

#### Usage

    evoland_db$get_read_expr(table_name)

#### Arguments

- `table_name`:

  Character string table name

#### Returns

Character string SQL expression (a composable subquery)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for evoland_db

#### Usage

    evoland_db$print(...)

#### Arguments

- `...`:

  passed to super\$print

------------------------------------------------------------------------

### Method `set_report()`

Set reporting metadata, see
[`db_set_report()`](https://ethzplus.github.io/evoland-plus/reference/reporting_t.md)

#### Usage

    evoland_db$set_report(...)

#### Arguments

- `...`:

  each named argument is entered into the table with the argument name
  as its key

------------------------------------------------------------------------

### Method [`set_neighbors()`](https://ethzplus.github.io/evoland-plus/reference/neighbors_t.md)

Set the neighbors table, see
[`set_neighbors()`](https://ethzplus.github.io/evoland-plus/reference/neighbors_t.md)

#### Usage

    evoland_db$set_neighbors(
      max_distance = 1000,
      distance_breaks = NULL,
      overwrite = FALSE,
      quiet = FALSE,
      chunksize = 1e+08
    )

#### Arguments

- `max_distance`:

  Maximum distance

- `distance_breaks`:

  Numeric vector of distance breaks

- `overwrite`:

  Logical, whether to overwrite existing neighbors_t table

- `quiet`:

  Logical, whether to suppress messages about progress

- `chunksize`:

  Integer, chunksize for writing; keeps down memory pressure

------------------------------------------------------------------------

### Method [`generate_neighbor_predictors()`](https://ethzplus.github.io/evoland-plus/reference/neighbors_t.md)

Generate neighbor prediction table, i.e. "how many neighbors within
distance X are of type Y", see
[`generate_neighbor_predictors()`](https://ethzplus.github.io/evoland-plus/reference/neighbors_t.md)

#### Usage

    evoland_db$generate_neighbor_predictors()

------------------------------------------------------------------------

### Method [`upsert_new_neighbors()`](https://ethzplus.github.io/evoland-plus/reference/neighbors_t.md)

Append new neighbors to predictor for a given period; depends on
generate_neighbor_predictors() having been run.

#### Usage

    evoland_db$upsert_new_neighbors(id_period)

#### Arguments

- `id_period`:

  Period to calculate predictors for

------------------------------------------------------------------------

### Method `trans_pred_data_v()`

Get transitions along with their predictor data in a wide data.table,
see `trans_pred_data_v()`

#### Usage

    evoland_db$trans_pred_data_v(id_trans, id_pred, ordered = FALSE)

#### Arguments

- `id_trans`:

  Integer transition ID, see
  [trans_meta_t](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md)

- `id_pred`:

  Optional integer vector of predictor IDs to include

- `ordered`:

  - if TRUE, order output by `id_coord` & `id_period`

------------------------------------------------------------------------

### Method [`pred_data_wide_v()`](https://ethzplus.github.io/evoland-plus/reference/pred_data_t.md)

Retrieve a wide view of the predictor data, see
[`pred_data_wide_v()`](https://ethzplus.github.io/evoland-plus/reference/pred_data_t.md)

#### Usage

    evoland_db$pred_data_wide_v(id_trans, id_period_anterior)

#### Arguments

- `id_trans`:

  Integer transition ID, see
  [trans_meta_t](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md)

- `id_period_anterior`:

  Integer ID of period to retrieve data for

------------------------------------------------------------------------

### Method [`alloc_dinamica()`](https://ethzplus.github.io/evoland-plus/reference/alloc_dinamica.md)

Runs a path-dependent Monte Carlo simulation using Dinamica EGO, see
[`alloc_dinamica()`](https://ethzplus.github.io/evoland-plus/reference/alloc_dinamica.md)

#### Usage

    evoland_db$alloc_dinamica(
      id_periods,
      gof_criterion,
      gof_maximize,
      work_dir = "dinamica_rundir",
      keep_intermediate = FALSE
    )

#### Arguments

- `id_periods`:

  Integer vector of period IDs to include in the simulation.

- `gof_criterion`:

  Which goodness-of-fit metric to use for model selection (e.g., "auc")

- `gof_maximize`:

  Maximize (TRUE) or minimize (FALSE) the gof_criterion?

- `work_dir`:

  Character path for Dinamica working directory. Default
  "dinamica_rundir".

- `keep_intermediate`:

  Logical, keep intermediate Dinamica files? Default FALSE.

------------------------------------------------------------------------

### Method [`eval_alloc_params_t()`](https://ethzplus.github.io/evoland-plus/reference/alloc_dinamica.md)

Evaluates allocation parameters in dinamica, see
[`eval_alloc_params_t()`](https://ethzplus.github.io/evoland-plus/reference/alloc_dinamica.md)

#### Usage

    evoland_db$eval_alloc_params_t(
      gof_criterion,
      gof_maximize,
      work_dir = "dinamica_rundir",
      keep_intermediate = FALSE
    )

#### Arguments

- `gof_criterion`:

  Which goodness-of-fit metric to use for model selection (e.g., "auc")

- `gof_maximize`:

  Maximize (TRUE) or minimize (FALSE) the gof_criterion?

- `work_dir`:

  Character path for Dinamica working directory. Default
  "dinamica_rundir".

- `keep_intermediate`:

  Logical, keep intermediate Dinamica files? Default FALSE.

------------------------------------------------------------------------

### Method [`create_alloc_params_t()`](https://ethzplus.github.io/evoland-plus/reference/alloc_params_t.md)

Computes allocation parameters for all viable transitions, see
[`create_alloc_params_t()`](https://ethzplus.github.io/evoland-plus/reference/alloc_params_t.md)

#### Usage

    evoland_db$create_alloc_params_t(n_perturbations = 5L, sd = 0.05)

#### Arguments

- `n_perturbations`:

  Number of perturbed parameter sets to generate, each assigned to a in
  id_run in
  [runs_t](https://ethzplus.github.io/evoland-plus/reference/runs_t.md)

- `sd`:

  Standard deviation for random perturbation of frac_expander

------------------------------------------------------------------------

### Method [`lulc_data_as_rast()`](https://ethzplus.github.io/evoland-plus/reference/lulc_data_t.md)

Retrieve LULC data as a SpatRaster object for a given period. See
[`lulc_data_as_rast()`](https://ethzplus.github.io/evoland-plus/reference/lulc_data_t.md)

#### Usage

    evoland_db$lulc_data_as_rast(id_period = NULL)

#### Arguments

- `id_period`:

  Optional integer vector of period IDs to include. If NULL (default),
  all periods are included.

------------------------------------------------------------------------

### Method [`fit_full_models()`](https://ethzplus.github.io/evoland-plus/reference/trans_models_t.md)

Fit full models on complete data using the best partial model
configuration for each transition, see
[`fit_full_models()`](https://ethzplus.github.io/evoland-plus/reference/trans_models_t.md)

#### Usage

    evoland_db$fit_full_models(
      partial_models,
      gof_criterion,
      gof_maximize,
      cluster = NULL
    )

#### Arguments

- `partial_models`:

  A trans_models_t table with partial models (see
  [`fit_partial_models()`](https://ethzplus.github.io/evoland-plus/reference/trans_models_t.md))

- `gof_criterion`:

  Which goodness-of-fit metric to use for model selection (e.g., "auc")

- `gof_maximize`:

  Maximize (TRUE) or minimize (FALSE) the gof_criterion?

- `cluster`:

  Optional cluster object for parallel processing

------------------------------------------------------------------------

### Method [`fit_partial_models()`](https://ethzplus.github.io/evoland-plus/reference/trans_models_t.md)

Fit partial models for each viable transition using stratified sampling.
Models are trained on a subsample and evaluated on held-out data, see
[`fit_partial_models()`](https://ethzplus.github.io/evoland-plus/reference/trans_models_t.md)
for details.

#### Usage

    evoland_db$fit_partial_models(
      fit_fun,
      sample_frac = 0.7,
      gof_fun,
      seed = NULL,
      cluster = NULL,
      ...
    )

#### Arguments

- `fit_fun`:

  Function for generating a model object.

- `sample_frac`:

  Fraction in \\0, 1\\ for stratified sampling.

- `gof_fun`:

  Function to evaluate goodness of fit.

- `seed`:

  Random seed for reproducible sampling

- `cluster`:

  Optional cluster object for parallel processing

- `...`:

  additional arguments passed to fit_fun

------------------------------------------------------------------------

### Method [`set_full_trans_preds()`](https://ethzplus.github.io/evoland-plus/reference/trans_preds_t.md)

Set an initial full set of transition / predictor relations, see
[`set_full_trans_preds()`](https://ethzplus.github.io/evoland-plus/reference/trans_preds_t.md)

#### Usage

    evoland_db$set_full_trans_preds(overwrite = FALSE)

#### Arguments

- `overwrite`:

  Logical, whether to overwrite existing `trans_preds_t` table. Default
  FALSE.

------------------------------------------------------------------------

### Method [`get_pruned_trans_preds_t()`](https://ethzplus.github.io/evoland-plus/reference/trans_preds_t.md)

Remove predictors from the transition-predictor relation, aka feature
selection. See
[`get_pruned_trans_preds_t()`](https://ethzplus.github.io/evoland-plus/reference/trans_preds_t.md).

#### Usage

    evoland_db$get_pruned_trans_preds_t(
      filter_fun = covariance_filter,
      cluster = NULL,
      ...
    )

#### Arguments

- `filter_fun`:

  Defaults to
  [`covariance_filter()`](https://ethzplus.github.io/evoland-plus/reference/covariance_filter.md),
  see
  [`get_pruned_trans_preds_t()`](https://ethzplus.github.io/evoland-plus/reference/trans_preds_t.md)
  for details.

- `cluster`:

  Optional cluster object for parallel processing

- `...`:

  Additional arguments passed to `filter_fun`.

------------------------------------------------------------------------

### Method [`predict_trans_pot()`](https://ethzplus.github.io/evoland-plus/reference/trans_pot_t.md)

Predict the transition potential for a given period, see
[`trans_pot_t()`](https://ethzplus.github.io/evoland-plus/reference/trans_pot_t.md)

#### Usage

    evoland_db$predict_trans_pot(id_period_post, gof_criterion, gof_maximize)

#### Arguments

- `id_period_post`:

  Integerish, posterior period of the transition potential interval

- `gof_criterion`:

  Which goodness-of-fit metric to use for model selection (e.g., "auc")

- `gof_maximize`:

  Maximize (TRUE) or minimize (FALSE) the gof_criterion?

------------------------------------------------------------------------

### Method [`get_obs_trans_rates()`](https://ethzplus.github.io/evoland-plus/reference/trans_rates_t.md)

Get the transition rates that were observed, see
[trans_rates_t](https://ethzplus.github.io/evoland-plus/reference/trans_rates_t.md)

#### Usage

    evoland_db$get_obs_trans_rates()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    evoland_db$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
