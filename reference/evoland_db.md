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

[`parquet_db`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.md)
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

- `trans_pot_t`:

  Get or upsert raw transition potentials
  [trans_pot_t](https://ethzplus.github.io/evoland-plus/reference/trans_pot_t.md).
  These are per-transition model probabilities stored by
  [`predict_trans_pot()`](https://ethzplus.github.io/evoland-plus/reference/trans_pot_t.md).
  Use
  [`adjusted_trans_pot_v()`](https://ethzplus.github.io/evoland-plus/reference/evoland_db_views.md)
  for allocation-ready values.

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

- [`evoland_db$adjusted_trans_pot_v()`](#method-evoland_db-adjusted_trans_pot_v)

- [`evoland_db$alloc_params_clumpy_v()`](#method-evoland_db-alloc_params_clumpy_v)

- [`evoland_db$new()`](#method-evoland_db-initialize)

- [`evoland_db$get_read_expr()`](#method-evoland_db-get_read_expr)

- [`evoland_db$print()`](#method-evoland_db-print)

- [`evoland_db$set_report()`](#method-evoland_db-set_report)

- [`evoland_db$set_neighbors()`](#method-evoland_db-set_neighbors)

- [`evoland_db$generate_neighbor_predictors()`](#method-evoland_db-generate_neighbor_predictors)

- [`evoland_db$upsert_new_neighbors()`](#method-evoland_db-upsert_new_neighbors)

- [`evoland_db$add_predictor()`](#method-evoland_db-add_predictor)

- [`evoland_db$trans_pred_data_v()`](#method-evoland_db-trans_pred_data_v)

- [`evoland_db$pred_data_wide_v()`](#method-evoland_db-pred_data_wide_v)

- [`evoland_db$alloc_dinamica()`](#method-evoland_db-alloc_dinamica)

- [`evoland_db$alloc_clumpy()`](#method-evoland_db-alloc_clumpy)

- [`evoland_db$eval_alloc_params_t()`](#method-evoland_db-eval_alloc_params_t)

- [`evoland_db$create_alloc_params_t()`](#method-evoland_db-create_alloc_params_t)

- [`evoland_db$lulc_data_as_rast()`](#method-evoland_db-lulc_data_as_rast)

- [`evoland_db$fit_full_models()`](#method-evoland_db-fit_full_models)

- [`evoland_db$fit_partial_models()`](#method-evoland_db-fit_partial_models)

- [`evoland_db$get_crossval_plots()`](#method-evoland_db-get_crossval_plots)

- [`evoland_db$set_full_trans_preds()`](#method-evoland_db-set_full_trans_preds)

- [`evoland_db$get_pred_filter_score()`](#method-evoland_db-get_pred_filter_score)

- [`evoland_db$predict_trans_pot()`](#method-evoland_db-predict_trans_pot)

- [`evoland_db$get_obs_trans_rates()`](#method-evoland_db-get_obs_trans_rates)

- [`evoland_db$clone()`](#method-evoland_db-clone)

Inherited methods

- [`parquet_db$column_max()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-column_max)
- [`parquet_db$commit()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-commit)
- [`parquet_db$delete_from()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-delete_from)
- [`parquet_db$execute()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-execute)
- [`parquet_db$fetch()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-fetch)
- [`parquet_db$get_query()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-get_query)
- [`parquet_db$get_table_metadata()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-get_table_metadata)
- [`parquet_db$get_table_path()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-get_table_path)
- [`parquet_db$list_tables()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-list_tables)
- [`parquet_db$row_count()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-row_count)

------------------------------------------------------------------------

### `evoland_db$trans_rates_dinamica_v()`

#### Usage

    evoland_db$trans_rates_dinamica_v(id_period)

------------------------------------------------------------------------

### `evoland_db$adjusted_trans_pot_v()`

#### Usage

    evoland_db$adjusted_trans_pot_v(id_period_post)

------------------------------------------------------------------------

### `evoland_db$alloc_params_clumpy_v()`

#### Usage

    evoland_db$alloc_params_clumpy_v()

------------------------------------------------------------------------

### `evoland_db$new()`

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

### `evoland_db$get_read_expr()`

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

### `evoland_db$print()`

Print method for evoland_db

#### Usage

    evoland_db$print(...)

#### Arguments

- `...`:

  passed to super\$print

------------------------------------------------------------------------

### `evoland_db$set_report()`

Set reporting metadata, see
[`db_set_report()`](https://ethzplus.github.io/evoland-plus/reference/reporting_t.md)

#### Usage

    evoland_db$set_report(...)

#### Arguments

- `...`:

  each named argument is entered into the table with the argument name
  as its key

------------------------------------------------------------------------

### `evoland_db$set_neighbors()`

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

### `evoland_db$generate_neighbor_predictors()`

Generate neighbor prediction table, i.e. "how many neighbors within
distance X are of type Y", see
[`generate_neighbor_predictors()`](https://ethzplus.github.io/evoland-plus/reference/neighbors_t.md)

#### Usage

    evoland_db$generate_neighbor_predictors()

------------------------------------------------------------------------

### `evoland_db$upsert_new_neighbors()`

Append new neighbors to predictor for a given period; depends on
generate_neighbor_predictors() having been run.

#### Usage

    evoland_db$upsert_new_neighbors(id_period)

#### Arguments

- `id_period`:

  Period to calculate predictors for

------------------------------------------------------------------------

### `evoland_db$add_predictor()`

Add a predictor using the currently active id_run, see
[`add_predictor()`](https://ethzplus.github.io/evoland-plus/reference/pred_data_t.md)

#### Usage

    evoland_db$add_predictor(
      pred_data_raw,
      name,
      fill_value,
      pretty_name = name,
      description = NA_character_,
      orig_format = NA_character_,
      sources = data.frame(url = character(0), md5sum = character(0)),
      unit = NA_character_
    )

#### Arguments

- `pred_data_raw`:

  Data frame with columns `id_coord`, `id_period`, and `value`

- `name`:

  Unique short name

- `fill_value`:

  Value to substitute if a coordinate point in
  [coords_t](https://ethzplus.github.io/evoland-plus/reference/coords_t.md)
  does not have an explicit associated value

- `pretty_name`:

  char, Friendly name for use in reporting

- `description`:

  char, For use in reporting.

- `orig_format`:

  char, Format description of the underlying data (raster, vector…)

- `sources`:

  data.frame with url/md5sum columns, used for keeping track of
  underlying raw data, see
  [`download_and_verify()`](https://ethzplus.github.io/evoland-plus/reference/util_download.md)

- `unit`:

  char, SI unit for physical predictors, or descriptions like "bed
  nights/year" as a proxy for touristic activity

------------------------------------------------------------------------

### `evoland_db$trans_pred_data_v()`

Get transitions along with their predictor data in a wide data.table,
see
[`trans_pred_data_v()`](https://ethzplus.github.io/evoland-plus/reference/pred_data_t.md)

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

### `evoland_db$pred_data_wide_v()`

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

### `evoland_db$alloc_dinamica()`

Runs a path-dependent Monte Carlo simulation using Dinamica EGO, see
[`alloc_dinamica()`](https://ethzplus.github.io/evoland-plus/reference/alloc_dinamica.md)

#### Usage

    evoland_db$alloc_dinamica(
      id_periods,
      select_score,
      select_maximize,
      work_dir = "dinamica_rundir",
      keep_intermediate = FALSE
    )

#### Arguments

- `id_periods`:

  Integer vector of period IDs to include in the simulation.

- `select_score`:

  Character string; mlr3 measure ID (e.g. `"classif.auc"`) used to
  select model for extrapolation

- `select_maximize`:

  Logical; maximize (`TRUE`) or minimize (`FALSE`) the score.

- `work_dir`:

  Character path for Dinamica working directory. Default
  "dinamica_rundir".

- `keep_intermediate`:

  Logical, keep intermediate Dinamica files? Default FALSE.

------------------------------------------------------------------------

### `evoland_db$alloc_clumpy()`

Runs CLUMPY-style LULC allocation, see
[`alloc_clumpy()`](https://ethzplus.github.io/evoland-plus/reference/alloc_clumpy.md).
The method (uSAM vs uPAM) is selected automatically from the patch
parameters: mono-pixel patches (`area_mean == 1`, `area_var == 0`) use
uSAM, otherwise uPAM.

#### Usage

    evoland_db$alloc_clumpy(
      id_periods,
      select_score,
      select_maximize,
      area_dist = "lognormal",
      avoid_aggregation = TRUE,
      batch_size = 0L,
      seed = NULL
    )

#### Arguments

- `id_periods`:

  Integer vector of period IDs to include in the simulation.

- `select_score`:

  Character string; mlr3 measure ID (e.g. `"classif.auc"`) used to
  select model for extrapolation.

- `select_maximize`:

  Logical; maximize (`TRUE`) or minimize (`FALSE`) the score.

- `area_dist`:

  Character; patch-area distribution, `"lognormal"` (default) or
  `"normal"`. See
  [`alloc_clumpy()`](https://ethzplus.github.io/evoland-plus/reference/alloc_clumpy.md).

- `avoid_aggregation`:

  Logical; if `TRUE` (default) uPAM patches that would merge fail and
  allocate nothing. Ignored for uSAM.

- `batch_size`:

  Integer; uPAM pivots attempted per MuST re-draw. `0` (default)
  auto-scales with the source pool; `> 0` is an explicit cap (`1` =
  strict uPAM); `< 0` = all candidates in one pass. Ignored for uSAM.

- `seed`:

  Optional integer random seed for reproducibility.

------------------------------------------------------------------------

### `evoland_db$eval_alloc_params_t()`

Evaluates allocation parameters in dinamica, see
[`eval_alloc_params_t()`](https://ethzplus.github.io/evoland-plus/reference/alloc_dinamica.md)

#### Usage

    evoland_db$eval_alloc_params_t(
      select_score,
      select_maximize,
      work_dir = "dinamica_rundir",
      keep_intermediate = FALSE
    )

#### Arguments

- `select_score`:

  Character string; mlr3 measure ID (e.g. `"classif.auc"`) used to
  select model for extrapolation

- `select_maximize`:

  Logical; maximize (`TRUE`) or minimize (`FALSE`) the score.

- `work_dir`:

  Character path for Dinamica working directory. Default
  "dinamica_rundir".

- `keep_intermediate`:

  Logical, keep intermediate Dinamica files? Default FALSE.

------------------------------------------------------------------------

### `evoland_db$create_alloc_params_t()`

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

### `evoland_db$lulc_data_as_rast()`

Retrieve LULC data as a SpatRaster object for a given period. See
[`lulc_data_as_rast()`](https://ethzplus.github.io/evoland-plus/reference/lulc_data_t.md)

#### Usage

    evoland_db$lulc_data_as_rast(id_period = NULL)

#### Arguments

- `id_period`:

  Optional integer vector of period IDs to include. If NULL (default),
  all periods are included.

------------------------------------------------------------------------

### `evoland_db$fit_full_models()`

Fit full models (trained on the complete dataset) for each viable
transition, see
[`fit_full_models()`](https://ethzplus.github.io/evoland-plus/reference/trans_models_t.md).
Two mutually exclusive modes: pass `learner` to train directly, or pass
`select_score` to pick the best partial model by score.

#### Usage

    evoland_db$fit_full_models(
      learner = NULL,
      select_score = NULL,
      select_maximize = TRUE,
      cluster = NULL,
      trans_preds = NULL,
      trans_meta = NULL
    )

#### Arguments

- `learner`:

  An mlr3 `Learner` or `AutoTuner` for direct-learner mode (`NULL` when
  `select_score` is used).

- `select_score`:

  Measure ID string for score-select mode, e.g. `"classif.auc"` (`NULL`
  when `learner` is used).

- `select_maximize`:

  Logical; maximize (`TRUE`) or minimize (`FALSE`) the score.

- `cluster`:

  Optional cluster object for parallel processing

- `trans_preds`:

  Optional
  [trans_preds_t](https://ethzplus.github.io/evoland-plus/reference/trans_preds_t.md);
  bypasses the database read in direct-learner mode. Ignored in
  score-select mode.

- `trans_meta`:

  Optional
  [trans_meta_t](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md);
  bypasses the database read in direct-learner mode. Ignored in
  score-select mode.

------------------------------------------------------------------------

### `evoland_db$fit_partial_models()`

Fit partial models for each viable transition using stratified sampling.
Models are trained on a subsample and evaluated on held-out data, see
[`fit_partial_models()`](https://ethzplus.github.io/evoland-plus/reference/trans_models_t.md)
for details.

#### Usage

    evoland_db$fit_partial_models(
      learner,
      measures,
      sample_frac = 0.7,
      seed = NULL,
      cluster = NULL,
      trans_preds = NULL,
      trans_meta = NULL
    )

#### Arguments

- `learner`:

  An mlr3 `Learner` or `AutoTuner` R6 object.

- `measures`:

  A vector of `Measure` names passed to
  [mlr3::msr](https://mlr3.mlr-org.com/reference/mlr_sugar.html) or a
  list of `Measure` objects for scoring the held-out split.

- `sample_frac`:

  Fraction in \\0, 1\\ for stratified sampling.

- `seed`:

  Random seed for reproducible sampling

- `cluster`:

  Optional cluster object for parallel processing

- `trans_preds`:

  Optional
  [trans_preds_t](https://ethzplus.github.io/evoland-plus/reference/trans_preds_t.md);
  bypasses the `trans_preds_t` database read.

- `trans_meta`:

  Optional
  [trans_meta_t](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md);
  bypasses the `trans_meta_t` database read.

------------------------------------------------------------------------

### `evoland_db$get_crossval_plots()`

Get cross-validation plots for stored predictions, see
[`get_crossval_plots()`](https://ethzplus.github.io/evoland-plus/reference/trans_models_t.md)

#### Usage

    evoland_db$get_crossval_plots(id_run = NULL, id_trans = NULL)

#### Arguments

- `id_run`:

  Optional integer; filter by run ID.

- `id_trans`:

  Optional integer; filter by transition ID.

------------------------------------------------------------------------

### `evoland_db$set_full_trans_preds()`

Set an initial full set of transition / predictor relations, see
[`set_full_trans_preds()`](https://ethzplus.github.io/evoland-plus/reference/trans_preds_t.md)

#### Usage

    evoland_db$set_full_trans_preds(overwrite = FALSE)

#### Arguments

- `overwrite`:

  Logical, whether to overwrite existing `trans_preds_t` table. Default
  FALSE.

------------------------------------------------------------------------

### `evoland_db$get_pred_filter_score()`

Add filter scores to predictors for each `id_run, id_trans`. See
[`get_pred_filter_score()`](https://ethzplus.github.io/evoland-plus/reference/trans_preds_t.md).

#### Usage

    evoland_db$get_pred_filter_score(
      filter = "correlation",
      trans_preds = NULL,
      cluster = NULL,
      ...
    )

#### Arguments

- `filter`:

  Character passed to
  [mlr3filters::flt](https://mlr3filters.mlr-org.com/reference/flt.html)
  or
  [mlr3filters::Filter](https://mlr3filters.mlr-org.com/reference/Filter.html)
  object specifying the filter method to use for feature selection.

- `trans_preds`:

  Optional
  [trans_preds_t](https://ethzplus.github.io/evoland-plus/reference/trans_preds_t.md);
  when supplied, it is used directly instead of reading from the
  database.

- `cluster`:

  Optional cluster object for parallel processing

- `...`:

  Additional arguments passed to `flt`.

------------------------------------------------------------------------

### `evoland_db$predict_trans_pot()`

Predict the raw transition potential for a given period and store in
`trans_pot_t`, see
[`predict_trans_pot()`](https://ethzplus.github.io/evoland-plus/reference/trans_pot_t.md).
Raw potentials are per-transition model probabilities (not yet
allocation-ready); use
[`adjusted_trans_pot_v()`](https://ethzplus.github.io/evoland-plus/reference/evoland_db_views.md)
to obtain column-scaled, row-closed values.

#### Usage

    evoland_db$predict_trans_pot(id_period_post, select_score, select_maximize)

#### Arguments

- `id_period_post`:

  Integerish, posterior period of the transition potential interval

- `select_score`:

  Character string; mlr3 measure ID (e.g. `"classif.auc"`) used to
  select model for extrapolation

- `select_maximize`:

  Logical; maximize (`TRUE`) or minimize (`FALSE`) the score.

------------------------------------------------------------------------

### `evoland_db$get_obs_trans_rates()`

Get the transition rates that were observed, see
[trans_rates_t](https://ethzplus.github.io/evoland-plus/reference/trans_rates_t.md)

#### Usage

    evoland_db$get_obs_trans_rates()

------------------------------------------------------------------------

### `evoland_db$clone()`

The objects of this class are cloneable with this method.

#### Usage

    evoland_db$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
