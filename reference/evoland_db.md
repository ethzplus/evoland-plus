# R6 Class for Folder-Based Data Storage Interface

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

- [evoland_db_tables](https://ethzplus.github.io/evoland-plus/reference/evoland_db_tables.md) -
  Table active bindings (coords_t, lulc_data_t, etc.)

- [evoland_db_views](https://ethzplus.github.io/evoland-plus/reference/evoland_db_views.md) -
  View active bindings (lulc_meta_long_v, etc.) and methods

- [evoland_db_neighbors](https://ethzplus.github.io/evoland-plus/reference/evoland_db_neighbors.md) -
  Neighbor analysis methods

## Super class

[`evoland::parquet_db`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.md)
-\> `evoland_db`

## Methods

### Public methods

- [`evoland_db$alloc_dinamica()`](#method-evoland_db-alloc_dinamica)

- [`evoland_db$eval_alloc_params_t()`](#method-evoland_db-eval_alloc_params_t)

- [`evoland_db$create_alloc_params_t()`](#method-evoland_db-create_alloc_params_t)

- [`evoland_db$set_neighbors()`](#method-evoland_db-set_neighbors)

- [`evoland_db$generate_neighbor_predictors()`](#method-evoland_db-generate_neighbor_predictors)

- [`evoland_db$trans_pred_data_v()`](#method-evoland_db-trans_pred_data_v)

- [`evoland_db$trans_rates_dinamica_v()`](#method-evoland_db-trans_rates_dinamica_v)

- [`evoland_db$new()`](#method-evoland_db-new)

- [`evoland_db$set_report()`](#method-evoland_db-set_report)

- [`evoland_db$set_coords()`](#method-evoland_db-set_coords)

- [`evoland_db$set_periods()`](#method-evoland_db-set_periods)

- [`evoland_db$add_predictor()`](#method-evoland_db-add_predictor)

- [`evoland_db$lulc_data_as_rast()`](#method-evoland_db-lulc_data_as_rast)

- [`evoland_db$fit_partial_models()`](#method-evoland_db-fit_partial_models)

- [`evoland_db$fit_full_models()`](#method-evoland_db-fit_full_models)

- [`evoland_db$set_full_trans_preds()`](#method-evoland_db-set_full_trans_preds)

- [`evoland_db$get_pruned_trans_preds_t()`](#method-evoland_db-get_pruned_trans_preds_t)

- [`evoland_db$clone()`](#method-evoland_db-clone)

Inherited methods

- [`evoland::parquet_db$attach_table()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-attach_table)
- [`evoland::parquet_db$commit()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-commit)
- [`evoland::parquet_db$delete_from()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-delete_from)
- [`evoland::parquet_db$detach_table()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-detach_table)
- [`evoland::parquet_db$execute()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-execute)
- [`evoland::parquet_db$fetch()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-fetch)
- [`evoland::parquet_db$get_query()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-get_query)
- [`evoland::parquet_db$get_table_metadata()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-get_table_metadata)
- [`evoland::parquet_db$list_tables()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-list_tables)
- [`evoland::parquet_db$print()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-print)
- [`evoland::parquet_db$row_count()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-row_count)
- [`evoland::parquet_db$set_partitioning()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-set_partitioning)
- [`evoland::parquet_db$with_tables()`](https://ethzplus.github.io/evoland-plus/reference/parquet_db.html#method-with_tables)

------------------------------------------------------------------------

### Method [`alloc_dinamica()`](https://ethzplus.github.io/evoland-plus/reference/alloc_dinamica.md)

#### Usage

    evoland_db$alloc_dinamica(
      id_periods,
      id_perturbation,
      work_dir = "dinamica_rundir",
      keep_intermediate = FALSE
    )

------------------------------------------------------------------------

### Method [`eval_alloc_params_t()`](https://ethzplus.github.io/evoland-plus/reference/eval_alloc_params_t.md)

#### Usage

    evoland_db$eval_alloc_params_t(
      id_perturbations = NULL,
      work_dir = "dinamica_rundir",
      keep_intermediate = FALSE
    )

------------------------------------------------------------------------

### Method [`create_alloc_params_t()`](https://ethzplus.github.io/evoland-plus/reference/create_alloc_params_t.md)

#### Usage

    evoland_db$create_alloc_params_t(n_perturbations = 5L, sd = 0.05)

------------------------------------------------------------------------

### Method `set_neighbors()`

#### Usage

    evoland_db$set_neighbors(
      max_distance = 1000,
      distance_breaks = c(0, 100, 500, 1000),
      overwrite = FALSE,
      quiet = FALSE,
      chunksize = 1e+08
    )

------------------------------------------------------------------------

### Method `generate_neighbor_predictors()`

#### Usage

    evoland_db$generate_neighbor_predictors()

------------------------------------------------------------------------

### Method `trans_pred_data_v()`

#### Usage

    evoland_db$trans_pred_data_v(
      id_trans,
      id_period = NULL,
      id_pred = NULL,
      na_value = NA,
      include_period_0 = TRUE
    )

------------------------------------------------------------------------

### Method `trans_rates_dinamica_v()`

#### Usage

    evoland_db$trans_rates_dinamica_v(id_period)

------------------------------------------------------------------------

### Method `new()`

Initialize a new evoland_db object

#### Usage

    evoland_db$new(path, update_reporting = TRUE, ...)

#### Arguments

- `path`:

  Character string. Path to the data folder.

- `update_reporting`:

  Logical. Whether to update the reporting table upon initialization.
  Defaults to TRUE. Set to FALSE for read-only workers to avoid lock
  contention.

- `...`:

  passed on to `set_report`

#### Returns

A new `evoland_db` object

------------------------------------------------------------------------

### Method `set_report()`

Set reporting metadata

#### Usage

    evoland_db$set_report(...)

#### Arguments

- `...`:

  each named argument is entered into the table with the argument name
  as its key

------------------------------------------------------------------------

### Method `set_coords()`

Set coordinates for DB. Cannot overwrite existing table (would mean
cascading deletion)

#### Usage

    evoland_db$set_coords(type = c("square"), ...)

#### Arguments

- `type`:

  string; which type of coordinates to set, see
  [coords_t](https://ethzplus.github.io/evoland-plus/reference/coords_t.md)

- `...`:

  named arguments are passed to the appropriate coordinate creator
  function

------------------------------------------------------------------------

### Method `set_periods()`

Set periods for DB. See
[`periods_t`](https://ethzplus.github.io/evoland-plus/reference/periods_t.md)

#### Usage

    evoland_db$set_periods(
      period_length_str = "P10Y",
      start_observed = "1985-01-01",
      end_observed = "2020-01-01",
      end_extrapolated = "2060-01-01"
    )

#### Arguments

- `period_length_str`:

  ISO 8601 duration string specifying the length of each period
  (currently only accepting years, e.g., "P5Y" for 5 years)

- `start_observed`:

  Start date of the observed data (YYYY-MM-DD)

- `end_observed`:

  End date of the observed data (YYYY-MM-DD)

- `end_extrapolated`:

  End date for extrapolation time range (YYYY-MM-DD)

------------------------------------------------------------------------

### Method `add_predictor()`

Add a predictor to the database

#### Usage

    evoland_db$add_predictor(pred_spec, pred_data, pred_type)

#### Arguments

- `pred_spec`:

  List of predictor specification; see
  [`create_pred_meta_t()`](https://ethzplus.github.io/evoland-plus/reference/pred_meta_t.md)

- `pred_data`:

  An object that can be coerced to
  [`pred_data_t`](https://ethzplus.github.io/evoland-plus/reference/pred_data_t.md),
  but doesn't have an `id_pred`

- `pred_type`:

  Passed to
  [`as_pred_data_t()`](https://ethzplus.github.io/evoland-plus/reference/pred_data_t.md);
  one of float, int, bool

------------------------------------------------------------------------

### Method `lulc_data_as_rast()`

#### Usage

    evoland_db$lulc_data_as_rast(
      extent = NULL,
      resolution = NULL,
      id_period = NULL
    )

------------------------------------------------------------------------

### Method `fit_partial_models()`

#### Usage

    evoland_db$fit_partial_models(
      fit_fun,
      gof_fun,
      sample_pct = 70,
      seed = NULL,
      na_value = NA,
      cores = 1L,
      ...
    )

------------------------------------------------------------------------

### Method `fit_full_models()`

#### Usage

    evoland_db$fit_full_models(
      partial_models,
      gof_criterion,
      maximize = TRUE,
      na_value = NA,
      envir = parent.frame(),
      cores = 1L
    )

------------------------------------------------------------------------

### Method `set_full_trans_preds()`

#### Usage

    evoland_db$set_full_trans_preds(overwrite = FALSE)

------------------------------------------------------------------------

### Method `get_pruned_trans_preds_t()`

#### Usage

    evoland_db$get_pruned_trans_preds_t(
      filter_fun = covariance_filter,
      na_value = NA,
      cores = 1L,
      ...
    )

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    evoland_db$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
