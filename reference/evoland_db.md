# R6 Class for Folder-Based Data Storage Interface

An R6 class that provides an interface to a folder-based data storage
system for the evoland package. Each table is stored as a parquet (or
CSV) file. This class uses DuckDB for in-memory SQL operations while
persisting data to disk in parquet format for better compression.

## Public fields

- `connection`:

  DBI connection object to an in-memory DuckDB database

- `path`:

  Character string path to the data folder

- `default_format`:

  Default file format for new tables

## Active bindings

- `coords_t`:

  A `coords_t` instance; see `create_coords_t()` for the type of object
  to assign. Assigning is an upsert operation.

- `extent`:

  Return a terra SpatExtent based on coords_t

- `coords_minimal`:

  data.table with only (id_coord, lon, lat)

- `periods_t`:

  A `periods_t` instance; see
  [`create_periods_t()`](https://ethzplus.github.io/evoland-plus/reference/periods_t.md)
  for the type of object to assign. Assigning is an upsert operation.

- `lulc_meta_t`:

  A `lulc_meta_t` instance; see
  [`create_lulc_meta_t()`](https://ethzplus.github.io/evoland-plus/reference/lulc_meta_t.md)
  for the type of object to assign. Assigning is an upsert operation.

- `lulc_meta_long_v`:

  Return a `lulc_meta_long_v` instance, i.e. unrolled `lulc_meta_t`.

- `lulc_data_t`:

  A `lulc_data_t` instance; see
  [`as_lulc_data_t()`](https://ethzplus.github.io/evoland-plus/reference/lulc_data_t.md)
  for the type of object to assign. Assigning is an upsert operation.

- `pred_meta_t`:

  A `pred_meta_t` instance; see
  [`create_pred_meta_t()`](https://ethzplus.github.io/evoland-plus/reference/pred_meta_t.md)
  for the type of object to assign. Assigning is an upsert operation.

- `pred_sources_v`:

  Retrieve a table of distinct predictor urls and their md5sum

- `pred_data_t_float`:

  A `pred_data_t_float` instance; see `create_pred_data_t()` for the
  type of object to assign. Assigning is an upsert operation.

- `pred_data_t_int`:

  A `pred_data_t_int` instance; see `create_pred_data_t()` for the type
  of object to assign. Assigning is an upsert operation.

- `pred_data_t_bool`:

  A `pred_data_t_bool` instance; see `create_pred_data_t()` for the type
  of object to assign. Assigning is an upsert operation.

- `trans_meta_t`:

  A `trans_meta_t` instance; see `create_trans_meta_t()` for the type of
  object to assign. Assigning is an upsert operation.

- `trans_preds_t`:

  A `trans_preds_t` instance; see `create_trans_preds_t()` for the type
  of object to assign. Assigning is an upsert operation.

- `intrv_meta_t`:

  A `intrv_meta_t` instance; see
  [`create_intrv_meta_t()`](https://ethzplus.github.io/evoland-plus/reference/intrv_meta_t.md)
  for the type of object to assign. Assigning is an upsert operation.

- `intrv_masks_t`:

  A `intrv_masks_t` instance; see
  [`as_intrv_masks_t()`](https://ethzplus.github.io/evoland-plus/reference/intrv_masks_t.md)
  for the type of object to assign. Assigning is an upsert operation.

- `trans_models_t`:

  A `trans_models_t` instance; see `create_trans_models_t()` for the
  type of object to assign. Assigning is an upsert operation.

- `alloc_params_t`:

  A `alloc_params_t` instance; see
  [`as_alloc_params_t()`](https://ethzplus.github.io/evoland-plus/reference/alloc_params_t.md)
  for the type of object to assign. Assigning is an upsert operation.

## Methods

### Public methods

- [`evoland_db$new()`](#method-evoland_db-new)

- [`evoland_db$commit()`](#method-evoland_db-commit)

- [`evoland_db$fetch()`](#method-evoland_db-fetch)

- [`evoland_db$list_tables()`](#method-evoland_db-list_tables)

- [`evoland_db$execute()`](#method-evoland_db-execute)

- [`evoland_db$row_count()`](#method-evoland_db-row_count)

- [`evoland_db$delete_from()`](#method-evoland_db-delete_from)

- [`evoland_db$set_coords()`](#method-evoland_db-set_coords)

- [`evoland_db$set_periods()`](#method-evoland_db-set_periods)

- [`evoland_db$add_predictor()`](#method-evoland_db-add_predictor)

- [`evoland_db$clone()`](#method-evoland_db-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new evoland_db object

#### Usage

    evoland_db$new(
      path,
      default_format = c("parquet", "csv"),
      report_name = "evoland_scenario",
      report_name_pretty = "Default Evoland Scenario",
      report_include_date = TRUE,
      report_username = Sys.getenv("USER", unset = "unknown")
    )

#### Arguments

- `path`:

  Character string. Path to the data folder.

- `default_format`:

  Character. Default file format ("parquet" or "csv"). Default is
  "parquet".

- `report_name`:

  Character string. Name of the report scenario.

- `report_name_pretty`:

  Character string. Pretty name of the report scenario.

- `report_include_date`:

  Logical. Whether to include the date in the report scenario.

- `report_username`:

  Character string. Username for the report scenario, defaults to \$USER
  env var

#### Returns

A new `evoland_db` object

------------------------------------------------------------------------

### Method `commit()`

Commit data to storage

#### Usage

    evoland_db$commit(x, table_name, mode = "upsert", format = NULL)

#### Arguments

- `x`:

  Data object to commit.

- `table_name`:

  Table to target

- `mode`:

  Character string. One of "upsert" (default), "append", or "overwrite".

- `format`:

  Character string. File format: "parquet" or "csv". Defaults to
  object's default_format.

#### Returns

NULL (called for side effects)

------------------------------------------------------------------------

### Method `fetch()`

Fetch data from storage

#### Usage

    evoland_db$fetch(table_name, where = NULL, limit = NULL)

#### Arguments

- `table_name`:

  Character string. Name of the table to query.

- `where`:

  Character string. Optional WHERE clause for the SQL query.

- `limit`:

  integerish, limit the amount of rows to return

#### Returns

A data.table

------------------------------------------------------------------------

### Method `list_tables()`

List all tables (files) in storage

#### Usage

    evoland_db$list_tables()

#### Returns

Character vector of table names

------------------------------------------------------------------------

### Method `execute()`

Execute statement

#### Usage

    evoland_db$execute(statement)

#### Arguments

- `statement`:

  A SQL statement

#### Returns

No. of rows affected by statement

------------------------------------------------------------------------

### Method `row_count()`

Get table row count

#### Usage

    evoland_db$row_count(table_name)

#### Arguments

- `table_name`:

  Character string. Name of the table to query.

#### Returns

No. of rows

------------------------------------------------------------------------

### Method `delete_from()`

Delete rows from a table

#### Usage

    evoland_db$delete_from(table_name, where = NULL)

#### Arguments

- `table_name`:

  Character string. Name of the table to delete from.

- `where`:

  Character string, defaults to NULL: delete everything in table.

#### Returns

No. of rows affected

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

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    evoland_db$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
