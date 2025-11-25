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

- `writeopts`:

  Default write options for DuckDB, see

## Active bindings

- `coords_t`:

  A `coords_t` instance; see `create_coords_t()` for the type of object
  to assign. Assigning is an upsert operation.

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

- `pred_data_t_float`:

  A `pred_data_t_float` instance; see `create_pred_data_t()` for the
  type of object to assign. Assigning is an upsert operation.

- `pred_data_t_int`:

  A `pred_data_t_int` instance; see `create_pred_data_t()` for the type
  of object to assign. Assigning is an upsert operation.

- `pred_data_t_bool`:

  A `pred_data_t_bool` instance; see `create_pred_data_t()` for the type
  of object to assign. Assigning is an upsert operation.

- `extent`:

  Return a terra SpatExtent based on coords_t

- `coords_minimal`:

  data.table with only (id_coord, lon, lat)

- `pred_meta_t`:

  A `pred_meta_t` instance; see
  [`create_pred_meta_t()`](https://ethzplus.github.io/evoland-plus/reference/pred_meta_t.md)
  for the type of object to assign. Assigning is an upsert operation.

- `pred_sources_v`:

  Retrieve a table of distinct predictor urls and their md5sum

- `trans_meta_t`:

  A `trans_meta_t` instance; see
  [`create_trans_meta_t()`](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md)
  for the type of object to assign. Assigning is an upsert operation.

- `trans_preds_t`:

  A `trans_preds_t` instance; see
  [`create_trans_preds_t()`](https://ethzplus.github.io/evoland-plus/reference/trans_preds_t.md)
  for the type of object to assign. Assigning is an upsert operation.

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

- [`evoland_db$commit_overwrite()`](#method-evoland_db-commit_overwrite)

- [`evoland_db$commit_append()`](#method-evoland_db-commit_append)

- [`evoland_db$commit_upsert()`](#method-evoland_db-commit_upsert)

- [`evoland_db$fetch()`](#method-evoland_db-fetch)

- [`evoland_db$list_tables()`](#method-evoland_db-list_tables)

- [`evoland_db$execute()`](#method-evoland_db-execute)

- [`evoland_db$get_query()`](#method-evoland_db-get_query)

- [`evoland_db$attach_table()`](#method-evoland_db-attach_table)

- [`evoland_db$detach_table()`](#method-evoland_db-detach_table)

- [`evoland_db$row_count()`](#method-evoland_db-row_count)

- [`evoland_db$delete_from()`](#method-evoland_db-delete_from)

- [`evoland_db$set_report()`](#method-evoland_db-set_report)

- [`evoland_db$set_coords()`](#method-evoland_db-set_coords)

- [`evoland_db$set_periods()`](#method-evoland_db-set_periods)

- [`evoland_db$add_predictor()`](#method-evoland_db-add_predictor)

- [`evoland_db$clone()`](#method-evoland_db-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new evoland_db object

#### Usage

    evoland_db$new(path, default_format = c("parquet", "csv"), ...)

#### Arguments

- `path`:

  Character string. Path to the data folder.

- `default_format`:

  Character. Default file format ("parquet" or "csv"). Default is
  "parquet".

- `...`:

  passed on to `set_report`

#### Returns

A new `evoland_db` object

------------------------------------------------------------------------

### Method `commit_overwrite()`

Commit data in overwrite mode

#### Usage

    evoland_db$commit_overwrite(
      x,
      table_name,
      autoincrement_cols = character(0),
      map_cols = character(0)
    )

#### Arguments

- `x`:

  Data frame to commit

- `table_name`:

  Character string table name

- `autoincrement_cols`:

  Character vector of column names to auto-increment

- `map_cols`:

  Character vector of columns to convert to MAP format

------------------------------------------------------------------------

### Method `commit_append()`

Commit data in append mode

#### Usage

    evoland_db$commit_append(
      x,
      table_name,
      autoincrement_cols = character(0),
      map_cols = character(0)
    )

#### Arguments

- `x`:

  Data frame to commit

- `table_name`:

  Character string table name

- `autoincrement_cols`:

  Character vector of column names to auto-increment

- `map_cols`:

  Character vector of columns to convert to MAP format

------------------------------------------------------------------------

### Method `commit_upsert()`

Commit data in upsert mode

#### Usage

    evoland_db$commit_upsert(
      x,
      table_name,
      key_cols = grep("^id_", names(x), value = TRUE),
      autoincrement_cols = character(0),
      map_cols = character(0)
    )

#### Arguments

- `x`:

  Data frame to commit

- `table_name`:

  Character string table name

- `key_cols`:

  Identify unique columns - heuristic: if prefixed with id\_, the set of
  all columns designates a uniqueness condition

- `autoincrement_cols`:

  Character vector of column names to auto-increment

- `map_cols`:

  Character vector of columns to convert to MAP format

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

### Method `get_query()`

Get Query

#### Usage

    evoland_db$get_query(statement)

#### Arguments

- `statement`:

  A SQL statement

#### Returns

No. of rows affected by statement

------------------------------------------------------------------------

### Method `attach_table()`

Attach one or more tables from the database folder as temporary tables
in DuckDB. This is useful for working with multiple tables in SQL
queries without loading them into R memory.

#### Usage

    evoland_db$attach_table(table_name, columns = "*")

#### Arguments

- `table_name`:

  Character vector. Names of table to attach.

- `columns`:

  Character vector. Optional sql column selection, defaults to "\*"

------------------------------------------------------------------------

### Method `detach_table()`

Detach one or more tables from the database.

#### Usage

    evoland_db$detach_table(table_name)

#### Arguments

- `table_name`:

  Character. Name of table to drop.

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

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    evoland_db$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
