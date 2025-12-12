# R6 Base Class for Parquet-Backed DuckDB Storage

A domain-agnostic R6 class that provides an interface to a folder-based
data storage system using DuckDB for in-memory SQL operations and
parquet files for efficient on-disk persistence. This class can be
inherited by domain-specific database classes.

## Public fields

- `connection`:

  DBI connection object to an in-memory DuckDB database

- `path`:

  Character string path to the data folder

- `writeopts`:

  Write options for DuckDB parquet output

## Methods

### Public methods

- [`parquet_duckdb$new()`](#method-parquet_duckdb-new)

- [`parquet_duckdb$execute()`](#method-parquet_duckdb-execute)

- [`parquet_duckdb$get_query()`](#method-parquet_duckdb-get_query)

- [`parquet_duckdb$attach_table()`](#method-parquet_duckdb-attach_table)

- [`parquet_duckdb$detach_table()`](#method-parquet_duckdb-detach_table)

- [`parquet_duckdb$row_count()`](#method-parquet_duckdb-row_count)

- [`parquet_duckdb$list_tables()`](#method-parquet_duckdb-list_tables)

- [`parquet_duckdb$with_tables()`](#method-parquet_duckdb-with_tables)

- [`parquet_duckdb$fetch()`](#method-parquet_duckdb-fetch)

- [`parquet_duckdb$delete_from()`](#method-parquet_duckdb-delete_from)

- [`parquet_duckdb$commit()`](#method-parquet_duckdb-commit)

- [`parquet_duckdb$print()`](#method-parquet_duckdb-print)

- [`parquet_duckdb$clone()`](#method-parquet_duckdb-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new parquet_duckdb object

#### Usage

    parquet_duckdb$new(path, extensions = character(0))

#### Arguments

- `path`:

  Character string. Path to the data folder.

- `extensions`:

  Character vector of DuckDB extensions to load (e.g., "spatial")

#### Returns

A new `parquet_duckdb` object

------------------------------------------------------------------------

### Method `execute()`

Execute a SQL statement

#### Usage

    parquet_duckdb$execute(statement)

#### Arguments

- `statement`:

  A SQL statement

#### Returns

Number of rows affected by statement

------------------------------------------------------------------------

### Method `get_query()`

Execute a SQL query and return results

#### Usage

    parquet_duckdb$get_query(statement)

#### Arguments

- `statement`:

  A SQL query statement

#### Returns

A data.table with query results

------------------------------------------------------------------------

### Method `attach_table()`

Attach a table from parquet file as a temporary table in DuckDB

#### Usage

    parquet_duckdb$attach_table(table_name, columns = "*", where = NULL)

#### Arguments

- `table_name`:

  Character. Name of table to attach.

- `columns`:

  Character vector. Optional SQL column selection, defaults to "\*"

- `where`:

  Character. Optional SQL WHERE clause to subset the table.

#### Returns

Invisible NULL (called for side effects)

------------------------------------------------------------------------

### Method `detach_table()`

Detach a table from the in-memory database

#### Usage

    parquet_duckdb$detach_table(table_name)

#### Arguments

- `table_name`:

  Character. Name of table to drop.

#### Returns

Invisible NULL (called for side effects)

------------------------------------------------------------------------

### Method `row_count()`

Get row count for a table

#### Usage

    parquet_duckdb$row_count(table_name)

#### Arguments

- `table_name`:

  Character string. Name of the table to query.

#### Returns

Integer number of rows

------------------------------------------------------------------------

### Method `list_tables()`

List all tables (files) in storage

#### Usage

    parquet_duckdb$list_tables()

#### Returns

Character vector of table names

------------------------------------------------------------------------

### Method `with_tables()`

Execute a function with specified tables attached, handling
attach/detach automatically. If a table is already attached in the
DuckDB instance, it won't be re-attached or detached.

#### Usage

    parquet_duckdb$with_tables(tables, func, ...)

#### Arguments

- `tables`:

  Character vector of table names to attach

- `func`:

  Function to execute with tables attached

- `...`:

  Additional arguments passed to func

#### Returns

Result of func

------------------------------------------------------------------------

### Method `fetch()`

Fetch data from a table

#### Usage

    parquet_duckdb$fetch(table_name, where = NULL, limit = NULL, map_cols = NULL)

#### Arguments

- `table_name`:

  Character string. Name of the table to query.

- `where`:

  Character string. Optional WHERE clause for the SQL query.

- `limit`:

  Integer. Optional limit on number of rows to return.

- `map_cols`:

  Vector of columns to be converted from key/value structs to R lists

#### Returns

A data.table

------------------------------------------------------------------------

### Method `delete_from()`

Delete rows from a table

#### Usage

    parquet_duckdb$delete_from(table_name, where = NULL)

#### Arguments

- `table_name`:

  Character string. Name of the table to delete from.

- `where`:

  Character string. Optional WHERE clause; if NULL, deletes all rows.

#### Returns

Number of rows deleted

------------------------------------------------------------------------

### Method `commit()`

Commit data using overwrite, append, or upsert modes. Handles
autoincrement, key identity columns, and list-to-MAP conversion.

#### Usage

    parquet_duckdb$commit(
      x,
      table_name,
      key_cols,
      autoincrement_cols = character(0),
      map_cols = character(0),
      method = c("overwrite", "append", "upsert")
    )

#### Arguments

- `x`:

  Data frame to commit. If character, in-duckdb-memory table.

- `table_name`:

  Character string table name

- `key_cols`:

  Character vector of columns that define uniqueness. If missing, use
  all columns starting with `id_`

- `autoincrement_cols`:

  Character vector of column names to auto-increment

- `map_cols`:

  Character vector of columns to convert to MAP format

- `method`:

  Character, one of "overwrite", "append", "upsert" (upsert being an
  update for existing rows, and insert for new rows)

#### Returns

Invisible NULL (called for side effects)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for parquet_duckdb

#### Usage

    parquet_duckdb$print(...)

#### Arguments

- `...`:

  Not used

#### Returns

self (invisibly) param x Data frame to commit. If character,
in-duckdb-memory table. param table_name Character string table name
param autoincrement_cols Character vector of column names to
auto-increment return Invisible NULL (called for side effects) param x
Data frame to commit. If character, in-duckdb-memory table. param
table_name Character string table name param autoincrement_cols
Character vector of column names to auto-increment return Invisible NULL
(called for side effects) param x Data frame to commit. If character,
in-duckdb-memory table. param table_name Character string table name
param key_cols Character vector of columns that define uniqueness. If
missing, use all columns starting with `id_` param autoincrement_cols
Character vector of column names to auto-increment return Invisible NULL
(called for side effects)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    parquet_duckdb$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
