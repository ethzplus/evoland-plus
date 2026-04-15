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

- `read_only`:

  If true, prevents writes that are not parallel-safe

## Methods

### Public methods

- [`parquet_db$new()`](#method-parquet_db-new)

- [`parquet_db$execute()`](#method-parquet_db-execute)

- [`parquet_db$get_query()`](#method-parquet_db-get_query)

- [`parquet_db$row_count()`](#method-parquet_db-row_count)

- [`parquet_db$column_max()`](#method-parquet_db-column_max)

- [`parquet_db$list_tables()`](#method-parquet_db-list_tables)

- [`parquet_db$fetch()`](#method-parquet_db-fetch)

- [`parquet_db$get_table_metadata()`](#method-parquet_db-get_table_metadata)

- [`parquet_db$delete_from()`](#method-parquet_db-delete_from)

- [`parquet_db$commit()`](#method-parquet_db-commit)

- [`parquet_db$print()`](#method-parquet_db-print)

- [`parquet_db$get_table_path()`](#method-parquet_db-get_table_path)

- [`parquet_db$get_read_expr()`](#method-parquet_db-get_read_expr)

- [`parquet_db$clone()`](#method-parquet_db-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new parquet_db object

#### Usage

    parquet_db$new(path, read_only = FALSE, extensions = character(0))

#### Arguments

- `path`:

  Character string. Path to the data folder.

- `read_only`:

  Logical. If true, prevents writes that are not parallel-safe.

- `extensions`:

  Character vector of DuckDB extensions to load (e.g., "spatial")

#### Returns

A new `parquet_db` object

------------------------------------------------------------------------

### Method `execute()`

Execute a SQL statement

#### Usage

    parquet_db$execute(statement)

#### Arguments

- `statement`:

  A SQL statement

#### Returns

Number of rows affected by statement

------------------------------------------------------------------------

### Method `get_query()`

Execute a SQL query and return results

#### Usage

    parquet_db$get_query(statement)

#### Arguments

- `statement`:

  A SQL query statement

#### Returns

A data.table with query results

------------------------------------------------------------------------

### Method `row_count()`

Get row count for a table (without applying id_run subsetting); returns
0 if table does not exist

#### Usage

    parquet_db$row_count(table_name)

#### Arguments

- `table_name`:

  Character string. Name of the table to query.

#### Returns

Integer number of rows

------------------------------------------------------------------------

### Method `column_max()`

Get maximum for a column in a table (without applying id_run
subsetting); returns 0 if table does not exist

#### Usage

    parquet_db$column_max(table_name, column_name)

#### Arguments

- `table_name`:

  Character string. Name of the table to query.

- `column_name`:

  Character string. Name of the column to get the maximum value for.

#### Returns

Maximum value of the column

------------------------------------------------------------------------

### Method `list_tables()`

List all tables (files and folders) in storage

#### Usage

    parquet_db$list_tables()

#### Returns

Character vector of table names

------------------------------------------------------------------------

### Method `fetch()`

Fetch data from a table

#### Usage

    parquet_db$fetch(table_name, cols = NULL, where = NULL, limit = NULL)

#### Arguments

- `table_name`:

  Character string. Name of the table to query.

- `cols`:

  SQL column selection string (e.g., "col1, col2" or "\*")

- `where`:

  Character string. Optional WHERE clause for the SQL query.

- `limit`:

  Integer. Optional limit on number of rows to return.

- `map_cols`:

  Vector of columns to be converted from key/value structs to R lists

#### Returns

A data.table

------------------------------------------------------------------------

### Method `get_table_metadata()`

Get table metadata

#### Usage

    parquet_db$get_table_metadata(table_name)

#### Arguments

- `table_name`:

  Character string. Name of the table to query.

#### Returns

Named list

------------------------------------------------------------------------

### Method `delete_from()`

Delete rows from a table

#### Usage

    parquet_db$delete_from(table_name, where = NULL)

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
partitioning, key identity columns, and list-to-MAP conversion. These
four special column types may be passed as attributes to the `x`
argument. If the table has previously been written to, these settings
are recovered from the parquet metadata.

#### Usage

    parquet_db$commit(x, table_name, method = c("overwrite", "append", "upsert"))

#### Arguments

- `x`:

  If data.table, the data to commit. If character, treated as an
  in-DuckDB-memory table or view name.

- `table_name`:

  Target table name to commit to.

- `method`:

  Character, one of "overwrite", "append", "upsert" (upsert being an
  update for existing rows, and insert for new rows; this necessitates
  loading the full data into memory to know what to update. This may be
  expensive.

#### Returns

Number of rows written

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for parquet_db

#### Usage

    parquet_db$print(subheaders = character(0), ...)

#### Arguments

- `subheaders`:

  optional character vector; insert as subheaders lines

- `...`:

  Not used

#### Returns

self (invisibly)

------------------------------------------------------------------------

### Method `get_table_path()`

Get file path (or directory path) for a table

#### Usage

    parquet_db$get_table_path(table_name)

#### Arguments

- `table_name`:

  Character string table name

#### Returns

Character path

------------------------------------------------------------------------

### Method `get_read_expr()`

Get SQL expression to read a table

#### Usage

    parquet_db$get_read_expr(table_name)

#### Arguments

- `table_name`:

  Character string table name

#### Returns

Character string SQL expression

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    parquet_db$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
