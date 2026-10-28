# R6 Base Class for DuckLake-Backed Storage

A domain-agnostic R6 class that provides an interface to a folder-based
data storage system. An in-memory [DuckDB](https://duckdb.org/) instance
attaches a [DuckLake](https://ducklake.select/) catalog, so that writes
are atomic and readers see snapshot-isolated data even while other
processes are writing. This class can be inherited by domain-specific
database classes.

By default a database is one self-contained folder: the catalog lives in
SQLite at `<path>/catalog.sqlite` and the parquet data files at
`<path>/data/`. Passing `catalog` and/or `data_path` puts either half
somewhere else, such as a shared PostgreSQL catalog or an S3 bucket.

## Public fields

- `connection`:

  DBI connection object to an in-memory DuckDB database

- `catalog`:

  Character string, the DuckLake catalog connection, e.g.
  `"sqlite:<path>/catalog.sqlite"`

- `data_path`:

  Character string, where DuckLake writes its data files

- `read_only`:

  If true, the catalog is attached read-only

- `retry_timeout`:

  Numeric, how long in seconds to keep retrying a contended catalog
  operation before giving up.

- `retry_wait`:

  Numeric, base wait in seconds for the retry backoff, which doubles per
  attempt up to 64 times this

- `retry_max`:

  Integer, maximum number of attempts in case something retries faster
  than `retry_timeout` can measure.

## Methods

### Public methods

- [`ducklake_db$new()`](#method-ducklake_db-initialize)

- [`ducklake_db$execute()`](#method-ducklake_db-execute)

- [`ducklake_db$get_query()`](#method-ducklake_db-get_query)

- [`ducklake_db$row_count()`](#method-ducklake_db-row_count)

- [`ducklake_db$list_tables()`](#method-ducklake_db-list_tables)

- [`ducklake_db$fetch()`](#method-ducklake_db-fetch)

- [`ducklake_db$get_table_metadata()`](#method-ducklake_db-get_table_metadata)

- [`ducklake_db$delete_from()`](#method-ducklake_db-delete_from)

- [`ducklake_db$commit()`](#method-ducklake_db-commit)

- [`ducklake_db$transaction()`](#method-ducklake_db-transaction)

- [`ducklake_db$checkpoint()`](#method-ducklake_db-checkpoint)

- [`ducklake_db$print()`](#method-ducklake_db-print)

- [`ducklake_db$next_id()`](#method-ducklake_db-next_id)

- [`ducklake_db$get_read_expr()`](#method-ducklake_db-get_read_expr)

- [`ducklake_db$clone()`](#method-ducklake_db-clone)

------------------------------------------------------------------------

### `ducklake_db$new()`

Initialize a new ducklake_db object

#### Usage

    ducklake_db$new(
      path = NULL,
      read_only = FALSE,
      extensions = character(0),
      catalog = NULL,
      data_path = NULL,
      sqlite_journal_mode = "wal",
      expire_older_than = NULL,
      delete_older_than = NULL
    )

#### Arguments

- `path`:

  Character string. Path to the data folder. Only needed for the halves
  of the database that are derived from it, so it may be omitted when
  both `catalog` and `data_path` are given.

- `read_only`:

  Logical. If true, the catalog is attached read-only.

- `extensions`:

  Character vector of additional DuckDB extensions to load

- `catalog`:

  Character string. DuckLake catalog connection; defaults to SQLite at
  `<path>/catalog.sqlite`. Anything DuckLake accepts works, e.g.
  `"postgres:dbname=evoland host=..."`. See [Choosing a catalog
  database](https://ducklake.select/docs/stable/duckdb/usage/choosing_a_catalog_database)

- `data_path`:

  Character string. Where DuckLake writes its data files; defaults to
  `<path>/data/`. May be remote, e.g. `"s3://bucket/prefix/"`. See
  [Choosing
  storage](https://ducklake.select/docs/stable/duckdb/usage/choosing_storage)

- `sqlite_journal_mode`:

  Character string, the journal mode to put a SQLite catalog in;
  [`wal`](https://www.sqlite.org/wal.html) by default, because the
  default rollback journal lets one writer block every other process's
  *reads* of the catalog. `NULL` leaves the catalog as it is. Ignored
  for catalogs that are not SQLite or when attaching read-only. Readers
  adapt to whatever journal mode they find.

- `expire_older_than, delete_older_than`:

  Retention for `$checkpoint()`, as an interval DuckDB reads, e.g.
  `"7 days"`: how old a snapshot must be before it is expired, and how
  old an unreferenced file must be before it is deleted. Stored in the
  catalog but can be overwritten by passing. Default to NULL, discarding
  nothing.

#### Returns

A new `ducklake_db` object

------------------------------------------------------------------------

### `ducklake_db$execute()`

Execute a SQL statement. Retried on catalog lock contention, except
inside a `$transaction()`, which is retried as a whole instead.

#### Usage

    ducklake_db$execute(statement, ..., .open = "{", .close = "}")

#### Arguments

- `statement`:

  A SQL statement, interpolated with
  [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html)
  in the caller's environment: `{value}` is quoted as a value,
  `` {`name`} `` as an identifier, `` {`names`*} `` as a comma-separated
  list of them, and a
  [`DBI::SQL()`](https://dbi.r-dbi.org/reference/SQL.html) object – such
  as `$get_read_expr()` returns – inserted as it stands. A statement
  already built with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) or
  [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html)
  is passed through as it is.

- `...`:

  Values for the interpolation, as with
  [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html).

- `.open, .close`:

  Interpolation delimiters, for a statement whose own syntax needs the
  braces.

#### Returns

Number of rows affected by statement

------------------------------------------------------------------------

### `ducklake_db$get_query()`

Execute a SQL query and return results. Reading contends for the catalog
lock too; queries are retried on the same terms as writes.

#### Usage

    ducklake_db$get_query(
      statement,
      ...,
      as_atomic = FALSE,
      .open = "{",
      .close = "}"
    )

#### Arguments

- `statement`:

  A SQL query statement, interpolated as in `$execute()`.

- `...`:

  Values for the interpolation, as with
  [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html).

- `as_atomic`:

  Logical. If true, return the single column the query selects as a
  plain vector rather than a one-column data.table.

- `.open, .close`:

  Interpolation delimiters, as in `$execute()`.

#### Returns

A data.table with query results, or a vector if `as_atomic`

------------------------------------------------------------------------

### `ducklake_db$row_count()`

Get row count for a table; returns 0 if table does not exist

#### Usage

    ducklake_db$row_count(table_name)

#### Arguments

- `table_name`:

  Character string. Name of the table to query.

#### Returns

Integer number of rows

------------------------------------------------------------------------

### `ducklake_db$list_tables()`

List all tables in storage

#### Usage

    ducklake_db$list_tables(include_internal = FALSE)

#### Arguments

- `include_internal`:

  Logical. If true, also list auxiliary tables this class constructs.

#### Returns

Character vector of table names

------------------------------------------------------------------------

### `ducklake_db$fetch()`

Fetch data from a table

#### Usage

    ducklake_db$fetch(table_name, cols = NULL, where = NULL, limit = NULL)

#### Arguments

- `table_name`:

  Character string. Name of the table to query.

- `cols`:

  SQL column selection string (e.g., "col1, col2" or "\*")

- `where`:

  Character string. Optional WHERE clause for the SQL query (e.g.
  `id_pred in (0,4,5)`)

- `limit`:

  Integer. Optional limit on number of rows to return.

#### Returns

A data.table

------------------------------------------------------------------------

### `ducklake_db$get_table_metadata()`

Get table metadata, stored as a comment on the catalog table. A table
with no metadata, and one that does not exist, both return an empty list

#### Usage

    ducklake_db$get_table_metadata(table_name)

#### Arguments

- `table_name`:

  Character string. Name of the table to query.

#### Returns

Named list

------------------------------------------------------------------------

### `ducklake_db$delete_from()`

Delete rows from a table

#### Usage

    ducklake_db$delete_from(table_name, where = NULL)

#### Arguments

- `table_name`:

  Character string. Name of the table to delete from.

- `where`:

  Character string. Optional WHERE clause; if NULL, deletes all rows.

#### Returns

Number of rows deleted

------------------------------------------------------------------------

### `ducklake_db$commit()`

Commit data using overwrite, append, or upsert modes. Handles
partitioning, key identity columns, and list-to-MAP conversion. Which
columns serve which purpose is read from `x`'s attributes, and otherwise
from the metadata the target table carries – so a character `x` inherits
the target's specs, and a table first created from one has none.

#### Usage

    ducklake_db$commit(x, table_name, method = c("overwrite", "append", "upsert"))

#### Arguments

- `x`:

  If data.table, the data to commit; use
  [as_ducklake_db_t](https://ethzplus.github.io/evoland-plus/reference/ducklake_db_utils.md)
  for setting constraints. If character, treated as an in-DuckDB-memory
  table or view name.

- `table_name`:

  Target table name to commit to.

- `method`:

  Character, one of "overwrite", "append", "upsert" (upsert being an
  update for existing rows, and insert for new rows). Only "overwrite"
  can change a table's schema; even if a table is empty, the others
  reject columns the table does not have.

#### Returns

Number of rows written

------------------------------------------------------------------------

### `ducklake_db$transaction()`

Evaluate `expr` as a single DuckLake transaction, so that a block of
reads/writes either all land or none do. On failure part-way through, DB
state is rolled back and no harm is done. Contention is retried by
re-evaluating the whole block, so `expr` is safe to run more than once –
it re-reads whatever it derived its writes from.

#### Usage

    ducklake_db$transaction(expr)

#### Arguments

- `expr`:

  Code to evaluate inside the transaction

#### Returns

The value of `expr`

------------------------------------------------------------------------

### `ducklake_db$checkpoint()`

Run DuckLake's `CHECKPOINT`, which flushes inlined data, expires
snapshots, merges adjacent files, rewrites files with many deletes, and
removes the files left unreferenced.

Every commit adds a snapshot and leaves the files it superseded in
place, and DuckLake deletes neither on its own, so a long run grows
without bound until this is called.

How much is discarded is governed by the `expire_older_than` and
`delete_older_than` options, set on the database (see `$new()`). Left
unset they keep everything, so checkpointing an unconfigured database
compacts but deletes nothing. Expiring a snapshot gives up time travel
to it, and anything still reading at one loses the files under it, so
set a retention that covers the readers you expect.

#### Usage

    ducklake_db$checkpoint()

#### Returns

Snapshot counts before and after, invisibly

------------------------------------------------------------------------

### `ducklake_db$print()`

Print method for ducklake_db

#### Usage

    ducklake_db$print(subheaders = character(0), ...)

#### Arguments

- `subheaders`:

  optional character vector; insert as subheaders lines

- `...`:

  Not used

#### Returns

self (invisibly)

------------------------------------------------------------------------

### `ducklake_db$next_id()`

Use inside `$transaction()`: Safely hand out the next value of an
integer id, such that two processes allocating at once cannot both get
the same one. Is not a DuckLake native feature; employs an auxiliary
table for row-level locks to ensure uniqueness.

#### Usage

    ducklake_db$next_id(table_name, column_name, n = 1L)

#### Arguments

- `table_name`:

  Character string, the table the id belongs to.

- `column_name`:

  Character string, the id column.

- `n`:

  Integer, how many consecutive ids to reserve.

#### Returns

Integer vector of length `n`, the allocated ids. Side effect: increment
counter in DB.

------------------------------------------------------------------------

### `ducklake_db$get_read_expr()`

Get SQL expression to read a table, potentially with additional
filtering applied.
[evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
uses it to inject its own state based filter.

#### Usage

    ducklake_db$get_read_expr(table_name, ...)

#### Arguments

- `table_name`:

  Character string table name

- `...`:

  Ignored here; lets subclasses add their own filters, see
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)

#### Returns

Character string SQL expression

------------------------------------------------------------------------

### `ducklake_db$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ducklake_db$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# needs a reachable PostgreSQL catalog and bucket credentials, so not run
if (FALSE) { # \dontrun{
ducklake_db$new(
  path = "scratch",
  catalog = "postgres:dbname=evoland host=catalog.example.org",
  data_path = "s3://evoland/lake/"
)
} # }
```
