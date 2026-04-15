# Parquet database utility functions

A set of utility functions for working with
[parquet_db](https://ethzplus.github.io/evoland-plus/reference/parquet_db.md).

## Usage

``` r
as_parquet_db_t(
  x,
  class_name = character(),
  key_cols = NULL,
  alternate_key_cols = NULL,
  map_cols = NULL,
  partition_cols = NULL
)

resolve_cols(x, metadata = list(), attr = character(1))

resolve_partition_clause(x, metadata = list())

resolve_metadata_clause(x, metadata = list())

convert_list_cols(x, cols, fn)

list_to_kv_df(x)

kv_df_to_list(x)

create_table_binding(
  table_name,
  mode = c("write_once", "upsert", "append", "overwrite")
)

create_method_binding(fun, with_private = FALSE, with_super = FALSE)

cols_to_select_expr(cols, table_name)
```

## Arguments

- x:

  The data to be committed.

- class_name:

  Optional class name to prepend before "parquet_db_t". Used to create
  more specific subclasses.

- key_cols:

  Optional character vector. Used for data.table's
  [data.table::setkey](https://rdrr.io/pkg/data.table/man/setkey.html)
  and used by
  [parquet_db](https://ethzplus.github.io/evoland-plus/reference/parquet_db.md)
  to determine which columns to use for upsert operations. Together with
  `alternate_key_cols`, these are used to define uniqueness constraints.

- alternate_key_cols:

  Optional character vector. These are used in conjunction with
  `key_cols` to define uniqueness constraints, but are not used for
  upsert operations. Example: numeric primary key whose distinctness is
  identical to a character primary key column, but upsert operations
  should be based on the character key, leaving the
  character\<-\>numeric mapping intact.

- map_cols:

  Optional character vector. Used to coerce columns to DuckDB's MAP
  type, used to store named unnested lists from R.

- partition_cols:

  Optional character vector. Used to specify columns for hive style
  parquet file partitioning.

- metadata:

  The metadata to be committed, as returned by
  [parquet_db](https://ethzplus.github.io/evoland-plus/reference/parquet_db.md)
  `$get_table_metadata()`

- attr:

  The column function to resolve, e.g. "alternate_key_cols", "map_cols",
  "key_cols".

- cols:

  The columns to select.

- fn:

  The function to apply to each element of the list columns

- table_name:

  The name of the table to bind to.

- mode:

  The mode of the binding, which determines the behavior when committing
  data. Options are: "write_once" (default, only allows writing if table
  doesn't exist), "upsert"

- fun:

  The underlying function to bind as an R6 method, which must have a
  `self` argument

- with_private:

  Whether to also pass the R6 reference to `private` as an argument

- with_super:

  Whether to also pass the R6 reference to `super` as an argument

## Functions

- `as_parquet_db_t()`: Coerce to parquet_db_t subclass. It coerces an
  object to a data.table and adds attributes that
  [parquet_db](https://ethzplus.github.io/evoland-plus/reference/parquet_db.md)
  relies on for database-like operations. See the paramaters for
  details.

- `resolve_cols()`: Resolves which columns to use for key, map,
  partition, etc.; pre-existing schema takes precedence.

- `resolve_partition_clause()`: Compose partitioning clause.

- `resolve_metadata_clause()`: Compose metadata clause, which includes
  any new metadata from the data's attributes, as well as any existing
  metadata from the database; existing metadata cannot be safely
  overwritten. New metadata takes precedence over existing metadata for
  this commit. Any atomic vector is coerced to a quoted comma separated
  list of values, which are recovered in
  [parquet_db](https://ethzplus.github.io/evoland-plus/reference/parquet_db.md)
  `$get_table_metadata()`. Non-atomic metadata values are dropped with a
  warning.

- `convert_list_cols()`: Convert list columns by applying `fn`

- `list_to_kv_df()`: Convert a named list to a data.frame with "key" and
  "value" columns.

- `kv_df_to_list()`: Convert a data.frame with "key" and "value" columns
  to a named list.

- `create_table_binding()`: Create a binding function for a table, which
  can be used to fetch or commit data to that table. The active binding
  either returns the table (missing argument), or upserts to it
  (assignment operation)

- `create_method_binding()`: Helper function to bind a function as a
  method to an R6 generator. Simply passes the R6 method's arguments
  as-is to the underlying "pure" function, passing the R6 reference to
  `self`, but not to `private`.

- `cols_to_select_expr()`: Paste vector of escaped column names into a
  SQL select statement, with optional table name prefix.
