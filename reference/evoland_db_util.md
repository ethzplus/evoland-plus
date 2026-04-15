# Utility functions and methods for `evoland_db`

Factored out utility functions and methods specifically used in
conjunction with `evoland_db` and its subclasses.

## Usage

``` r
get_evoland_db_read_expr(self, super, table_name)

run_parallel_evoland(items, worker_fun, parent_db, cluster = NULL, ...)
```

## Arguments

- self:

  The `evoland_db` instance

- table_name:

  The name of the table to read from

- items:

  A list of items to iterate over

- worker_fun:

  A function to apply to each item, with signature
  `worker_fun(item, db, ...)`. The `db` argument will be a new
  `evoland_db` instance for each worker.

- parent_db:

  An
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance; used for its `path` and `id_run`; or passed directly to the
  worker for serial case.

- cluster:

  An optional cluster object created by
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
  or `mirai::make_cluster()`.

- ...:

  Additional arguments passed to `worker_fun`.

## Value

A SQL expression string to read most specific data for slice

A list of results

## Functions

- `get_evoland_db_read_expr()`: Gets the read expression for a given
  table, taking into account the run lineage and distinctness columns.
  If the table has no `id_run` column or is the `runs_t` table itself,
  falls back to the default read expression. Otherwise, constructs a
  read expression that filters for the active `id_run` and its lineage,
  using the distinctness columns to determine which slices of data to
  include.

- `run_parallel_evoland()`: Run a worker function in parallel with an
  `evoland_db` instance; takes an (optional) worker cluster, a worker
  function, a set of items, and a parent `evoland_db` instance. If
  **no** cluster is passed, apply the worker serially. **If** a cluster
  is passed, each worker gets its own database instance derived from the
  parent database.
