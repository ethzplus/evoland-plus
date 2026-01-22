# Run a function in parallel over a list of items

A wrapper around
[`parallel::parLapply()`](https://rdrr.io/r/parallel/clusterApply.html)
that handles `evoland_db` connection management. It uses a PSOCK cluster
(via
[`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html))
to support Windows and ensures a clean environment for each worker.

## Usage

``` r
run_parallel_task(items, worker_fun, db, cores = 1L, ...)
```

## Arguments

- items:

  A list or vector of items to iterate over.

- worker_fun:

  A function to apply to each item. Must accept `item` as the first
  argument and `db` (the database instance) as the second argument.

- db:

  The current
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance (typically `self`).

- cores:

  Integer. Number of cores to use.

- ...:

  Additional arguments passed to `worker_fun`.

## Value

A list of results, same as
[`lapply()`](https://rdrr.io/r/base/lapply.html).
