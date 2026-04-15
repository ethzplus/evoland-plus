# Create Runs Table

Creates a runs_t table that stores the identities, hierarchy, and
description of runs. Defaults to a table with one row with the base
`id_run := 0`. Runs are a feature that allows one to label data that is
either overriding base data (e.g. predictors being used for forecasting)
or produced by a specific model experiment (e.g. a given allocation of
land use change given a set of transition potential models and
allocation parameters).

## Usage

``` r
as_runs_t(x)

# S3 method for class 'runs_t'
print(x, nrow = 10, ...)

db_active_id_run(self, private, x)
```

## Arguments

- x:

  A list or data.frame coercible to a data.table

- nrow:

  see
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

## Value

A data.table of class "runs_t" with columns:

- `id_run`: Foreign key to runs_t

- `id_period`: Foreign key to periods_t

- `id_trans`: Foreign key to trans_meta_t

- `rate`: Transition rate (0 to 1)

## Methods (by generic)

- `print(runs_t)`: Print a runs_t object, passing params to data.table
  print

## Functions

- `db_active_id_run()`: Get or set the active run ID; error if no
  lineage is found
