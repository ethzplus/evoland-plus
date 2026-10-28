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

db_active_id_run(self, private, y)
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

- self:

  an evoland_db instance

- private:

  an evoland_db private environment

- y:

  (optional) scalar integerish or NULL; if provided, sets the active run
  ID and lineage

## Value

A data.table of class "runs_t" with columns:

- `id_run`: Unique ID for each run; 0 is the base run

- `parent_id_run`: The run this run inherits data from; `NA` for the
  base run

- `description`: Human-readable description of the run

## Methods (by generic)

- `print(runs_t)`: Print a runs_t object, passing params to data.table
  print

## Functions

- `db_active_id_run()`: Get or set the active run ID; error if no
  lineage is found
