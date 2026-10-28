# Cross-tabulation of land use between two periods

Counts the cells by their land use class in one period and in another:
the transition matrix, in long form. Each period is read from its own
run, through that run's lineage, so the same call gives observed change
(both periods from one run), simulated change (a simulated run from its
initial period), or agreement between an observed and a simulated map of
the same period (two runs, one period).

## Usage

``` r
lulc_crosstab_v(
  self,
  id_period_anterior,
  id_period_post,
  id_run_anterior = self$id_run,
  id_run_post = id_run_anterior
)
```

## Arguments

- self:

  An
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance

- id_period_anterior, id_period_post:

  Integer, the two periods to compare

- id_run_anterior:

  Integer, run to read `id_period_anterior` from; defaults to the active
  run

- id_run_post:

  Integer, run to read `id_period_post` from; defaults to
  `id_run_anterior`

## Value

A
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with `id_lulc_anterior`, `id_lulc_posterior` and `n_cells`, one row per
pair of classes that occurs. Cells missing from either map are ignored.
Use
[`data.table::dcast()`](https://rdrr.io/pkg/data.table/man/dcast.data.table.html)
for the matrix form.
