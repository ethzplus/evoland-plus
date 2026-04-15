# Calculate Transition Potential

Estimate transition potential at `id_period_post`. Based on the LULC at
`id_period_anterior`

## Usage

``` r
as_trans_pot_t(x)

# S3 method for class 'trans_pot_t'
print(x, nrow = 10, ...)

predict_trans_pot(self, id_period_post, gof_criterion, gof_maximize)
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

  an
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance

- id_period_post:

  scalar integerish, passed to `self$pred_data_wide_v()`

## Value

A data.table of class "trans_pot_t" with columns:

- `id_trans`: Foreign key to
  [`trans_meta_t()`](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md)

- `id_period_post`: Foreign key to
  [`periods_t()`](https://ethzplus.github.io/evoland-plus/reference/periods_t.md)

- `id_coord`: Foreign key to
  [`coords_t()`](https://ethzplus.github.io/evoland-plus/reference/coords_t.md)

- `value`: Map of model (hyper) parameters

## Methods (by generic)

- `print(trans_pot_t)`: Print a trans_pot_t object, passing params to
  data.table print

## Functions

- `predict_trans_pot()`: For each viable transition, predict the
  transition potential for a given period, with cumulative probabilities
  for a single id_coord capped to 1; returns a `trans_pot_t` object
