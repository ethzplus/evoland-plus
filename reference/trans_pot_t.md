# Calculate Transition Potential

Estimate transition potential at `id_period_post`. Based on the LULC at
`id_period_anterior`

## Usage

``` r
as_trans_pot_t(x)

# S3 method for class 'trans_pot_t'
print(x, nrow = 10, ...)

predict_trans_pot(self, id_period_post, select_score, select_maximize)
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

- select_score:

  character scalar, name of score/measure to identify best fitting model

- select_maximize:

  logical scalar, whether to maximize or minimize `select_score`

## Value

A data.table of class "trans_pot_t" with columns:

- `id_trans`: Foreign key to
  [`trans_meta_t()`](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md)

- `id_period_post`: Foreign key to
  [`periods_t()`](https://ethzplus.github.io/evoland-plus/reference/periods_t.md)

- `id_coord`: Foreign key to
  [`coords_t()`](https://ethzplus.github.io/evoland-plus/reference/coords_t.md)

- `value`: Map of model (hyper) parameters

A `trans_pot_t` object (invisibly); the same data are committed to the
DB.

## Methods (by generic)

- `print(trans_pot_t)`: Print a trans_pot_t object, passing params to
  data.table print

## Functions

- `predict_trans_pot()`: For each viable transition, predict the raw
  transition potential for a given period and store it in `trans_pot_t`
  in the database. Raw potentials are per-transition MLR3 model
  probabilities; they are **not** yet allocation-ready (not
  column-scaled to target rates, not row-closed). Use
  [`adjusted_trans_pot_v()`](https://ethzplus.github.io/evoland-plus/reference/evoland_db_views.md)
  to obtain allocation-ready values.
