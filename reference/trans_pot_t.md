# Calculate Transition Potential

Estimate transition potential at `id_period_post`. Based on the LULC at
`id_period_anterior`

## Usage

``` r
as_trans_pot_t(x)

# S3 method for class 'trans_pot_t'
print(x, nrow = 10, ...)

predict_trans_pot(
  self,
  id_period_post,
  select_score,
  select_maximize,
  force = FALSE
)
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

  scalar integerish, passed to
  [`pred_data_wide_v()`](https://ethzplus.github.io/evoland-plus/reference/pred_data_t.md)

- select_score:

  character scalar, name of score/measure to identify best fitting model

- select_maximize:

  logical scalar, whether to maximize or minimize `select_score`

- force:

  logical, Force prediction even if a prediction is found

## Value

A data.table of class "trans_pot_t" with columns:

- `id_trans`: Foreign key to
  [`trans_meta_t()`](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md)

- `id_period_post`: Foreign key to
  [`periods_t()`](https://ethzplus.github.io/evoland-plus/reference/periods_t.md)

- `id_coord`: Foreign key to
  [`coords_t()`](https://ethzplus.github.io/evoland-plus/reference/coords_t.md)

- `value`: Map of model (hyper) parameters

`predict_trans_pot()`: called for side effect; commit `trans_pot_t` to
database

## Methods (by generic)

- `print(trans_pot_t)`: Print a trans_pot_t object, passing params to
  data.table print

## Functions

- `predict_trans_pot()`: For each viable transition in current `id_run`,
  predict the raw transition potential for a given period and store it
  in `trans_pot_t` in the database. Raw potentials are per-transition
  MLR3 model probabilities; they are **not** yet allocation-ready (not
  column-scaled to target rates, not row-closed to max probability of
  1). Use
  [`adjusted_trans_pot_v()`](https://ethzplus.github.io/evoland-plus/reference/evoland_db_views.md)
  to obtain allocation-ready values. Set
  `options(evoland.use_prefetch_predict=TRUE)` to prefetch all
  predictors; this causes higher memory pressure but only needs to go to
  disk once. The learners have `parallel_predict` enabled, see
  [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html): the
  prediction task is automatically chunked out to any
  [future](https://future.futureverse.org/) workers available.
