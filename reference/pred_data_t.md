# Create Predictor Data Table

Construct and validate a `pred_data_t` objects, which are used to store
predictor data. Different data types (float, int, bool) have different
subclasses (pred_data_t_float, ...)

## Usage

``` r
as_pred_data_t(x)

# S3 method for class 'pred_data_t'
print(x, nrow = 10, ...)

pred_data_available_v(self)

trans_pred_data_v(self, id_trans, id_pred, ordered = FALSE)

pred_data_wide_v(self, id_trans, id_period_anterior)

set_pred_coltypes(result, pred_meta_t)
```

## Arguments

- x:

  Coercible to data.table

- nrow:

  see
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- self, :

  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance to query

- id_trans:

  Integer transition ID, see
  [trans_meta_t](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md)

- id_pred:

  Optional integer vector of predictor IDs to include; if missing, use
  all predictor IDs from
  [pred_meta_t](https://ethzplus.github.io/evoland-plus/reference/pred_meta_t.md)

- ordered:

  - if TRUE, order output by id_period then id_coord (otherwise no
    guaranteed order; need ordering for reproducible subsampling)

- id_period_anterior:

  Scalar integerish of anterior period ID, i.e. we get the predictors
  that explain the processes over the next N years

- result:

  data.table with predictor columns named `id_pred_{n}`

- pred_meta_t:

  see
  [pred_meta_t](https://ethzplus.github.io/evoland-plus/reference/pred_meta_t.md)

- type:

  Character string specifying the data type: "float", "int", or "bool"

## Value

A data.table of class "pred_data_t\_" and "pred_data_t" with columns:

- `id_run`: Foreign key to runs_t

- `id_pred`: Foreign key to pred_meta_t

- `id_coord`: Foreign key to coords_t

- `id_period`: Foreign key to periods_t

- `value`: Predictor value (type depends on subclass)

data.table with columns id_coord, id_period, did_transition (bool), and
one column per predictor (`id_pred_{n}`)

## Methods (by generic)

- `print(pred_data_t)`: Print a pred_data_t object, passing params to
  data.table print

## Functions

- `pred_data_available_v()`: Check if predictor data is complete, i.e.
  each entry in
  [pred_meta_t](https://ethzplus.github.io/evoland-plus/reference/pred_meta_t.md)
  is either present in period 0 or for all other periods for a given
  run.

- `trans_pred_data_v()`: Get transitions along with their predictor data
  in a wide data.table

- `pred_data_wide_v()`: Get predictor data in a wide data.table for
  transition potential prediction (cols `id_coord, id_pred_{n}`)

- `set_pred_coltypes()`: In-place casting of predictor columns to their
  correct data types based on pred_meta_t; also fills NA values with
  fill_value from pred_meta_t if specified
