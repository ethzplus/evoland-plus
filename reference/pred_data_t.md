# Create Predictor Data Table

Construct and validate a `pred_data_t` objects, which are used to store
predictor data. Different data types (float, int, bool) have different
subclasses (pred_data_t_float, ...)

## Usage

``` r
as_pred_data_t(x, type)

# S3 method for class 'pred_data_t'
print(x, nrow = 10, ...)
```

## Arguments

- x:

  Coercible to data.table

- type:

  Character string specifying the data type: "float", "int", or "bool"

- nrow:

  see
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

## Value

A data.table of class "pred_data_t\_" and "pred_data_t" with columns:

- `id_pred`: Foreign key to pred_meta_t

- `id_coord`: Foreign key to coords_t

- `id_period`: Foreign key to periods_t

- `value`: Predictor value (type depends on subclass)

## Methods (by generic)

- `print(pred_data_t)`: Print a pred_data_t object, passing params to
  data.table print
