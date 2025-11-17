# Create LULC Data Table

Creates an `lulc_data_t` table and validates that it matches the schema.

## Usage

``` r
as_lulc_data_t(x)

# S3 method for class 'lulc_data_t'
print(x, nrow = 10, ...)
```

## Arguments

- x:

  An object that can be passed to
  [`data.table::setDT()`](https://rdatatable.gitlab.io/data.table/reference/setDT.html)

- nrow:

  see
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

## Value

A data.table of class "lulc_data_t" with columns:

- `id_coord`: Foreign key to coords_t

- `id_lulc`: Foreign key to lulc_meta_t

- `id_period`: Foreign key to periods_t

## Methods (by generic)

- `print(lulc_data_t)`: Print an lulc_data_t object, passing params to
  data.table print
