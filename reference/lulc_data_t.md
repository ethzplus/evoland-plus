# Create LULC Data Table

Creates an `lulc_data_t` table and validates that it matches the schema.

## Usage

``` r
as_lulc_data_t(x)

# S3 method for class 'lulc_data_t'
print(x, nrow = 10, ...)

lulc_data_as_rast(self, id_period = NULL)
```

## Arguments

- x:

  An object that can be passed to
  [`data.table::setDT()`](https://rdrr.io/pkg/data.table/man/setDT.html)

- nrow:

  see
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- id_period:

  Integer or vector of integers specifying which periods to retrieve;
  returned as layers.

## Value

A data.table of class "lulc_data_t" with columns:

- `id_run`: Foreign key to runs_t

- `id_period`: Foreign key to periods_t

- `id_lulc`: Foreign key to lulc_meta_t

- `id_coord`: Foreign key to coords_t

## Methods (by generic)

- `print(lulc_data_t)`: Print an lulc_data_t object, passing params to
  data.table print

## Functions

- `lulc_data_as_rast()`: Retrieve LULC data as SpatRaster
