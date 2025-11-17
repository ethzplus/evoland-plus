# Create Allocation Parameters Table

Creates a alloc_params_t table for storing transition model metadata and
serialized model objects. This function creates an empty table with
proper structure for storing fitted models.

## Usage

``` r
as_alloc_params_t(x)

# S3 method for class 'alloc_params_t'
print(x, nrow = 10, ...)
```

## Arguments

- x:

  A list or data.frame coercible to a data.table

- nrow:

  see
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

## Value

A data.table of class "alloc_params_t" with columns:

- `id_trans`: Foreign key to trans_meta_t

- `id_period`: Foreign key to periods_t

- `alloc_params`: Map of model (hyper) parameters

- `goodness_of_fit`: Map of various measures of fit (e.g., ROC AUC)

## Methods (by generic)

- `print(alloc_params_t)`: Print a alloc_params_t object, passing params
  to data.table print
