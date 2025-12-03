# Create Transition Models Table

Creates a trans_models_t table for storing transition model metadata and
serialized model objects. This function creates an empty table with
proper structure for storing fitted models.

## Usage

``` r
as_trans_models_t(x)

# S3 method for class 'trans_models_t'
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

A data.table of class "trans_models_t" with columns:

- `id_trans`: Foreign key to trans_meta_t

- `model_family`: Model family (e.g., "rf", "glm", "bayesian")

- `model_params`: Map of model (hyper) parameters

- `goodness_of_fit`: Map of various measures of fit (e.g., ROC AUC,
  RMSE)

- `fit_call`: Character string of the original fit function call for
  reproducibility

- `model_obj_part`: BLOB of serialized model object for validation

- `model_obj_full`: BLOB of serialized model object for extrapolation

## Methods (by generic)

- `print(trans_models_t)`: Print a trans_models_t object, passing params
  to data.table print
