# Create Transition-Predictor Relationship Table

Creates a trans_preds_t table based on the relationships between
transitions and predictors. This function establishes which predictors
are useful for modelling each transition type.

## Usage

``` r
as_trans_preds_t(x)

# S3 method for class 'trans_preds_t'
print(x, nrow = 10, ...)

set_full_trans_preds(self, overwrite = FALSE)

get_pruned_trans_preds_t(
  self,
  filter_fun = covariance_filter,
  cluster = NULL,
  ordered_pred_data = FALSE,
  ...
)
```

## Arguments

- nrow:

  see
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- overwrite:

  Bool, should a potentially existing table be overwritten?

- filter_fun:

  A function that takes a transition-predictor data (cf.
  trans_pred_data_v) and returns a character vector of column names to
  keep, see e.g.
  [covariance_filter](https://ethzplus.github.io/evoland-plus/reference/covariance_filter.md)

- cluster:

  An optional cluster object, see
  [run_parallel_evoland](https://ethzplus.github.io/evoland-plus/reference/evoland_db_util.md)

- ordered_pred_data:

  Bool, should the predictor data be ordered? needed for fully
  deterministic behavior

- db:

  An
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance with populated trans_meta_t and pred_meta_t tables

- na_value:

  Value to use for missing data when retrieving predictor data

## Value

A data.table of class "trans_preds_t" with columns:

- `id_run`: Foreign key to runs_t

- `id_pred`: Foreign key to pred_meta_t

- `id_trans`: Foreign key to trans_meta_t

## Methods (by generic)

- `print(trans_preds_t)`: Print a trans_preds_t object, passing params
  to data.table print

## Functions

- `set_full_trans_preds()`: Set an initial full set of transition /
  predictor relations

- `get_pruned_trans_preds_t()`: Get a pruned set of transition-predictor
  relationships based on a filtering function
