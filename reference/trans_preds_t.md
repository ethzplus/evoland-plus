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

get_pred_filter_score(
  self,
  filter,
  cluster = NULL,
  ordered_pred_data = FALSE,
  ...
)
```

## Arguments

- x:

  A list or data.frame coercible to a trans_preds_t object. If missing,
  an empty table will be created.

- nrow:

  see
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- ...:

  Additional arguments passed to `flt` if `filter` is a character string

- self:

  An
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance with populated trans_meta_t and pred_meta_t tables

- overwrite:

  Bool, should a potentially existing table be overwritten?

- filter:

  An
  [mlr3filters::Filter](https://mlr3filters.mlr-org.com/reference/Filter.html)
  object or a character string specifying the filter method, retrieved
  via
  [mlr3filters::flt](https://mlr3filters.mlr-org.com/reference/flt.html).
  Note that your filter must be compatible with the feature data types;
  compare your `pred_meta_t` table to <https://mlr3filters.mlr-org.com>
  for filter compatibility.

- cluster:

  An optional cluster object, see
  [run_parallel_evoland](https://ethzplus.github.io/evoland-plus/reference/evoland_db_util.md)

- ordered_pred_data:

  Bool, should the predictor data be ordered? Needed for fully
  deterministic behavior

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

- `get_pred_filter_score()`: Get a filter score for all
  transition-predictor relationships based on mlr3filters. Returns
  trans_preds_t with an additional column named after the filter\$id.
  The filter score can be used for feature selection: simply subset
  according to the score and overwrite trans_preds_t in the database
  using `db$trans_preds_t <- trans_preds_t[score > threshold]` or
  similar.
