# Create Transition-Predictor Relationship Table

Creates a trans_preds_t table based on the relationships between
transitions and predictors. This function establishes which predictors
are useful for modelling each transition type.

## Usage

``` r
as_trans_preds_t(x)

create_trans_preds_t()

# S3 method for class 'trans_preds_t'
print(x, nrow = 10, ...)
```

## Arguments

- nrow:

  see
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- db:

  An
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance with populated trans_meta_t and pred_meta_t tables

## Value

A data.table of class "trans_preds_t" with columns:

- `id_pred`: Foreign key to pred_meta_t

- `id_trans`: Foreign key to trans_meta_t

## Methods (by generic)

- `print(trans_preds_t)`: Print a trans_preds_t object, passing params
  to data.table print

## Functions

- `create_trans_preds_t()`: Create a transition-predictor relation, i.e.
  records the result of a predictor selection step.
