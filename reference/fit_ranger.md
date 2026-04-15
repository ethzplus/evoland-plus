# Random Forest Model Fitting for Transition Models

Fits a random forest model using the ranger package for transition
modeling. Uses observation-based weighting and stratified downsampling
to handle class imbalance.

## Usage

``` r
fit_ranger(data, num.trees = 100, max.depth = 100, ...)
```

## Arguments

- data:

  A data.table containing the did_transition column and predictor
  columns (prefixed with "id_pred\_")

- num.trees:

  Number of trees to grow in the random forest (default: 100)

- max.depth:

  Maximum depth of each tree (default: 100)

- ...:

  Additional arguments passed to
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)

## Value

A fitted ranger model object, optionally butchered to reduce memory
footprint

## Details

The function:

- Uses ranger for efficient random forest implementation

- Applies observation-based weights (same approach as grrf_filter)

- Uses stratified downsampling via sample.fraction

- Returns probability predictions for the positive class

- Computes variable importance using impurity measure

- Applies butcher::butcher() if available to reduce model size

Default hyperparameters:

- num.trees = 500

- min.node.size = 1
