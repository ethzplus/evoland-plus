# Random Forest Model Fitting for Transition Models

Fits a random forest model using the ranger package for transition
modeling. Uses observation-based weighting and stratified downsampling
to handle class imbalance.

## Usage

``` r
fit_ranger(data, result_col = "result", num.trees = 500, ...)
```

## Arguments

- data:

  A data.table containing the result column and predictor columns
  (prefixed with "id_pred\_")

- result_col:

  Name of the column representing the transition results (logical: TRUE
  = transition occurred, FALSE = no transition)

- ...:

  Additional arguments (currently unused, for future extensibility)

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

- mtry = floor(sqrt(n_predictors))

- min.node.size = 1
