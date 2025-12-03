# Goodness of Fit Evaluation for Random Forest Models

Evaluates the goodness of fit for a ranger random forest model on test
data, computing multiple performance metrics including correlation, MSE,
AUC, and out-of-bag error.

## Usage

``` r
gof_ranger(model, test_data, result_col = "result", ...)
```

## Arguments

- model:

  A fitted ranger model object (from fit_ranger)

- test_data:

  A data.table containing test data with the same structure as training
  data

- result_col:

  Name of the column representing the transition results

- ...:

  Additional arguments (currently unused, for future extensibility)

## Value

A named list containing:

- `cor`: Pearson correlation between predictions and actual values

- `mse`: Mean squared error

- `auc`: Area under the ROC curve (if pROC package is available)

- `oob_error`: Out-of-bag prediction error from the model

- `n_test`: Number of test observations

## Details

The function extracts probability predictions for the TRUE class from
the ranger model. It uses the pROC package for AUC calculation if
available. If pROC is not installed, AUC will be NA.
