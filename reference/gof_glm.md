# Goodness of Fit Evaluation for GLM Models

Evaluates the goodness of fit for a GLM model on test data, computing
multiple performance metrics including correlation, MSE, and AUC.

## Usage

``` r
gof_glm(model, test_data, result_col = "result", ...)
```

## Arguments

- model:

  A fitted GLM model object (from fit_glm)

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

- `n_test`: Number of test observations

## Details

The function uses the pROC package for AUC calculation if available. If
pROC is not installed, AUC will be NA.
