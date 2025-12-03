# GLM Model Fitting for Transition Models

Fits a generalized linear model (GLM) with quasibinomial family for
transition modeling. The quasibinomial family is recommended over
binomial as it better handles overdispersion in the data.

## Usage

``` r
fit_glm(data, result_col = "result", ...)
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

A fitted GLM model object, optionally butchered to reduce memory
footprint

## Details

The function:

- Uses quasibinomial family to handle overdispersion

- Automatically detects predictor columns (those starting with
  "id_pred\_")

- Applies butcher::butcher() if the package is available to reduce model
  size
