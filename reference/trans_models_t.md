# Create Transition Models Table

Creates a trans_models_t table for storing transition model metadata and
serialized model objects. This function creates an empty table with
proper structure for storing fitted models.

## Usage

``` r
as_trans_models_t(x)

fit_partial_models(
  self,
  fit_fun,
  gof_fun,
  sample_frac = 0.7,
  seed = NULL,
  cluster = NULL,
  ...
)

fit_full_models(self, gof_criterion, gof_maximize, cluster = NULL)

# S3 method for class 'trans_models_t'
print(x)
```

## Arguments

- x:

  A list or data.frame coercible to a data.table

- self, :

  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance to query for transitions and predictor data

- fit_fun:

  Function that takes a data.frame with predictors and did_transition
  columns and returns a fitted model object. The data argument is passed
  as the first argument to the function, and additional arguments can be
  passed via ...

- gof_fun:

  Function that takes a fitted model object and a test data.frame and
  returns a list of goodness-of-fit metrics. The model argument is
  passed as the first argument and the test_data argument is passed as
  the second argument.

- sample_frac:

  Numeric between 0 and 1 indicating the fraction of data to use for
  training the partial models. The rest is used for testing and
  calculating goodness-of-fit metrics. Default is 0.7 (70% training, 30%
  testing).

- seed:

  Optional integer seed for reproducible subsampling.

- cluster:

  An optional cluster object created by
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
  or `mirai::make_cluster()`.

- gof_criterion:

  Character string specifying which goodness-of-fit metric to use for
  selecting the best partial model for each transition (e.g., "roc_auc",
  "rmse").

- gof_maximize:

  Logical indicating whether to select the model with the maximum (TRUE)
  or minimum (FALSE) value of the specified goodness-of-fit criterion.
  Default is TRUE.

- partial_models:

  A trans_models_t table containing the fitted partial models and their
  goodness-of-fit metrics.

## Value

A data.table of class "trans_models_t" with columns:

- `id_run`: Foreign key to runs_t

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

- `print(trans_models_t)`: Print a trans_models_t object as yaml-style
  list

## Functions

- `fit_partial_models()`: Fit partial models for each viable transition
  and store results in a trans_models_t table.

- `fit_full_models()`: Fit full models for each transition based on the
  best partial model according to a specified goodness-of-fit criterion.
