# Create Transition Models Table

Creates a trans_models_t table for storing transition model metadata and
serialized model objects. This function creates an empty table with
proper structure for storing fitted models via the mlr3 interface.

## Usage

``` r
as_trans_models_t(x)

fit_partial_models(
  self,
  learner,
  measures,
  sample_frac = 0.7,
  seed = NULL,
  cluster = NULL
)

fit_full_models(
  self,
  learner = NULL,
  select_score = NULL,
  select_maximize = TRUE,
  cluster = NULL
)

# S3 method for class 'trans_models_t'
print(x, ...)

get_crossval_plots(self, id_run = NULL, id_trans = NULL)
```

## Arguments

- x:

  A list or data.frame coercible to a data.table

- self:

  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance

- learner:

  An mlr3 `Learner` or `AutoTuner` object for direct-learner mode that
  supports twoclass classification. Its `predict_type` is coerced to
  `"prob"` if not already set. Must be `NULL` when `select_score` is
  provided.

- measures:

  Either a character vector of mlr3 measure IDs (e.g.
  `c("classif.auc", "classif.acc")`) or a list of instantiated mlr3
  `Measure` objects (e.g. `list(mlr3::msr("classif.auc"))`). Character
  IDs are converted via
  [`mlr3::msrs()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html)
  internally. Results are written to `crossval_score`.

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
  or
  [`mirai::make_cluster()`](https://mirai.r-lib.org/reference/make_cluster.html).

- select_score:

  Character string; mlr3 measure ID (e.g. `"classif.auc"`) used to rank
  partial models in score-select mode. Must be `NULL` when `learner` is
  provided.

- select_maximize:

  Logical; if `TRUE` (default) the model with the highest `select_score`
  is selected; if `FALSE`, the lowest. Only used in score-select mode.

- ...:

  ignored

- id_run:

  Optional integer; filter by run ID.

- id_trans:

  Optional integer; filter by transition ID.

## Value

A data.table of class "trans_models_t" with columns:

- `id_run`: Foreign key to runs_t

- `id_trans`: Foreign key to trans_meta_t

- `learner_id`: mlr3 twoclass
  [LearnerClassif](https://mlr3.mlr-org.com/reference/LearnerClassif.html)
  key, e.g. `"classif.ranger"`

- `learner_params`: MAP of atomic scalar learner hyperparameters for
  querying; complete hyperparameters are captured by `learner_spec`

- `learner_spec`: BLOB of serialized untrained mlr3 `Learner`; for
  AutoTuners, this is the optimal inner learner after tuning

- `crossval_score`: MAP of cross-validation performance scores (from
  `prediction$score(measures)`)

- `crossval_predictions`: BLOB of serialized mlr3 `PredictionClassif` on
  the held-out test split

- `learner_full`: BLOB of serialized trained mlr3 `Learner` fitted on
  the full dataset, used for extrapolation

## Methods (by generic)

- `print(trans_models_t)`: Print a trans_models_t object as yaml-style
  list; additional arguments silently ignored

## Functions

- `fit_partial_models()`: Fit partial (cross-validation) models for each
  viable transition; returns a trans_models_t object with one row per
  viable transition, containing the learner identity, serialized spec,
  cross-validation scores (`crossval_score`), and serialized held-out
  predictions (`crossval_predictions`).

- `fit_full_models()`: Fit full models (trained on the complete dataset)
  for each viable transition and return a trans_models_t object with
  `learner_full` populated. Two mutually exclusive modes are supported:

  - **Direct-learner mode** (`learner` provided, `select_score`
    omitted): a fresh clone of `learner` is trained on the full data for
    each transition. `crossval_score` and `crossval_predictions` will be
    `NULL` in the result. Does not require a prior call to
    `fit_partial_models()`.

  - **Score-select mode** (`select_score` provided, `learner` omitted):
    selects the best partial model per transition by `select_score`,
    reconstructs its learner from `learner_spec`, and retrains on the
    full data. Requires `fit_partial_models()` to have been run first.

- `get_crossval_plots()`: Deserialize cross-validation predictions and
  return plots via
  [`mlr3viz::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
  Requires the `mlr3viz` package.
