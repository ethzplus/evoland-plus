# Guided Regularized Random Forest Learner

Guided Regularized Random Forest Learner

Guided Regularized Random Forest Learner

## Details

An
[mlr3::LearnerClassif](https://mlr3.mlr-org.com/reference/LearnerClassif.html)
that fits a Guided Regularized Random Forest (GRRF) for use in
[mlr3filters::FilterImportance](https://mlr3filters.mlr-org.com/reference/mlr_filters_importance.html)-based
feature selection.

The learner implements a two-stage approach:

1.  An initial unregularized random forest estimates variable importance
    scores.

2.  Those scores are normalized and used to compute per-feature
    split-selection weights (`coefReg`), penalizing less informative
    predictors.

3.  A second "guided regularized" forest is trained with those weights,
    and its impurity importance scores are exposed via `$importance()`.

Class-balanced sample weights are computed automatically from the target
column, so no external weight vector is required.

## Parameters

- `gamma` (`numeric [0, 1]`, default `0.5`): Guidance coefficient. `0` =
  unguided regularized forest (equal penalty for all features); `1` =
  strongest guiding effect (most important features penalized least).

- `num.trees` (`integer >= 1`, default `500`): Number of trees in each
  forest.

- `max.depth` (`integer >= 0`, default `100`): Maximum tree depth (`0` =
  unlimited).

## References

Deng, H., & Runger, G. (2013). Gene selection with guided regularized
random forest. *Pattern Recognition*, 46(12), 3483-3489.
<https://doi.org/10.1016/j.patcog.2013.05.018> and Wundervald, B. et al.
(2020). Generalizing Gain Penalization for Feature Selection in
Tree-Based Models. *IEEE Access*, Vol. 8, 190231 - 190239.
<https://doi.org/10.1109/ACCESS.2020.3032095>

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3::LearnerClassif`](https://mlr3.mlr-org.com/reference/LearnerClassif.html)
-\> `LearnerClassifGrrf`

## Methods

### Public methods

- [`LearnerClassifGrrf$new()`](#method-LearnerClassifGrrf-new)

- [`LearnerClassifGrrf$importance()`](#method-LearnerClassifGrrf-importance)

- [`LearnerClassifGrrf$clone()`](#method-LearnerClassifGrrf-clone)

Inherited methods

- [`mlr3::Learner$base_learner()`](https://mlr3.mlr-org.com/reference/Learner.html#method-base_learner)
- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/reference/Learner.html#method-configure)
- [`mlr3::Learner$encapsulate()`](https://mlr3.mlr-org.com/reference/Learner.html#method-encapsulate)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$print()`](https://mlr3.mlr-org.com/reference/Learner.html#method-print)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/reference/Learner.html#method-reset)
- [`mlr3::Learner$selected_features()`](https://mlr3.mlr-org.com/reference/Learner.html#method-selected_features)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)
- [`mlr3::LearnerClassif$predict_newdata_fast()`](https://mlr3.mlr-org.com/reference/LearnerClassif.html#method-predict_newdata_fast)

------------------------------------------------------------------------

### Method `new()`

Initialise the learner with its parameter set.

#### Usage

    LearnerClassifGrrf$new()

------------------------------------------------------------------------

### Method `importance()`

Return impurity importance scores from the fitted GRRF.

#### Usage

    LearnerClassifGrrf$importance()

#### Returns

Named numeric vector of importance scores, sorted decreasingly. All
features present in the training task are included (unused features
receive a score of 0).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerClassifGrrf$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
library(mlr3)
library(mlr3filters)

learner <- LearnerClassifGrrf$new()
learner$param_set$values <- list(gamma = 0.9, num.trees = 50L)

filter <- mlr3filters::FilterImportance$new(learner = learner)
task <- mlr3::tsk("sonar")
filter$calculate(task)
as.data.table(filter)
} # }
```
