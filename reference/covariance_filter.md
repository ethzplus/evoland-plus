# Two stage covariate filtering

The `covariance_filter` returns a set of covariates for land use land
cover change (LULCC) models based on a two-stage variable selection: a
first statistical fit estimates a covariate's quality for a given
prediction task. A second step selects all variables below a given
correlation threshold: We iterate over a correlation matrix ordered in
the first step. Starting within the leftmost column, all rows (i.e.
candidates) greater than the given threshold are dropped from the full
set of candidates. This candidate selection is retained and used to
select the next column, until no further columns are left to
investigate. The columns that were iterated over are those returned as a
character vector of selected variable names.

## Usage

``` r
covariance_filter(
  data,
  result_col = "result",
  rank_fun = rank_poly_glm,
  weights = compute_balanced_weights(data[[result_col]]),
  corcut = 0.7,
  ...
)

rank_poly_glm(x, y, weights = NULL, ...)

compute_balanced_weights(trans_result, legacy = FALSE)

select_by_correlation(cor_mat, corcut)
```

## Arguments

- data:

  A data.table of target variable and candidate covariates to be
  filtered; wide format with one predictor per column.

- result_col:

  Name of the column representing the transition results (0: no trans,
  1: trans)

- rank_fun:

  Optional function to compute ranking scores for each covariate. Should
  take arguments (x, y, weights, ...) and return a single numeric value
  (lower = better). Defaults to polynomial GLM p-value ranking.

- weights:

  Optional weights vector

- corcut:

  Correlation cutoff threshold

- ...:

  Additional arguments passed to rank_fun.

- x:

  A numeric vector representing a single covariate

- y:

  A binary outcome vector (0/1)

- trans_result:

  Binary outcome vector (0/1)

- legacy:

  Bool, use legacy weighting?

- cor_mat:

  Absolute correlation matrix

## Value

A set of column names (covariates) to retain

## Details

The function first ranks covariates using the provided ranking function
(default: quasibinomial polynomial GLM). Then, it iteratively removes
highly (Pearson) correlated variables based on the correlation cutoff
threshold, preserving variables in order of their ranking. See
<https://github.com/ethzplus/evoland-plus-legacy/blob/main/R/lulcc.covfilter.r>
for where the concept came from. The original author was Antoine Adde,
with edits by Benjamin Black. A similar mechanism is found in
<https://github.com/antadde/covsel/>.

## Functions

- `rank_poly_glm()`: Default ranking function using polynomial GLM.
  Returns the lower p value for each of the polynomial terms

- `compute_balanced_weights()`: Compute class-balanced weights for
  imbalanced binary outcomes; returns a numeric vector

- `select_by_correlation()`: Implements the iterative selection
  procedure.
