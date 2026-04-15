# Create Allocation Parameters Table

Creates an alloc_params_t table for storing allocation parameters for
each transition and run. This function creates an empty table with the
proper structure for storing allocation parameters.

## Usage

``` r
as_alloc_params_t(x)

# S3 method for class 'alloc_params_t'
print(x, nrow = 10, ...)

isometry_from_elongation(
  elongation,
  curve = data.frame(isometry = c(0, 0.25, 0.5, 0.75, 1, 1.2, 1.5, 2), elongation =
    c(0.34, 0.334, 0.333, 0.328, 0.32, 0.172, 0.171, 0.17)),
  clamp = TRUE
)

compute_alloc_params_single(lulc_ant, lulc_post, id_lulc_ant, id_lulc_post)

create_alloc_params_t(self, n_perturbations = 5L, sd = 0.05)
```

## Arguments

- x:

  A list or data.frame coercible to a data.table

- nrow:

  see
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- elongation:

  Numeric vector of elongation values

- curve:

  Data frame with columns `isometry` and `elongation` defining the
  mapping curve

- clamp:

  Logical, whether to clamp values outside the curve range to min/max or
  to return NA

- lulc_ant:

  SpatRast with LULC data for the anterior (earlier) period

- lulc_post:

  SpatRast with LULC data for the posterior (later) period

- id_lulc_ant:

  Integer ID of the anterior LULC class

- id_lulc_post:

  Integer ID of the posterior LULC class

- self:

  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance to query

- n_perturbations:

  Integer number of randomly perturbed parameter sets to create per
  transition (default: 5)

- sd:

  Numeric standard deviation for random perturbations (default: 0.05)

## Value

A data.table of class "alloc_params_t" with columns:

- `id_run`: Foreign key to runs_t

- `id_trans`: Foreign key to trans_meta_t

- `mean_patch_size`: Mean area of new patches (in cell units)

- `patch_size_variance`: Standard deviation of patch area

- `patch_isometry`: Measure of patch shape regularity

- `frac_expander`: Fraction of transition cells adjacent to existing
  patches

- `frac_patcher`: Fraction of transition cells forming new patches

- `similarity`: Similarity metric for allocation parameters, see
  [`calc_fuzzy_similarity()`](https://ethzplus.github.io/evoland-plus/reference/calc_fuzzy_similarity.md)
  and
  [`eval_alloc_params_t()`](https://ethzplus.github.io/evoland-plus/reference/alloc_dinamica.md)

A named list with allocation parameters:

- mean_patch_size: Mean area of patches (hectares)

- patch_size_variance: Standard deviation of patch area (hectares)

- patch_isometry: Measure of patch shape regularity (0-1)

- frac_expander: Fraction of transition cells adjacent to old patches in
  \[0, 1\]

- frac_patcher: Fraction of transition cells forming new patches in \[0,
  1\]

## Methods (by generic)

- `print(alloc_params_t)`: Print a alloc_params_t object, passing params
  to data.table print

## Functions

- `isometry_from_elongation()`: Map patch elongation to Dinamica
  "isometry" patcher parameter, c.f. fig. 3.10 in Mazy, 2022
  https://theses.hal.science/tel-04382012v1

- `compute_alloc_params_single()`: Compute allocation parameters for a
  single transition and period pair. This is an internal function used
  by `create_alloc_params_t`.

- `create_alloc_params_t()`: Create allocation parameters for each
  transition by estimating patch shape and expansion/patch ratio from
  observed periods, then aggregate and perturb parameters for use in
  runs.
