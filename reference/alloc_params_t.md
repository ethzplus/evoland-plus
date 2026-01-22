# Create Allocation Parameters Table

Creates a alloc_params_t table for storing transition model metadata and
serialized model objects. This function creates an empty table with
proper structure for storing fitted models.

## Usage

``` r
as_alloc_params_t(x)

# S3 method for class 'alloc_params_t'
print(x, nrow = 10, ...)

compute_alloc_params_single(lulc_ant, lulc_post, id_lulc_ant, id_lulc_post)
```

## Arguments

- x:

  A list or data.frame coercible to a data.table

- nrow:

  see
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- lulc_ant:

  SpatRast with LULC data for the anterior (earlier) period

- lulc_post:

  SpatRast with LULC data for the posterior (later) period

- id_lulc_ant:

  Integer ID of the anterior LULC class

- id_lulc_post:

  Integer ID of the posterior LULC class

## Value

A data.table of class "alloc_params_t" with columns:

- `id_perturbation`: The perturbation's ID - no. 1 == unperturbed

- `id_trans`: Foreign key to trans_meta_t

- ... various other columns describing allocation parameters and
  goodness of fit

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

- `compute_alloc_params_single()`: Compute allocation parameters for a
  single transition and period pair. This is an internal function used
  by `create_alloc_params_t`.
