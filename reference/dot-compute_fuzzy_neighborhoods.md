# Compute fuzzy neighborhood vectors for all cells in a map

Compute fuzzy neighborhood vectors for all cells in a map

## Usage

``` r
.compute_fuzzy_neighborhoods(rast, categories, weight_matrix)
```

## Arguments

- rast:

  SpatRaster, categorical map

- categories:

  Vector of category values

- weight_matrix:

  Matrix of distance weights

## Value

List of SpatRasters, one per category, containing fuzzy memberships
