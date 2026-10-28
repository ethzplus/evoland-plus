# Distance-decayed membership in a change map

Distance-decayed membership in a change map

## Usage

``` r
.change_membership(change_map, weight_matrix)
```

## Arguments

- change_map:

  Logical SpatRaster, `TRUE` where the change happened, see
  [`create_change_map()`](https://ethzplus.github.io/evoland-plus/reference/create_change_map.md)

- weight_matrix:

  Matrix of distance weights, see
  [`.build_weight_matrix()`](https://ethzplus.github.io/evoland-plus/reference/dot-build_weight_matrix.md)

## Value

SpatRaster with, for every cell, the weight of the nearest change within
the window (0 if there is none)
