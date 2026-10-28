# Where land use changed between two maps

Where land use changed between two maps

## Usage

``` r
create_change_map(initial_map, final_map, from_class = NULL, to_class = NULL)
```

## Arguments

- initial_map, final_map:

  SpatRasters on the same grid

- from_class, to_class:

  Integer, optional: only count this transition. Give both or neither.

## Value

Logical SpatRaster: `TRUE` where the land use changed (from `from_class`
to `to_class`, if given), `FALSE` where it did not, `NA` where either
map is `NA`.
