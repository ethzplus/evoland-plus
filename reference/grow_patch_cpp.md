# Grow a single land-use patch from a pivot cell (C++)

Low-level patch grower (the building block where the mutable working
layer and the immutable anterior reference are distinct), kept for
direct use / unit testing. On success the allocated cells are written
back into `landscape` (set to `to_class`); on failure nothing is
committed. Neighbour vectors are 1-based with 0 == no neighbour, as
produced by
[`raster_neighbors_cpp()`](https://ethzplus.github.io/evoland-plus/reference/raster_neighbors_cpp.md).

## Usage

``` r
grow_patch_cpp(
  landscape,
  ant_landscape,
  probs,
  nbr_above,
  nbr_below,
  nbr_left,
  nbr_right,
  pivot,
  target_area,
  from_class,
  to_class,
  elongation,
  ncol,
  avoid_aggregation = FALSE
)
```

## Arguments

- landscape:

  IntegerVector of current LULC values (NA_INTEGER = no-data).

- ant_landscape:

  IntegerVector of anterior (immutable) LULC values.

- probs:

  NumericVector of transition probabilities (length == landscape).

- nbr_above, nbr_below, nbr_left, nbr_right:

  Neighbour index vectors.

- pivot:

  1-based pivot cell index.

- target_area:

  Target patch size (cells).

- from_class, to_class:

  Source/target LULC classes.

- elongation:

  Target elongation in \[0, 1\] (0 = isometric).

- ncol:

  Raster column count.

- avoid_aggregation:

  If TRUE, the patch is all-or-nothing and fails if it would merge with
  another patch or cannot reach `target_area`.

## Value

1-based integer vector of allocated cell indices (incl. pivot), or empty
if the patch failed / the pivot is not an available `from_class` cell.
