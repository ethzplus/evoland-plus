# Rook-adjacency neighbour indices for a raster (C++)

Rook-adjacency neighbour indices for a raster (C++)

## Usage

``` r
raster_neighbors_cpp(nrow, ncol)
```

## Arguments

- nrow, ncol:

  Raster dimensions.

## Value

Named list `above`/`below`/`left`/`right`, each a 1-based cell index per
cell (row-major) with 0 meaning "no neighbour" (edge).
