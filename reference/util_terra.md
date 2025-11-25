# evoland utility functions to work with terra objects

Useful to coax terra raster and vector data into evoland tabular form.

## Usage

``` r
extract_using_coords_t(x, coords_t, na_omit = TRUE)

compute_neighbors(
  coords_t,
  max_distance,
  distance_breaks = NULL,
  resolution = 100
)
```

## Arguments

- x:

  The object to extract from; use "simple" extraction for rasters, i.e.
  no resampling is done.

- coords_t:

  A coords_t object containing coordinate points

- max_distance:

  Maximum distance to search for neighbors (in same units as coordinates
  and resolution)

- distance_breaks:

  Optional numeric vector defining distance class boundaries. If NULL,
  no distance classification is performed. If provided, must have at
  least 2 elements defining interval breaks.

- resolution:

  Grid cell size for rasterization (default: 100.0, in same units as
  coordinates)

## Value

A long data.table with `id_coord`, a `layer`/`attribute` column, and a
`value` column. NAs are omitted

A data.table with columns:

- id_coord_origin: ID of the origin coordinate

- id_coord_neighbor: ID of the neighboring coordinate

- distance: Distance between origin and neighbor

- distance_class: Factor indicating distance class (if distance_breaks
  provided)

## Functions

- `extract_using_coords_t()`: Extract values from a SpatRaster or
  SpatVector object using a (minimal) `coords_t`

- `compute_neighbors()`: Compute neighboring coordinates within
  specified distances. In order to be computationally feasible, the
  coordinates' IDs are rasterized before their actual Euclidean distance
  is calculated. If coordinates are so close that they get rasterized to
  the same cell, the first one is used and a warning is emitted. If this
  happens, try again using a lower resolution.
