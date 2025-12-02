# evoland utility functions to work with terra objects

Useful to coax terra raster and vector data into evoland tabular form.

## Usage

``` r
extract_using_coords_t(x, coords_t, na_omit = TRUE)
```

## Arguments

- x:

  The object to extract from; use "simple" extraction for rasters, i.e.
  no resampling is done.

- coords_t:

  A coords_t object containing coordinate points

## Value

A long data.table with `id_coord`, a `layer`/`attribute` column, and a
`value` column. NAs are omitted

## Functions

- `extract_using_coords_t()`: Extract values from a SpatRaster or
  SpatVector object using a (minimal) `coords_t`
