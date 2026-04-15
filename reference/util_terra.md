# evoland utility functions to work with terra objects

Useful to coax terra raster and vector data into evoland tabular form.

## Usage

``` r
extract_using_coords_t(x, coords, na_omit = TRUE)

tabular_to_raster(data, coords, value_col = "id_lulc", resolution = NULL)
```

## Arguments

- x:

  The object to extract from; use "simple" extraction for rasters, i.e.
  no resampling is done.

- coords:

  A
  [coords_t](https://ethzplus.github.io/evoland-plus/reference/coords_t.md)
  object with coordinate information. Must have `epsg` and optionally
  `resolution` attributes.

- na_omit:

  Logical, whether to omit rows with NA values in the output

- data:

  A data.table with column id_coord and value_col

- resolution:

  Numeric, raster resolution in CRS units. If `NULL`, use "resolution"
  attribute on coords, or estimate from coord spacing.

## Value

[terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with LULC values. If multiple periods present in data, returns a
multi-layer raster with one layer per period.

## Functions

- `extract_using_coords_t()`: Extract values from a SpatRaster or
  SpatVector object using a (minimal) `coords_t` object. Returns a long
  data.table with `id_coord`, `layer`/`attribute`, and `value`.

- `tabular_to_raster()`: Converts a table with id_coord information to a
  SpatRaster. Useful for spatial analysis and validation that requires
  raster format.
