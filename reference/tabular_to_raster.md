# Convert tabular LULC data to raster

Converts a `lulc_data_t` table (with coordinates) to a SpatRaster.
Useful for spatial analysis and validation that requires raster format.

## Usage

``` r
tabular_to_raster(lulc_data, coords_t, resolution = NULL)
```

## Arguments

- lulc_data:

  A `lulc_data_t` object or data.table with columns: id_coord, id_lulc,
  (optionally id_period)

- coords_t:

  A coords_t object with coordinate information. Must have `epsg` and
  optionally `resolution` attributes.

- resolution:

  Numeric, raster resolution in CRS units. If NULL, attempts to infer
  from coords_t attributes or data spacing.

## Value

SpatRaster with LULC values. If multiple periods present in lulc_data,
returns a multi-layer raster with one layer per period.
