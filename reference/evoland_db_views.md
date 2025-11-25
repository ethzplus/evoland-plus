# Views on the evoland-plus data model

Functions to generate views on the database

## Usage

``` r
make_pred_sources_v(self, private)

make_lulc_meta_long_v(self, private)

make_coords_minimal(self, private)
```

## Functions

- `make_pred_sources_v()`: Retrieve a table of distinct predictor urls
  and their md5sum

- `make_lulc_meta_long_v()`: Return a `lulc_meta_long_v` instance, i.e.
  unrolled `lulc_meta_t`.

- `make_coords_minimal()`: Minimal coordinate representation (id_coord,
  lon, lat)
