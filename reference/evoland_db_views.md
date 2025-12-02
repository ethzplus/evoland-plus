# Views on the evoland-plus data model

This file adds view active bindings and methods to the `evoland_db`
class using R6's `$set()` method. These provide computed views on the
database without storing additional data.

## Active Bindings Added

- `lulc_meta_long_v` - Unrolled LULC metadata with one row per source
  class

- `pred_sources_v` - Distinct predictor URLs and their MD5 checksums

- `transitions_v` - Land use transitions derived from lulc_data_t

- `extent` - Spatial extent of coords_t as terra::SpatExtent

- `coords_minimal` - Minimal coordinate representation (id_coord, lon,
  lat)

## Methods Added

- `trans_pred_data_v(id_trans)` - Returns wide table of transition
  results and predictor data for a specific transition. Used as input to
  covariance filtering.
