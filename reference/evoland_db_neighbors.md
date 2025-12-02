# Neighbor analysis methods for evoland_db

This file adds neighbor analysis methods to the `evoland_db` class using
R6's `$set()` method. These methods compute neighbor relationships and
generate neighbor-based predictors.

## Methods Added

- `create_neighbors_t(max_distance, distance_breaks, resolution, overwrite)` -
  Computes neighbor relationships between coordinates.

  - `max_distance`: Maximum distance for neighbors (default: 1000)

  - `distance_breaks`: Vector of breaks for distance classes (default:
    c(0, 100, 500, 1000))

  - `resolution`: Grid resolution for distance calculations (default:
    100)

  - `overwrite`: Whether to overwrite existing neighbors_t (default:
    FALSE)

- `generate_neighbor_predictors()` - Generates predictor variables based
  on neighbor land use counts by distance class. Requires neighbors_t
  with distance_class column (create_neighbors_t with distance_breaks).
