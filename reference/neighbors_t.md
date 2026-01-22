# Create Neighbors Table

Creates a `neighbors_t` table and validates that it matches the schema.

## Usage

``` r
as_neighbors_t(x)

create_neighbors_t(
  coords_t,
  max_distance,
  distance_breaks = NULL,
  quiet = FALSE
)

# S3 method for class 'neighbors_t'
validate(x, ...)

# S3 method for class 'neighbors_t'
print(x, nrow = 10, ...)
```

## Arguments

- x:

  An object that can be passed to
  [`data.table::setDT()`](https://rdatatable.gitlab.io/data.table/reference/setDT.html)

- max_distance:

  Maximum distance to search for neighbors (in same units as
  coordinates)

- distance_breaks:

  Optional numeric vector defining distance class boundaries. If NULL,
  no distance classification is performed. If provided, must have at
  least 2 elements defining interval breaks.

- ...:

  Passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- nrow:

  Maximum number of rows to print. See
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

## Value

A data.table of class "neighbors_t" with columns:

- `id_coord_origin`: Foreign key to coords_t (origin coordinate)

- `id_coord_neighbor`: Foreign key to coords_t (neighbor coordinate)

- `distance`: Numeric distance between coordinates

- `distance_class`: Optional factor representing distance intervals

A data.table with columns:

- id_coord_origin: ID of the origin coordinate

- id_coord_neighbor: ID of the neighboring coordinate

- distance: Distance between origin and neighbor

- distance_class: Factor indicating distance class (if distance_breaks
  provided)

## Methods (by generic)

- `validate(neighbors_t)`: Validate a neighbors_t object

- `print(neighbors_t)`: Print a neighbors_t object

## Functions

- `create_neighbors_t()`: Compute neighboring coordinates within
  specified distances. This uses a spatial hash map for efficiency.
