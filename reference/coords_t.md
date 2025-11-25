# Coordinate Tables

Create and validate `coords_t` objects, describing the base set of
coordinate points upon which your land use model is intended to run.
Because the coordinates are declared as points, you can describe sparse
domains (e.g. following polity boundaries) or with arbitrary
distribution (e.g. square, hexagonal, voronoi, polygon tesselations).

## Usage

``` r
as_coords_t(x)

# S3 method for class 'coords_t'
print(x, nrow = 10, ...)

create_coords_t_square(epsg, extent, resolution, ...)
```

## Arguments

- x:

  A table that can be coerced to a valid `coords_t` object.

- nrow:

  see
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- epsg:

  An integerish scalar representing an EPSG CRS code

- extent:

  A
  [terra::SpatExtent](https://rspatial.github.io/terra/reference/SpatExtent-class.html)
  object describing the extent of a desired `coords_t`

- resolution:

  A numeric scalar, describing the required resolution of your
  `coords_t` object. The unit is that of the epsg.

## Value

A data.table of class "coords_t" with columns:

- `id_coord`: Unique ID for each coordinate pair

- `lon`: Longitude/x coordinate

- `lat`: Latitude/y coordinate

- `elevation`: Elevation (initially NULL)

- `geom_polygon`: Geometry polygon object (for grid cells)

## Methods (by generic)

- `print(coords_t)`: Print a coords_t object, passing params to
  data.table print

## Functions

- `as_coords_t()`: Cast and validate as coords_t

- `create_coords_t_square()`: Create a set of square coordinates
