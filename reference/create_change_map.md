# Create change/difference map between two time periods

Creates a map showing only cells that changed between two time periods.
Cells that did not change are set to NA. This is used in the "similarity
of differences" approach.

## Usage

``` r
create_change_map(initial_map, final_map, from_class = NULL, to_class = NULL)
```

## Arguments

- initial_map:

  SpatRaster, initial time period

- final_map:

  SpatRaster, final time period

- from_class:

  Integer, optional filter for initial class (for specific transition)

- to_class:

  Integer, optional filter for final class (for specific transition)

## Value

SpatRaster showing only changed cells. For general changes, values are
the final class. For specific transitions, values indicate presence (1)
or absence (NA) of that transition.
