# Create Transition Metadata Table

Creates a trans_meta_t table based on observed land use transitions in
the LULC data. This function analyzes transition patterns and creates
metadata entries for each viable transition type.

## Usage

``` r
as_trans_meta_t(x)

create_trans_meta_t(
  transitions,
  min_cardinality_abs = NULL,
  min_frequency_rel = NULL,
  exclude_anterior = NULL,
  exclude_posterior = NULL
)

# S3 method for class 'trans_meta_t'
print(x, nrow = 10, ...)
```

## Arguments

- transitions:

  A transitions_v table, with columns id_coord, id_lulc_anterior,
  id_lulc_posterior, id_period

- min_cardinality_abs:

  Minimum absolute number of transitions for viability (optional)

- min_frequency_rel:

  Minimum relative frequency of transitions for viability (optional)

- exclude_anterior:

  Vector of id_lulc values to exclude as anterior (source) classes

- exclude_posterior:

  Vector of id_lulc values to exclude as posterior (target) classes

- nrow:

  see
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

## Value

A data.table of class "trans_meta_t" with columns:

- `id_trans`: Unique ID for each transition

- `id_lulc_anterior`: Foreign key to lulc_meta_t (before transition)

- `id_lulc_posterior`: Foreign key to lulc_meta_t (after transition)

- `cardinality`: How many times this transition occurred

- `frequency_rel`: Frequency relative to all transitions

- `frequency_abs`: Frequency relative to all coordinate pairs

- `is_viable`: Whether this transition is viable for modelling

## Methods (by generic)

- `print(trans_meta_t)`: Print a trans_meta_t object, passing params to
  data.table print

## Functions

- `create_trans_meta_t()`: Calculate the transition metadata and mark
  for modelling feasibility. Does not attribute `id_trans`; this only
  makes sense as part of a DB.
