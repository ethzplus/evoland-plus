# Create Transition Metadata Table

Creates a trans_meta_t table based on observed land use transitions in
the LULC data. This function analyzes transition patterns and creates
metadata entries for each viable transition type.

## Usage

``` r
as_trans_meta_t(x)

# S3 method for class 'trans_meta_t'
print(x, nrow = 10, ...)
```

## Arguments

- nrow:

  see
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- db:

  An
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance with populated lulc_data_t and lulc_meta_t tables

## Value

A data.table of class "trans_meta_t" with columns:

- `id_trans`: Unique ID for each transition

- `id_lulc_anterior`: Foreign key to lulc_meta_t (before transition)

- `id_lulc_posterior`: Foreign key to lulc_meta_t (after transition)

- `cardinality`: How many times this transition occurred

- `frequency_rel`: Frequency relative to all transitions in this
  timestep

- `frequency_abs`: Frequency relative to all coordinate pairs with data
  in this timestep

- `is_viable`: Whether this transition is viable for modelling

## Methods (by generic)

- `print(trans_meta_t)`: Print a trans_meta_t object, passing params to
  data.table print
