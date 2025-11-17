# Create LULC Metadata Table

Create a `lulc_meta_t` table or rows thereof that meet the schema
requirements. These metadata describe the characteristics of land use
classes.

## Usage

``` r
as_lulc_meta_t(x)

create_lulc_meta_t(lulc_class_spec)

# S3 method for class 'lulc_meta_t'
print(x, nrow = 10, ...)
```

## Arguments

- lulc_class_spec:

  A list of intervention specifications, see examples

- nrow:

  see
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

## Value

A data.table of class "lulc_meta_t" with columns:

- `id_lulc`: Unique ID for each land use class

- `name`: Name for use in code and queries

- `pretty_name`: Long name for plots/output

- `description`: Long description / operationalisation

## Methods (by generic)

- `print(lulc_meta_t)`: Print an lulc_meta_t object, passing params to
  data.table print

## Functions

- `create_lulc_meta_t()`: Creates a lulc_meta_t table from intervention
  specifications

## Examples

``` r
create_lulc_meta_t(list(
  forest = list(
    pretty_name = "Dense Forest",
    description = "Mature, dense forest cover",
    src_classes = c(1L, 2L)
  ),
  arable = list(
    pretty_name = "Arable land",
    description = "Non-permanent cropland",
    src_classes = 3:6
  )
))
#> LULC Metadata Table
#> Number of classes: 2
#> Key: <id_lulc>
#>    id_lulc   name  pretty_name                description src_classes
#>      <int> <char>       <char>                     <char>      <list>
#> 1:       1 forest Dense Forest Mature, dense forest cover         1,2
#> 2:       2 arable  Arable land     Non-permanent cropland     3,4,5,6
```
