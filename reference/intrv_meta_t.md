# Create Intervention Metadata Table / Entries

Creates an `intrv_meta_t` table or rows therein based on intervention
specifications.

## Usage

``` r
as_intrv_meta_t(x)

create_intrv_meta_t(intrv_spec)

# S3 method for class 'intrv_meta_t'
print(x, ...)
```

## Arguments

- x:

  An object that can be passed to
  [`data.table::setDT()`](https://rdrr.io/pkg/data.table/man/setDT.html)

- intrv_spec:

  A list of intervention specifications, schema: see examples

- ...:

  passed to
  [`data.table::print.data.table()`](https://rdrr.io/pkg/data.table/man/print.data.table.html)

## Value

A data.table of class "intrv_meta_t" with columns:

- `id_intrv`: Unique ID for each intervention

- `id_period_list`: Array of associated period IDs

- `id_trans_list`: Array of associated transition IDs

- `pre_allocation`: Boolean indicating if intervention is pre-allocation

- `name`: Name for use in code and queries

- `pretty_name`: Name for plots/output

- `description`: Long description / operationalisation

- `sources`: Array of structs with url and md5sum

- `params`: Map of parameters

## Methods (by generic)

- `print(intrv_meta_t)`: Print an intrv_meta_t object, passing params to
  data.table print

## Functions

- `create_intrv_meta_t()`: Creates an intrv_meta_t table from
  intervention specifications

## Examples

``` r
create_intrv_meta_t(list(
  protected_areas = list(
    pre_allocation = TRUE,
    pretty_name = "Nature protection areas",
    description = "introduces additional protected areas (PAs",
    periods = c(7, 8),
    transitions = c(1, 2),
    sources = list(
      list(
        url = "file:///somedir/protected_areas.gpkg",
        md5sum = "something"
      )
    )
  ),
  hydro_predictors = list(
    pre_allocation = TRUE,
    pretty_name = "Hydrological predictor variables",
    description = "Provide dynamic predictor vars",
    params = list(
      tmpdir = "/mnt/ramdisk"
    )
  )
))
#> Intervention Metadata Table
#> Number of interventions: 2
#> Key: <id_run, id_intrv>
#>    id_intrv id_period_list id_trans_list pre_allocation             name
#>       <int>         <list>        <list>         <lgcl>           <char>
#> 1:        1            7,8           1,2           TRUE  protected_areas
#> 2:        2                                        TRUE hydro_predictors
#> 5 variables not shown: [pretty_name <char>, description <char>, sources <list>, params <list>, id_run <int>]
```
