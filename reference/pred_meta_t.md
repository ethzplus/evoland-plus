# Create Predictor Metadata Table

Construct and validate `pred_meta_t` objects, which are used to store
predictor metadata.

## Usage

``` r
as_pred_meta_t(x)

create_pred_meta_t(pred_spec)

# S3 method for class 'pred_meta_t'
print(x, ...)
```

## Arguments

- x:

  An object that is accepted by
  [`data.table::setDT()`](https://rdatatable.gitlab.io/data.table/reference/setDT.html)

- pred_spec:

  A list of predictor specifications, schema: see examples

- ...:

  passed to
  [`data.table::print.data.table()`](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

## Value

A data.table of class "pred_meta_t" with columns:

- `id_pred`: Unique ID for each predictor

- `name`: Name for use in code and queries

- `pretty_name`: Name for plots/output

- `description`: Long description / operationalisation

- `orig_format`: Original format description

- `sources`: Sources, a data.frame with cols `url` and `md5sum`

- `unit`: SI-compatible unit (nullable for categorical)

- `factor_levels`: Map of factor levels (nullable)

## Methods (by generic)

- `print(pred_meta_t)`: Print an `pred_meta_t` object, passing params to
  data.table print

## Functions

- `create_pred_meta_t()`: Creates a `pred_meta_t` table from
  intervention specifications

## Examples

``` r
create_pred_meta_t(list(
   noise = list(
     unit = "dBa",
     pretty_name = "Maximum noise exposure",
     orig_format = "10m*10m raster",
     description = "daytime & nighttime road & rail noise exposure",
     sources = list(
       list(
         url = "https://data.geo.admin.ch/ch.bafu.laerm-strassenlaerm_tag/laerm-strassenlaerm_tag/laerm-strassenlaerm_tag_2056.tif",
         md5sum = "a4b9f1c04ee63824f18852bfd1eecbdd"
       ),
       list(
         url = "https://data.geo.admin.ch/ch.bafu.laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht_2056.tif",
         md5sum = "4b782128495b5af8467e2259bd57def2"
       )
     )
   ),
   distance_to_lake = list(
     unit = "m",
     pretty_name = "Distance to closest lake",
     orig_format = "vector",
     description = "Derived from swissTLM3D",
     sources = list(list(
       url = "https://data.geo.admin.ch/ch.swisstopo.swisstlm3d/swisstlm3d_2025-03/swisstlm3d_2025-03_2056_5728.gpkg.zip",
       md5sum = "ecb3bcfbf6316c6e7542e20de24f61b7"
     ))
   )
 ))
#> Predictor Metadata Table
#> Number of predictors: 2
#>                name              pretty_name
#>              <char>                   <char>
#> 1:            noise   Maximum noise exposure
#> 2: distance_to_lake Distance to closest lake
#> 5 variables not shown: [description <char>, orig_format <char>, sources <list>, unit <char>, factor_levels <list>]
```
