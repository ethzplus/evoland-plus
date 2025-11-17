# Ingest GeoTIFFs as Predictors

In a likely analytic scenario, your analytic workflow will include the
ingestion of predictor data from disk instead of a remote source. This
is not ideal for reproducibility - in the best case, you would be
retrieving data from a persistent archive that uses a DOI handle.

This file shows how to work in either scenario. Let’s start by attaching
a database to work in, and setting some coordinate points

``` r
library(evoland)
library(data.table)
db <- evoland_db$new(path = "noisemodel.evolanddb")
db$set_coords(
  type = "square",
  epsg = 2056,
  extent = terra::ext(c(
    xmin = 2697000,
    xmax = 2698000,
    ymin = 1252000,
    ymax = 1253000
  )),
  resolution = 100
)

db$set_periods(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2020-01-01",
  end_extrapolated = "2060-01-01"
)
```

## Ingest Predictors from Online Sources

Here, we’ll start off with a relatively simple predictor: The current
level of noise pollution, in accordance with the current levels of
traffic.

``` r
coords_minimal <- db$coords_minimal

sonbase_spec <- list(
  noise = list(
    unit = "dBa",
    pretty_name = "Maximum noise exposure",
    orig_format = "10m*10m raster",
    description = "The maximum across all sonBASE noise exposure categories, i.e. daytime & nighttime road & rail exposure",
    sources = list(
      list(
        url = "https://data.geo.admin.ch/ch.bafu.laerm-strassenlaerm_tag/laerm-strassenlaerm_tag/laerm-strassenlaerm_tag_2056.tif",
        md5sum = "09791808dbf12fcde82182e70f2ebdfb"
      ),
      list(
        url = "https://data.geo.admin.ch/ch.bafu.laerm-strassenlaerm_nacht/laerm-strassenlaerm_nacht/laerm-strassenlaerm_nacht_2056.tif",
        md5sum = "6e79dc1d353751084e21dc6b61778b99"
      ),
      list(
        url = "https://data.geo.admin.ch/ch.bafu.laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht_2056.tif",
        md5sum = "161df62f9a2a29c9120380f965aa19ba"
      ),
      list(
        url = "https://data.geo.admin.ch/ch.bafu.laerm-bahnlaerm_tag/laerm-bahnlaerm_tag/laerm-bahnlaerm_tag_2056.tif",
        md5sum = "6016b9ca4c974cb982fbe18112f201fe"
      )
    )
  )
)

sonbase_sources <-
  sonbase_spec$noise$sources |>
  data.table::rbindlist() |>
  download_and_verify()
```

    Downloading: https://data.geo.admin.ch/ch.bafu.laerm-strassenlaerm_tag/laerm-strassenlaerm_tag/laerm-strassenlaerm_tag_2056.tif

    Downloading: https://data.geo.admin.ch/ch.bafu.laerm-strassenlaerm_nacht/laerm-strassenlaerm_nacht/laerm-strassenlaerm_nacht_2056.tif

    Downloading: https://data.geo.admin.ch/ch.bafu.laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht_2056.tif

    Downloading: https://data.geo.admin.ch/ch.bafu.laerm-bahnlaerm_tag/laerm-bahnlaerm_tag/laerm-bahnlaerm_tag_2056.tif

``` r
# data is at 10m
# extent plus 1km, given metres for unit; enough for all resampling strategies
extent_wide <- db$extent |> terra::extend(1000)

sonbase_max <-
  purrr::map(
    sonbase_sources$local_path,
    \(x) terra::rast(x) |> terra::crop(extent_wide)
  ) |>
  terra::rast() |>
  max(na.rm = TRUE) |>
  terra::resample(10, method = "bilinear") |> # downsample factor 10: to hectare
  extract_using_coords_t(coords_minimal)

db$add_predictor(
  pred_spec = sonbase_spec,
  pred_data = sonbase_max[
    ,
    # the id_period = 0L is a 0 integer; this special period indicates "static" data
    # that's not associated with a historical period
    .(id_coord, id_period = 0L, value)
  ],
  pred_type = "float"
)
```

## Check Results

Let’s see how the data looks in the DB.

``` r
db$pred_meta_t
```

    Predictor Metadata Table
    Number of predictors: 1
    Key: <id_pred>
       id_pred   name            pretty_name
         <int> <char>                 <char>
    1:       1  noise Maximum noise exposure
    5 variables not shown: [description <char>, orig_format <char>, sources <list>, unit <char>, factor_levels <list>]

We can see that the `noise` predictor was assigned `id_pred = 1`, which
makes sense, given that this was the first predictor to be ingested

``` r
db$pred_data_t_float
```

    Predictor Data Table (float)
    Observations: 100
    Predictors: 1, Coordinates: 100, Periods: 1
    Key: <id_pred>
         id_pred id_coord id_period    value
           <int>    <int>     <int>    <num>
      1:       1        1         0 37.09188
      2:       1        2         0 34.46947
      3:       1        3         0 34.04930
      4:       1        4         0 33.04930
      5:       1        5         0 32.97577
     ---
     96:       1       96         0 39.19714
     97:       1       97         0 38.97085
     98:       1       98         0 47.31019
     99:       1       99         0 56.00335
    100:       1      100         0 52.97599

We see that there’s currently a single predictor in our table at 100
coordinate points. As expected, once the metadata was inserted into the
database, we used the allotted identifier to label our inserted
`sonbase_max` data.
