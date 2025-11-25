# Getting Started - Ingest Swiss Arealstatistik LULC Data

This file introduces `evoland-plus`, or `evoland` for short, using the
the example of ingesting the [Arealstatistik
(AS)](https://www.bfs.admin.ch/bfs/de/home/dienstleistungen/geostat/geodaten-bundesstatistik/boden-nutzung-bedeckung-eignung/arealstatistik-schweiz.html)
land use land cover data into a database.

In order to read in land use data, we first must create a database with
the following metadata:

- A set of **coordinates** built from a geographic specification
- **Periods** describing a regular time series, built from start, end,
  and interval
- **LULC Metadata** describing the land use/land cover classes we will
  use

### Setup

In this step, we create a new `evoland_db` object, either in-memory
using the special `path` argument “:memory:”, or on-disk. The `path`
creates or loads a file in which the data we work with is persisted.
Somewhat unusually for R objects, you call methods on it like you would
on a Python object (e.g. `pandas_df.drop_duplicates()`). This is called
“encapsulated object oriented programming” (cf. [Advanced
R](https://adv-r.hadley.nz/r6.html)). This is the wrong place to go into
details, but the gist of this style of object is that it provides a nice
representation of a database on disk. In essence, you can read and write
to tables in the database like so:

``` r
library(evoland)

# 1. Create a database object - it organizes a set of tables as parquet files
db <- evoland_db$new("examplefolder.evolanddb")

# 2. Assign an object which has been cast to and validated as `lulc_meta_t` to the database's table. The `_t` suffix indicates a database object that is a Table, as opposed to a View `_v`.
db$lulc_meta_t <- as_lulc_meta_t(x)

# 3. Retrieve and print the data we just stored in the database
print(db$lulc_meta_t)
```

If `evoland_db$new()` encounters a previously existing database, it will
load it. If there is no database yet, it will create a new database with
the `evoland` database schema. The database connection will only be
closed once the db object is removed from the workspace and garbage
collected (i.e. `rm(db); gc()`); until then, there will be a lock on the
database, even for reading! You can call the file whatever you like! You
may call it `my_lulc_model.db` or `🪶🤓.😘` or
`arealstatistik-model.evolanddb`, though I strongly advise against using
non-ascii characters in filenames. Underneath, the file is always a
[DuckDB](https://duckdb.org/) database. The `.evolanddb` suffix is a
clear indication what the purpose of the file is, so let’s go with that
for our actual workflow:

``` r
library(evoland)
library(data.table)
db <- evoland_db$new(path = "arealstatistik-model.evolanddb")
```

### Construct Coordinate Set

The coordinate points provide the fundamental geographic domain on which
our model will operate. These points may lie anywhere in cartesian
space; in their simplest form, they describe a dense matrix of square
cells in a given coordinate reference system. The essential part of the
table that we’ll use later to build a correct sample of land use classes
corresponding to the coordinate point IDs is also built.

``` r
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
minimal_coords_t <- db$coords_minimal
```

### Retrieve Data

Let’s define a data source (along with an MD5 checksum, enabling our
future selves to notice when the underlying file is changed) and
download them to the directory at `getOption("evoland.cachedir")`. This
option is read on package load from the `EVOLAND_CACHEDIR` environment
variable, defaulting to `~/evoland-cache`. For reporting purposes, the
URL and checksum are written to the reporting table.

``` r
lulc_files <-
  data.frame(
    url = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/32376216/appendix",
    md5sum = "c32937eb4a11fc9c5c58c66e9830360a"
  ) |>
  download_and_verify(target_dir = getOption("evoland.cachedir"))

db$commit(
  data.frame(
    key = c("lulc_data_url", "lulc_data_md5sum", "lulc_data_provider"),
    value = c(lulc_files$url, lulc_files$md5sum, "BFS Arealstatistik")
  ),
  table_name = "reporting_t",
  mode = "append"
)

zippath <- file.path(
  getOption("evoland.cachedir"),
  lulc_files$md5sum,
  lulc_files$local_filename
)

# find singular csv
csv_file <-
  unzip(zippath, list = TRUE) |>
  purrr::pluck("Name") |>
  stringi::stri_subset_fixed(".csv")
stopifnot(length(csv_file) == 1L)
```

### Read Data

Now that we have the file and know the name of the contained CSV file,
let’s read it into a data.table. The columns matching with
`AS[0-9]{2}_72` indicate the land use / land cover in one of the [72
NOAS04
classes](https://www.bfs.admin.ch/bfs/de/home/statistiken/raum-umwelt/nomenklaturen/arealstatistik/noas2004.html).

> The Arealstatistik provides information on the flight years (columns
> `FJxx`) for the orthophotographs from which the classification was
> derived; we might eventually be able to use this information as an
> auxiliary predictor of the form “number of years difference between
> flight year and standardized period start”.

``` r
csv_con <- unz(zippath, csv_file, open = "r")
arealstat_dt <-
  readLines(csv_con) |>
  data.table::fread(
    text = _,
    # selecting only years 1985-2018 for now; Arealstatistik 2025 is not yet finished
    select = c(
      "E_COORD",
      "N_COORD",
      "AS85_72",
      "AS97_72",
      "AS09_72",
      "AS18_72"
      # "AS25_72"
    )
  )
close(csv_con)
```

### Recode LULC Classes

We can define a table of land use metadata from a list of lists. This
allows us to legibly determine a pretty name for reporting purposes and
a description that contains a more detailed operationalisation. Each
class is assigned a numeric `id_lulc`, which is used throughout the
database. Each class also refers to a vector of `src_classes`, which
we’ll use to recode/aggregate the source material to our internal
classification system.

``` r
# create metadata lookup table
db$lulc_meta_t <- create_lulc_meta_t(
  list(
    closed_forest = list(
      pretty_name = "Dense Forest",
      description = "Normal forest; Forest strips; Afforestations; Felling areas; Brush forest",
      src_classes = c(50:53, 57L)
    ),
    arable = list(
      pretty_name = "Arable Land",
      src_classes = 41L
    ),
    urban = list(
      pretty_name = "Urban areas",
      description = "Industrial and commercial buildings; Surroundings of industrial and commercial buildings; One- and two-family houses; Surroundings of one- and two-family houses; Terraced houses; Surroundings of terraced houses; Blocks of flats; Surroundings of blocks of flats; Public buildings; Surroundings of public buildings; Agricultural buildings; Surroundings of agricultural buildings; Unspecified buildings; Surroundings of unspecified buildings; Parking areas; Construction sites; Unexploited urban areas; Public parks; Sports facilities; Golf courses; Camping areas; Garden allotments; Cemeteries",
      src_classes = c(1:14, 19L, 29:36)
    ),
    static = list(
      pretty_name = "Static / immutable classes",
      description = "Motorways; Green motorway environs; Roads and paths; Green road environs;  Sealed railway areas; Green railway environs;  Airports; Airfields, green airport environs;  Energy supply plants; Waste water treatment plants; Other supply or waste treatment plants; Dumps; Quarries, mines;  Lakes; Rivers; Flood protection structures; Avalanche and rockfall barriers;  Wetlands; Alpine sports facilities; Rocks; Screes, sand; Landscape interventions",
      src_classes = c(15:18, 20:28, 61:63, 66:71)
    )
  )
)

# lulc_meta_long_v provides an "unrolled" aka "unnested longer" form
# each src_class is related to one id_lulc
lulc_meta_long_dt <- db$lulc_meta_long_v[, .(id_lulc, src_class)]

# longer form: arealstatistik AS replaced by id_lulc
# data.table has form that can be coerced to multilayer terra::rast using type = "xylz"
lulc_dt <-
  data.table::melt(
    # pivot longer with year from regex and coords as ID columns
    arealstat_dt,
    id.vars = c("E_COORD", "N_COORD"),
    value.name = "src_class",
    measure.vars = data.table::measure(
      year = as.integer, # match group
      pattern = "AS([0-9]{2})_72"
    )
  )[
    ,
    year := ifelse(year > 84L, year + 1900L, year + 2000L) # two-digit year to four digit
  ][
    lulc_meta_long_dt,
    .(
      x = E_COORD,
      y = N_COORD,
      year,
      id_lulc
    ),
    on = "src_class",
    nomatch = NULL
  ]

# setting a key on a data.table pre-sorts it
data.table::setkey(lulc_dt, year, x, y)
```

### Rasterize and Sample

We want to take advantage of the spatial structure of the data to
extract the coordinate values of the Arealstatistik at the points where
our coordinates are declared. Hence, we rasterize the tabular data into
a multilayer
[`terra::rast`](https://rspatial.github.io/terra/reference/rast.html)
object.

``` r
r <-
  terra::rast(
    lulc_dt,
    type = "xylz",
    crs = "EPSG:2056"
  ) |>
  terra::as.int() # necessary because terra::rast casts to numeric?
```

Now we have what’s essentially a 3D array with spatial properties, let’s
use extract and some pivoting magic to coerce these data into a table
with an `id_coord, id_lulc` tuple for each period.

``` r
id_coord_yr_lulc_dt <-
  terra::extract(
    x = r,
    y = as.matrix(minimal_coords_t[, .(lon, lat)]),
    method = "simple"
  ) |>
  data.table::as.data.table() |>
  cbind(id_coord = minimal_coords_t[["id_coord"]]) |>
  data.table::melt(
    id.vars = "id_coord",
    measure.vars = data.table::measure(
      year = as.integer,
      pattern = "([0-9]{4})"
    ),
    value.name = "id_lulc"
  ) |>
  na.omit(cols = "id_lulc")
```

### Associate with Regular Periods

Since our land use change model runs in discrete time and our original
land use data is not in a regular time series, we here associate each
`id_coord, id_lulc` tuple with a regular discrete `id_period`. The model
will run on 10yr periods, starting in 1985. Periods starting after 2020
are marked as extrapolation periods. For now, the join condition (left
closed) is only codified here and in the wiki. Making this canonical
will save future modellers headaches.

``` r
# setup transition periods
db$periods_t <- periods_t <- create_periods_t(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2020-01-01",
  end_extrapolated = "2060-01-01"
)

# build date objects from years. use 1st of january
id_coord_yr_lulc_dt[
  ,
  year := data.table::as.IDate(paste0(year, "-01-01"))
]

lulc_data_t <-
  as_lulc_data_t(
    id_coord_yr_lulc_dt[
      periods_t,
      .(
        id_coord,
        id_lulc,
        id_period
      ),
      on = .(
        # left closed interval
        year >= start_date,
        year < end_date
      ),
      nomatch = NULL
    ]
  )
```

### Finalize: Ingest

Now that we have the data prepared and validated in the correct format,
we upsert them into the database. An upsert is a database operation that
either in*serts* new data or *up*dates existing entries.

``` r
db$lulc_data_t <- lulc_data_t
```

## Bonus: Getting rid of unused coords

Now that we know which coordinates we have fundamental data for, let’s
use that information to reduce the amount of coordinates we’ll go
forward with:

``` r
id_coord_keep <- lulc_data_t[, id_coord]
db$commit(
  x = db$coords_t[id_coord %in% id_coord_keep],
  table_name = "coords_t",
  mode = "overwrite"
)
```

For this domain in the middle of Switzerland, this represents a small
amount of savings - but as the sparsity of the domain grows, e.g. by
omission of adjacent administrative areas, or by omission of certain
land use classes - the advantage of the tabular representation becomes
clearer.
