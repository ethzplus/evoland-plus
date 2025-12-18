## How to interact with the user

Please ask for clarifications if the specifications you receive are not clear, or if edge cases exist.
Present a work plan in a single (potentially nested) list, numbered in such a way that feedback can be given to individual points.
Don't overwhelm the user: add 10 tests or one function at a time, then ask for feedback before continuing.
Don't create summary documents: all that is too long for the chat needs to be part of the documentation in the codebase itself.

## How to interact with the evoland-plus codebase

This is the codebase for evoland-plus, or evoland for short.
It is an R package that is used for land use / land cover change analyses and simulations.
The main concept that differentiates this package from e.g. <https://github.com/simonmoulds/lulcc> is that the data are processed in tabular form.
This enables the storage and processing of sparse domains.

## Database

Storage is done in parquet files written and read via duckdb, with a domain-agnostic DB implementation in [parquet_db.R](R/parquet_db.R) as an R6 object.
The advantage of using parquet files over a duckdb file lies in their better compression ratio and their being writable by other software

The domain specific database elements are in [evoland_db.R](R/evoland_db.R) and other files, using the `$set` syntax to amend the generator object.
Primary keys / uniqueness derives from all `id_[a-z]+\b` columns.
The schema underlying the database tables is dependent on natural joins.
Because the tables can be written to from other software, they should be considered part of the interface and schema changes should be avoided as much as possible.
When working on in-memory tables, S3 objects inheriting from data.tables are used for efficiency, see [evoland_db_tables.R](R/evoland_db_tables.R).
Regular data.frame-like objects can be coerced to these classes using `as_` functions, e.g. `as_lulc_data_t()`.
Classes of source data or "materialized" expensive calculation results are suffixed with `_t` to differentiate them from cheap `_v` views on the database.

## Environment setup

If there is no renv environment set up (check if `renv.lock` exists) ask the user if you can set up an renv development environment.
You can efficiently initialize an renv like so from the project root directory:

```sh
R -e "
install.packages('renv', repos = 'https://cloud.r-project.org')
renv::init(bare = TRUE)
install.packages('pak')
pak::local_install_dev_deps()
pak::pkg_install('devtools')
"
```

There may be issues where [.Rprofile](.Rprofile) already contains a `source("renv/activate.R")` line, or where `renv.lock` or the `renv` folder already exist.
These need to be deleted before installation can be successful.

## Dependencies

The dependencies are declared as part of [DESCRIPTION](DESCRIPTION).
Prefer base R solutions.
You are allowed to suggest new dependencies, but make sure the user knows why they are needed.
Avoid packages from the tidyverse, as their APIs tend to shift around over the years.
Avoid niche packages that are seldom maintained; if the functionality is simple, rather implement it as a non-exported utility function.

## Code Style

We use the tidyverse style in general.
This repo uses air as its principal formatter. Config in `air.toml`.
The user may have the languageserver R package installed, meaning `.lintr` directives should also be picked up.
Pay attention to linter warnings, but only once the main functionality is implemented.

Don't add comments where the code's purpose is self-explanatory.
Only catch errors early if the emitted error would be hard to understand for the user.
Use the `stopifnot("error message" = condition)` pattern _iff_ something could fail in a hard to understand manner.

## Documentation

The package uses roxygen, so you can use `R -e "roxygen2::roxygenize()"` to make sure the Rd documentation is up to date.
Tutorials are written as quarto files in `vignettes/*.qmd`.
A pkgdown github action transforms all of this into a webpage, see `.github/workflows/pkgdown.yaml`.
Use roxygen collation directives (e.g. `#' @include evoland_db.R`) to ensure that R6 generator objects previously declared can be built upon using the `$set()` syntax.

## Testing

This repo uses tinytest, see `inst/tinytest`.
As opposed to testthat, this can also conduct test after development.
This means that non-exported functions need to be tested as `evoland:::private_function`.

- Test the full package using `R -e "tinytest::build_test_install()"`.
- Test individual files using `R -e "pkgload::load_all(); tinytest::run_test_file('inst/tinytest/somefile.R')"`

While tests should be comprehensive, the full test suite should remain small enough to test often.

## Rcpp components

C++ code is in the `src/` folder.
This code interfaces with R using Rcpp; you can make sure the binaries are built using `pkbuild::build()` and clean dlls using `pkgbuild::clean_dll()`
If there is no low-hanging use case for writing a standalone C++ program/header, prefer to use the Rcpp namespace and take advantage of the data types that affords.
