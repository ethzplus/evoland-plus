# How to interact with the evoland-plus codebase

_Naturally, this is also handy for human devs._

This is the codebase for evoland-plus, or evoland for short.
It is an R package that is used for land use / land cover change analyses and simulations.
The main concept that differentiates this package from e.g. <https://github.com/simonmoulds/lulcc> is that the data are processed in tabular form.
This enables the storage and processing of sparse domains.

Storage is done in parquet files written and read via duckdb, with the domain-agnostic DB implementation in `R/parquet_duckdb.R`. 
Using a normal duckdb file is very inefficient in terms of storage/compression; the parquet files have the advantage of being writable by other software, so they should be considered part of the interface and schema changes should be avoided as much as possible.
The domain specific elements are in the `R/evoland_db*.R` files.
When working on in-memory tables, S3 objects inheriting from data.tables are used for efficiency.
Regular data.frame-like objects can be coerced to these classes using `as_` functions, e.g. `as_lulc_data_t()`.
Classes of source data or "materialized" expensive calculation results are suffixed with `_t` to differentiate them from cheap `_v` views on the database.

## Environment setup

If there is no renv environment set up (check if "renv.lock" exists) ask the user if you can set up an renv development environment.
You can efficiently initialize an renv like so from the project root directory:

```sh
R -e "install.packages('renv', repos = 'https://cloud.r-project.org'); renv::init(bare = TRUE); install.packages('pak'); pak::local_install_dev_deps(); pak::pkg_install('devtools')"
```

There may be issues where .Rprofile already contains a source("renv/activate.R") line, or where renv.lock or the renv folder exist.
These need to be deleted before installation can be successful.

## Dependencies

You are allowed to suggest new dependencies, but make sure the user knows why they are needed.
Avoid packages from the tidyverse, as their APIs tend to shift around over the years.
Avoid niche packages that are seldom maintained; if the functionality is simple, rather implement it as a non-exported utility function.

## Code Style

We use the tidyverse style in general.
This repo uses air as its principal formatter. Config in `air.toml`.
The user may have the languageserver R package installed, meaning `.lintr` directives should also be picked up.
Pay attention to linter warnings, but only once the main functionality is implemented.

Don't add comments where the code's purpose is self-explanatory.
Don't check for missingness or nullness to avoid errors: only catch errors early if the emitted error would be hard to understand for the user.
Generally, use the `stopifnot("error message" = condition)` pattern if something could fail in a hard to understand manner.

## Documentation

The package uses roxygen, so you can use `R -e "roxygen2::roxygenize()"` to make sure the Rd documentation is up to date.
Tutorials are written as quarto files in `vignettes/*.qmd`.
A pkgdown github action transforms all of this into a webpage, see `.github/workflows/pkgdown.yaml`.

## Testing

This repo uses tinytest (not testthat) for conducting tests not only during development (i.e. when the full package namespace is attached via `pkgload::load_all()`), but also after installation.
This means that non-exported functions need to be tested as `evoland:::private_function`.
You can test the full package using `R -e "tinytest::build_test_install()"`.
You can test individual files using `R -e "pkgload::load_all(); tinytest::run_test_file('inst/tinytest/somefile.R')"`

While tests should be comprehensive, the full test suite should remain small enough to test often.
Don't overwhelm the user, add 10 tests at a time, then ask for feedback before continuing.

## Rcpp components

C++ code is in the `src/` folder.
This code interfaces with R using Rcpp; you can make sure the binaries are built using `pkbuild::build()` and clean dlls using `pkgbuild::clean_dll()`
If there is no low-hanging use case for writing a standalone C++ program/header, prefer to use the Rcpp namespace and take advantage of the data types that affords.
