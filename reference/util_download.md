# Utilities to download and manage files

In order to maintain a clean set of raw data files, these functions help
with retrieving the raw data from an online source and caching it.

## Usage

``` r
download_and_verify(
  df_in,
  target_dir = getOption("evoland.cachedir"),
  overwrite = FALSE
)
```

## Arguments

- df_in:

  Data frame with 'url' and 'md5sum' columns

- target_dir:

  Target directory for downloads, defaults to option `evoland.cachedir`

- overwrite:

  Whether to overwrite existing files (default: FALSE)

## Value

df_out, Same as df_in but with added column "local_filename", the full
path of which can be constructed

## Functions

- `download_and_verify()`: Download a set of files and check their
  integrity against their md5 checksum
