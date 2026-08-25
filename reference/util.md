# evoland utility functions

These are mostly used to validate evoland S3 objects.

## Usage

``` r
validate(x, ...)

x %||% y

pluck_wildcard(lst, ...)

ensure_dir(dir)

require_suggested(package, purpose)

print_rowwise_yaml(df)

cast_dt_col(x, colname, type, levels = NULL)
```

## Arguments

- x:

  Left-hand side value

- ...:

  Index arguments. Use NA to match all elements at that level

- y:

  Right-hand side value (fallback)

- lst:

  The list to index into

- dir:

  Directory path to ensure exists

- package:

  Name of the suggested package

- purpose:

  What the package is needed for, completing "is required to ..."

- df:

  A data.frame or data.table to print

- colname:

  Name of the column

- type:

  one of "int", "float", "bool", "factor"

- levels:

  Optional character vector of factor levels (only used when type =
  "factor")

## Value

The indexed result, which may be a single element or a list of elements

## Functions

- `validate()`: Provides an S3 validation generic

- `x %||% y`: Null coalescing operator

- `pluck_wildcard()`: Pluck with wildcard support

- `ensure_dir()`: Ensure that a directory exists; return its argument
  for pipeability

- `require_suggested()`: Require a package that evoland only suggests,
  failing with an actionable message rather than with the one R gives
  for a missing namespace

- `print_rowwise_yaml()`: Print a dataframe in a row-wise yaml style

- `cast_dt_col()`: Cast a data.table column; invisibly returns x
