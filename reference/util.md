# evoland utility functions

These are mostly used to validate evoland S3 objects.

## Usage

``` r
validate(x, ...)

new_evoland_table(x, class_name, keycols)

check_missing_names(x, required_names)

x %||% y

pluck_wildcard(lst, ...)

ensure_dir(dir)

print_rowwise_yaml(df)

cast_dt_col(x, colname, castfun)
```

## Arguments

- x:

  Left-hand side value

- ...:

  Index arguments. Use NA to match all elements at that level

- class_name:

  The class name to attach before "evoland_t"

- keycols:

  The columns to be set as key, see
  [`data.table::setkey()`](https://rdatatable.gitlab.io/data.table/reference/setkey.html)

- required_names:

  Vector of required names

- y:

  Right-hand side value (fallback)

- lst:

  The list to index into

## Value

NULL, called for side effect

The indexed result, which may be a single element or a list of elements

## Functions

- `validate()`: Provides an S3 validation generic

- `new_evoland_table()`: Add evoland_t class

- `check_missing_names()`: Check that all required names are present

- `x %||% y`: Null coalescing operator

- `pluck_wildcard()`: Pluck with wildcard support

- `ensure_dir()`: Ensure that a directory exists; return its argument
  for pipeability

- `print_rowwise_yaml()`: Print a dataframe in a row-wise yaml style

- `cast_dt_col()`: Cast a data.table column; invisibly returns x
