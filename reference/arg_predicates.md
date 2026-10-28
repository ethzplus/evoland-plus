# Argument predicates

Type checks for function arguments, meant as the conditions of a named
[`stopifnot()`](https://rdrr.io/r/base/stopifnot.html), whose name is
the message the user sees:

## Usage

``` r
is_integerish(x)

is_scalar_integerish(x)

is_scalar_numeric(x)

is_scalar_character(x)

is_scalar_logical(x)
```

## Arguments

- x:

  Object to check

## Value

`TRUE` or `FALSE`

## Details

    stopifnot("id_period must be a single integer" = is_scalar_integerish(id_period))

None of them accepts `NA`.

- `is_integerish()`: a numeric vector of finite whole numbers, of any
  length including 0; `1` and `1L` both qualify, so callers need not
  write `1L`.

- `is_scalar_integerish()`: one finite whole number.

- `is_scalar_numeric()`: one number.

- `is_scalar_character()`: one string.

- `is_scalar_logical()`: `TRUE` or `FALSE`.
