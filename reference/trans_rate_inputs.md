# Preparing the inputs of the transition rate solver

Unit conversion and label normalisation that happen before any program
is built. The program itself is in
[trans_rate_lp](https://ethzplus.github.io/evoland-plus/reference/trans_rate_lp.md).

## Usage

``` r
rescale_trans_rate(rate, from_years, to_years, is_persistence = FALSE)

canonical_shapes(shapes)
```

## Arguments

- rate:

  Numeric vector of rates in `[0, 1]`.

- from_years, to_years:

  Period lengths in years.

- is_persistence:

  Logical, `TRUE` for `i -> i` rates.

- shapes:

  A data.table with `id_lulc` and `shape`.

## Value

`rescale_trans_rate()` returns rates on the `to_years` scale.

`canonical_shapes()` returns `shapes` with canonical labels, `NA` where
unset.

## Functions

- `rescale_trans_rate()`: Rescale a transition rate between period
  lengths. Transition rates are survival-style quantities: the
  complement `1 - rate` compounds multiplicatively, so a rate observed
  over 12 years is not comparable to one over 10. Persistence (a class
  staying itself) is the complement of the total outflow and therefore
  compounds directly.

- `canonical_shapes()`: Normalise elicited trajectory shape labels.
