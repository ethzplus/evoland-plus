# Multinomial Sampling Test (MuST) in C++

Inverse-CDF multinomial draw of a final state per cell (Mazy 2022,
Appendix 3.B). This is the same test the reference `clumpy`
implementation calls the "generalized allocation rejection test" (GART);
the thesis itself only uses the MuST name, so we follow that here. NaN
and negative probabilities are clamped to 0 (matching the reference
`clumpy` Python).

## Usage

``` r
must_cpp(P, states, u = NULL)
```

## Arguments

- P:

  Numeric matrix (n_cells x n_states); each row should sum to ~1
  (include the "stay" column).

- states:

  Integer vector of length `ncol(P)` giving the state id of each column.

- u:

  Optional NumericVector of length `nrow(P)` of uniform draws in \[0, 1)
  to replay instead of drawing from R's RNG. Lets an external uniform
  stream (e.g. numpy's, from the reference `clumpy`) be replayed for an
  exact cross-tool comparison of the pivot test.

## Value

Integer vector of length `nrow(P)` with the sampled state per cell.
