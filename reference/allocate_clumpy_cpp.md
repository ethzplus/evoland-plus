# Run the CLUMPY allocation routine (C++)

Allocates LULC change for a single period. See the file header for the
uSAM vs uPAM methods and the meaning of `rarefy` / `avoid_aggregation` /
`area_dist`. The anterior reference is snapshotted internally from
`landscape`, so a cell is eligible as a pivot only while it still equals
its original source class (prevents a cell changing twice in one time
step).

## Usage

``` r
allocate_clumpy_cpp(
  landscape,
  nrow,
  ncol,
  trans_from,
  trans_to,
  prob_cell,
  prob_value,
  area_mean,
  area_var,
  elongation,
  target_rate,
  method,
  batch_size,
  rarefy,
  shuffle,
  avoid_aggregation,
  area_dist
)
```

## Arguments

- landscape:

  IntegerVector of the anterior LULC state (row-major, 1-based class
  ids, NA_INTEGER for no-data). Not modified; a copy is returned with
  the allocated changes applied.

- nrow, ncol:

  Raster dimensions.

- trans_from, trans_to:

  IntegerVectors (length T) of the source/target class for each
  transition. The set of anterior classes is derived from `trans_from`.

- prob_cell, prob_value:

  Lists of length T (one element per transition) giving the SPARSE
  adjusted potentials: `prob_cell[[t]]` is an integer vector of 1-based
  cell indices and `prob_value[[t]]` the matching numeric potentials for
  transition t. Cells absent from a transition read as 0.

- area_mean, area_var, elongation:

  NumericVectors (length T) of patch parameters per transition.

- target_rate:

  NumericVector (length T) of the target transition rate P(v\|u) per
  transition (fraction of source pixels that change). Used only by uPAM
  to set the per-transition pixel quota.

- method:

  0 = uSAM (mono-pixel single pass), 1 = uPAM (iterative, quota).

- batch_size:

  uPAM only: pivots attempted per MuST re-draw. `> 0` is an explicit cap
  (1 = strict uPAM); `< 0` processes all candidates in one pass; `0`
  auto-scales to ~1% of each class's pool (bounds MuST passes so large
  rasters avoid the O(#patches x pool) cost of strict batch=1).

- rarefy:

  If TRUE, divide pivot probabilities by `area_mean` (the 1/E(sigma)
  factor) so the allocated quantity of change matches the target.

- shuffle:

  If TRUE, randomise pivot processing order.

- avoid_aggregation:

  uPAM only: if TRUE, patches that would merge fail and allocate nothing
  (clumpy GaussianPatcher semantics).

- area_dist:

  Patch-area distribution: 0 = log-normal, 1 = normal.

## Value

IntegerVector (length n_cells) of the posterior LULC state.
