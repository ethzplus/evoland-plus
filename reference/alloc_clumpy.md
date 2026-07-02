# CLUMPY-style Allocation Methods

Methods for running CLUMPY-style LULC allocation. The algorithm works in
three stages per period:

1.  **Prediction** – raw transition potentials are predicted and stored
    in `trans_pot_t` via
    [`predict_trans_pot()`](https://ethzplus.github.io/evoland-plus/reference/trans_pot_t.md).

2.  **Adjustment** – the adjusted view
    [`adjusted_trans_pot_v()`](https://ethzplus.github.io/evoland-plus/reference/evoland_db_views.md)
    rescales potentials to match target rates and closes rows to \[0,
    1\].

3.  **Allocation** – the whole pivot-selection + patch-growth routine
    runs in C++
    ([`allocate_clumpy_cpp()`](https://ethzplus.github.io/evoland-plus/reference/allocate_clumpy_cpp.md)).
    The method is chosen automatically from the patch parameters:

    - **uSAM** (Unbiased Simple Allocation Method, Mazy sec. 3.4.1) when
      every transition is mono-pixel (`area_mean == 1` and
      `area_var == 0`): one MuST (Multinomial Sampling Test, Mazy App.
      3.B; the same test the reference `clumpy` calls "GART") pass per
      anterior class, each selected pivot allocated as a single cell.
      Quantity of change is enforced in expectation.

    - **uPAM** (Unbiased Patch Allocation Method, Mazy sec. 3.4.2, Fig.
      3.2) otherwise: iterative MuST with a per-transition pixel quota
      and sampling without replacement. Affordable here because
      evoland's potentials come from a fixed fitted model, so the
      marginal density does not need to be re-estimated between patches.

    (Multi-pixel patches require uPAM; "uSAM with patches larger than
    one pixel" is not a valid method, hence the automatic selection
    rather than a user switch.)

    The per-cell pivot probability is divided by the mean patch area
    (the 1/E(sigma) factor, Mazy Fig. 3.2) so the allocated quantity of
    change matches the target transition rate; without it allocation
    over-shoots by roughly the mean patch size.

## Usage

``` r
alloc_clumpy_one_period(
  self,
  id_period_ant,
  id_period_post,
  anterior_rast,
  select_score,
  select_maximize,
  area_dist = "lognormal",
  avoid_aggregation = TRUE,
  batch_size = 0L
)

alloc_clumpy(
  self,
  id_periods,
  select_score,
  select_maximize,
  area_dist = "lognormal",
  avoid_aggregation = TRUE,
  batch_size = 0L,
  seed = NULL
)
```

## Arguments

- self:

  An
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance.

- id_period_ant:

  Integer anterior period ID.

- id_period_post:

  Integer posterior period ID.

- anterior_rast:

  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  of the anterior LULC state.

- select_score:

  Character; mlr3 measure ID for model selection.

- select_maximize:

  Logical; whether to maximise `select_score`.

- area_dist:

  Character; patch-area distribution, `"lognormal"` (default) or
  `"normal"`.

- avoid_aggregation:

  Logical; uPAM merge avoidance (default `TRUE`).

- batch_size:

  Integer; uPAM pivots attempted per MuST re-draw. `0` (default)
  auto-scales with the source pool; see `alloc_clumpy_one_period()`.

- id_periods:

  Integer vector of posterior period IDs to simulate.

- seed:

  Optional integer random seed for reproducibility.

## Value

An
[lulc_data_t](https://ethzplus.github.io/evoland-plus/reference/lulc_data_t.md)
with the simulated posterior LULC.

## Functions

- `alloc_clumpy_one_period()`: Allocate LULC changes for a single period
  using the CLUMPY algorithm.

- `alloc_clumpy()`: Run CLUMPY-style allocation over multiple periods.

## References

Mazy, 2022 (<https://theses.hal.science/tel-04382012v1>), Ch. 3.
