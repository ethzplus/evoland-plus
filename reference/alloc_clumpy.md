# CLUMPY-style Allocation Methods

Methods for running CLUMPY-style LULC allocation, with two entry points:

- `db$alloc_clumpy(id_periods, ...)` runs a whole sequence of periods.
  After each period it commits the allocated map to `lulc_data_t` and
  recomputes the neighbour predictors the next period is predicted from.
  Use it for plain simulations.

- `alloc_clumpy_one_period(db, id_period_post, ...)` allocates one
  period for the active `id_run` and returns the map **without
  committing it**. Use it to write your own loop: edit `trans_pot_t`
  between prediction and allocation (e.g. interventions), edit the
  allocated map afterwards, or draw an ensemble of single-period
  realisations. The caller then commits the map with
  `db$commit(x, "lulc_data_t", method = "upsert")` and, if a later
  period follows, runs `db$upsert_new_neighbors(id_period_post)`, in
  that order.

Both reuse transition potentials already in `trans_pot_t` for the run
and period, and only predict them when none exist (or when
`force_predict_trans_pot = TRUE`).

The algorithm works in three stages per period:

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
  db,
  id_period_post,
  select_score,
  select_maximize,
  area_dist = "lognormal",
  avoid_aggregation = TRUE,
  batch_size = 0L,
  use_parent_trans_pot = FALSE,
  force_predict_trans_pot = FALSE
)

alloc_clumpy(
  self,
  id_periods,
  select_score,
  select_maximize,
  area_dist = "lognormal",
  avoid_aggregation = TRUE,
  batch_size = 0L,
  use_parent_trans_pot = FALSE,
  force_predict_trans_pot = FALSE,
  update_neighbors = TRUE
)
```

## Arguments

- db:

  An
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance; uses its active `id_run`.

- id_period_post:

  Integer posterior period ID.

- select_score:

  Character; mlr3 measure ID for model selection.

- select_maximize:

  Logical; whether to maximise `select_score`.

- area_dist:

  Character; patch-area distribution, `"lognormal"` (default) or
  `"normal"` (Gaussian with sd = `sqrt(area_var)`, clamped to \>= 1).

- avoid_aggregation:

  Logical; if `TRUE` (default) uPAM patches that would merge with an
  existing patch fail and allocate nothing (clumpy `GaussianPatcher`
  semantics). Ignored for the mono-pixel uSAM path.

- batch_size:

  Integer; uPAM pivots attempted per MuST re-draw. `0` (default)
  auto-scales to ~1% of each class's source pool, bounding the number of
  MuST passes so large rasters stay tractable; `> 0` is an explicit cap
  (1 = strict uPAM); `< 0` processes all candidates in a single pass.

- use_parent_trans_pot:

  Logical; if TRUE, predict (or reuse) transition potentials under the
  parent run, so sibling runs share one set, e.g. for Monte-Carlo
  ensembles.

- force_predict_trans_pot:

  Logical; if TRUE, recompute transition potentials even if
  `trans_pot_t` already holds them for this run and period.

- self:

  An
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance.

- id_periods:

  Integer vector of contiguous posterior period IDs to simulate.

- update_neighbors:

  Logical; whether to recompute neighbour predictors after the last
  requested period (default `TRUE`). Intermediate periods are always
  updated, since the next period's prediction reads them. Set `FALSE`
  when nothing is allocated onwards from the last period, e.g. for
  single-period ensembles.

## Value

`alloc_clumpy_one_period()`: an
[lulc_data_t](https://ethzplus.github.io/evoland-plus/reference/lulc_data_t.md)
with the simulated posterior LULC. `alloc_clumpy()`: called for its side
effects on `lulc_data_t` and `pred_data_t`.

## Functions

- `alloc_clumpy_one_period()`: Allocate a single period and return the
  map without committing it; see Description for when to use which.

- `alloc_clumpy()`: Allocate a contiguous sequence of periods,
  committing each one and recomputing neighbour predictors in between;
  the method behind `db$alloc_clumpy()`.

## References

Mazy, 2022 (<https://theses.hal.science/tel-04382012v1>), Ch. 3.
