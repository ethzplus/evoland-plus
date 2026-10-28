# Greedy (rank-and-fill) Allocation Methods

Deterministic allocation that fills each transition's demanded quantity
with the cells of highest transition potential: rank the candidate
(cell, transition) pairs, then walk the ranking and accept a pair when
its cell has not changed yet and its transition still has quantity left.
Every cell changes at most once, and no transition exceeds its demand.
There is no randomness and no patch geometry, so the result is one map,
the same on every run.

This is the allocator behind lulcc's Ordered model (Fuchs et al. 2013)
and SEALS' rank-and-fill, and a useful comparator for the stochastic
allocators: it concentrates change on the most probable cells, which a
single-map score such as the figure of merit rewards, whereas an
ensemble of unbiased realisations
([`alloc_clumpy()`](https://ethzplus.github.io/evoland-plus/reference/alloc_clumpy.md))
reproduces where change occurs.

Two ways to settle the competition for a cell between transitions
(`arbitration`):

- `"joint"` (default): one ranking of all pairs by adjusted potential,
  across transitions; a cell goes to whichever of its transitions comes
  first.

- `"ordered"`: transitions take their cells in turn, in the order given
  by `order`, each taking its best remaining cells; earlier transitions
  win contested cells. This is the Ordered procedure of Fuchs et al.
  (2013), with transitions in place of classes.

**Tie-breaking is arbitrary.** To be deterministic, the ranking needs a
total order, so pairs with equal potential are ranked by secondary keys
that carry no meaning:

- `"joint"`: by `id_trans` (lower first), then `id_coord`. Where two
  transitions have equal potential at a cell, the one with the lower
  `id_trans` gets it; where equally ranked pairs compete for the last
  cells of a quota, those with the lower `id_coord` are taken. The
  `"joint"` ranking is therefore not symmetric in the transitions:
  renumbering them can change the result wherever potentials tie.

- `"ordered"`: within a transition, by `id_coord`.

Ties are rare for continuous potentials, but common for coarse ones
(e.g. classification trees, or potentials rounded on input), and
`id_coord` usually follows the raster's row order, so tied cells are
taken from the top of the map first.

The allocation works on `id_coord` rows only, not on a raster, so it
applies to any tessellation `coords_t` can describe.

Entry points mirror
[`alloc_clumpy()`](https://ethzplus.github.io/evoland-plus/reference/alloc_clumpy.md):
`db$alloc_greedy(id_periods, ...)` runs and commits a sequence of
periods; `alloc_greedy_one_period(db, id_period_post, ...)` returns one
period's map without committing it.

The ranking uses the allocation-ready potentials of
[`adjusted_trans_pot_v()`](https://ethzplus.github.io/evoland-plus/reference/evoland_db_views.md),
which are scaled to each transition's rate, so that they are comparable
across transitions. The quantity per transition is the `count` in
`trans_rates_t`, or, where it is missing, the `rate` times the number of
cells of the anterior class. Because ties are broken by ids (see above),
the result does not depend on the order rows are read in.

## Usage

``` r
alloc_greedy_one_period(
  db,
  id_period_post,
  select_score,
  select_maximize,
  arbitration = c("joint", "ordered"),
  order = NULL,
  use_parent_trans_pot = FALSE,
  force_predict_trans_pot = FALSE
)

alloc_greedy(
  self,
  id_periods,
  select_score,
  select_maximize,
  arbitration = c("joint", "ordered"),
  order = NULL,
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

- arbitration:

  Character, `"joint"` (default) or `"ordered"`; how transitions compete
  for a cell. Both break ties in potential by ids, which is arbitrary;
  see Description.

- order:

  Integer vector of `id_trans`, the priority for
  `arbitration = "ordered"`; viable transitions not listed follow in
  `id_trans` order. Ignored for `"joint"`.

- use_parent_trans_pot:

  Logical; if TRUE, predict (or reuse) transition potentials under the
  parent run.

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
  updated.

## Value

`alloc_greedy_one_period()`: an
[lulc_data_t](https://ethzplus.github.io/evoland-plus/reference/lulc_data_t.md)
with the simulated posterior LULC. `alloc_greedy()`: called for its side
effects on `lulc_data_t` and `pred_data_t`.

## Functions

- `alloc_greedy_one_period()`: Allocate a single period and return the
  map without committing it.

- `alloc_greedy()`: Allocate a contiguous sequence of periods,
  committing each one and recomputing neighbour predictors in between;
  the method behind `db$alloc_greedy()`.

## References

Fuchs, R., Herold, M., Verburg, P. H., & Clevers, J. G. P. W. (2013). A
high-resolution and harmonized model approach for reconstructing and
analysing historic land changes in Europe. Biogeosciences, 10(3),
1543-1559. https://doi.org/10.5194/bg-10-1543-2013
