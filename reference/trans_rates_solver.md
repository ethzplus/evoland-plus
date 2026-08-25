# Demand-driven transition rate solver

A coupled linear program that derives per-transition flows from
per-class area targets, as an alternative to
[`extrapolate_trans_rates()`](https://ethzplus.github.io/evoland-plus/reference/trans_rates_t.md),
which fits one independent univariate regression per transition and has
no input for a scenario target.

## Usage

``` r
trans_rate_bounds(obs_rates, periods, trans_meta, include_persistence = TRUE)

trans_rate_reachability(lulc_data, bounds, periods)

solve_trans_rates(lulc_data, targets, shapes, bounds, periods, ...)
```

## Arguments

- obs_rates:

  A
  [trans_rates_t](https://ethzplus.github.io/evoland-plus/reference/trans_rates_t.md)
  table of observed rates for a single `id_run`, as returned by
  `evoland_db$get_obs_trans_rates()`.

- periods:

  A
  [periods_t](https://ethzplus.github.io/evoland-plus/reference/periods_t.md)
  table, used for the true period lengths.

- trans_meta:

  A
  [trans_meta_t](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md)
  table, defining the full set of transitions and their viability.

- include_persistence:

  Whether to reconstruct the `i -> i` diagonal as `1 - sum(rate)` over
  all transitions out of `i`. The diagonal is not part of
  [trans_meta_t](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md)
  but the LP needs it; without it, persistence is left unbounded.

- lulc_data:

  A
  [lulc_data_t](https://ethzplus.github.io/evoland-plus/reference/lulc_data_t.md)
  for a single `id_run`; the areas of its last observed period are the
  initial state.

- bounds:

  Per-transition rate bounds, as returned by `trans_rate_bounds()`.

- targets:

  A data.table with `id_lulc` and either `area` (cells on the same grid
  as `lulc_data`) or `share` (of the landscape, rehydrated against
  `lulc_data`).

- shapes:

  A data.table with `id_lulc` and `shape`, or `NULL` for no shape
  preference. See
  [trans_rate_lp](https://ethzplus.github.io/evoland-plus/reference/trans_rate_lp.md).

- ...:

  Further arguments to `trans_rate_lp$new()`, such as the penalty
  weights.

## Value

`trans_rate_bounds()` returns a data.table with `id_trans` (`NA` on the
diagonal), `id_lulc_anterior`, `id_lulc_posterior`, `min_rate`,
`max_rate`, `ref_rate` (the historic mean, for the optional
historic-preference term) and `is_viable`.

`trans_rate_reachability()` returns a data.table with `id_lulc`,
`id_period`, `area_init`, `area_min` and `area_max`, in cells.

`solve_trans_rates()` returns the solved
[trans_rate_lp](https://ethzplus.github.io/evoland-plus/reference/trans_rate_lp.md)
object.

## Details

The model works in three unit layers, each at the layer where it is
correct: scenario targets may be given as a share of the landscape
(grid-independent), the LP itself is solved in shares and reported in
cells (readable diagnostics), and the result is written to
[trans_rates_t](https://ethzplus.github.io/evoland-plus/reference/trans_rates_t.md)
as both a `rate` (what
[evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
`adjusted_trans_pot_v` and the allocators consume) and a `count` of
cells.

The program is built and solved by the
[trans_rate_lp](https://ethzplus.github.io/evoland-plus/reference/trans_rate_lp.md)
class; these functions are the one-call form of it. Decision variables
are the per-period flows between classes and the per-period class areas.
Constraints are the initial condition, row and column closure,
total-area conservation, softly penalised per-transition rate bounds
derived from observed history, an optional hard monotonic direction per
class, and optional soft trajectory-shape and smoothness terms. The
objective is a weighted sum of slack penalties, normalised per class so
that small classes are not ignored.

## Functions

- `trans_rate_bounds()`: Derive per-transition minimum and maximum rate
  bounds from observed history. Rates are annualised before the minimum
  and maximum are taken and then re-inflated to the length of an
  extrapolated period, because observed periods are typically irregular.
  Transitions absent from `obs_rates` in a given period count as a rate
  of 0 for that period, not as missing – otherwise `min_rate` is biased
  upward.

- `trans_rate_reachability()`: Reachability precheck: the areas each
  class can attain at each extrapolated period under mass balance and
  *hard* historic maximum rates, ignoring targets. Persistence is left
  free and minimum rates are not imposed, which makes this the loosest
  honest question – "can this class get there at all, given that no
  transition has ever moved faster than it historically did?".

- `solve_trans_rates()`: Solve for the per-transition flows that take
  the landscape from its observed state to `targets` over the
  extrapolated periods. A one-call form of
  [trans_rate_lp](https://ethzplus.github.io/evoland-plus/reference/trans_rate_lp.md);
  use the class itself to inspect the program or to write the same
  solution to several runs.

## Reachability

Elicited scenario targets are normative and routinely lie outside the
envelope of observed transition rates. `trans_rate_reachability()`
quantifies this without needing a target, and `solve_trans_rates()` runs
it as a precheck: it *reports* how far outside history each target is
and only fails on gross violations (see `max_reachability_ratio`). A
solver that does not report how far outside history it went is actively
misleading, so the returned diagnostics are part of the result, not
debug output.

## Solver dependency

The formulation is deliberately kept inside what
[`lpSolve::lp()`](https://rdrr.io/pkg/lpSolve/man/lp.html) can express.
Terms that would usually be written as quadratic penalties (terminal
fit, historic preference, fairness across classes) are implemented as
their L1 or minimax equivalents, which are linear.

`lpSolve` is a *suggested* dependency, because most of evoland never
solves a linear program: `trans_rate_bounds()` and
[`trans_rate_areas()`](https://ethzplus.github.io/evoland-plus/reference/trans_rates_t.md)
work without it, while `trans_rate_reachability()` and
`solve_trans_rates()` need it installed and say so if it is missing.

## See also

[trans_rate_lp](https://ethzplus.github.io/evoland-plus/reference/trans_rate_lp.md),
[trans_rates_t](https://ethzplus.github.io/evoland-plus/reference/trans_rates_t.md),
[trans_meta_t](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md),
[periods_t](https://ethzplus.github.io/evoland-plus/reference/periods_t.md)
