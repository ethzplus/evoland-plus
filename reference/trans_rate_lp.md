# R6 Class for the Demand-Driven Transition Rate Program

A coupled linear program that derives per-transition flows from
per-class area targets. It is assembled one constraint block at a time;
every block has an `add_` method of the same name, and
[lp_problem](https://ethzplus.github.io/evoland-plus/reference/lp_problem.md)
keeps the resulting rows tagged with that name so a program can be
solved over a subset of its blocks. That is what makes the reachability
precheck the same program as the solve, rather than a second
implementation of it.

## Units

The program is solved in shares of the landscape: shares and absolute
areas are the same program up to a scalar, but the share version spans
six fewer orders of magnitude. Everything the object returns is in
cells, rehydrated against the number of cells `lulc_data` holds at the
anchor period.

## Periods

Following
[trans_rates_t](https://ethzplus.github.io/evoland-plus/reference/trans_rates_t.md),
the flows of period `p` take the landscape from its state at `p - 1` to
its state at `p`. Area variables therefore exist at the anchor period –
the last observed one – and at every extrapolated period, while flow
variables exist at the extrapolated periods only.

## Variables

All variables are non-negative. `id_lulc` identifies the class a
class-level variable belongs to; `id_lulc_anterior` and
`id_lulc_posterior` identify a transition-level one.

|  |  |  |
|----|----|----|
| **block** | **keys** | **meaning** |
| `flow` | transition, period | cells moving along a transition |
| `area` | class, period | area of a class at a state |
| `rate_lower`, `rate_upper` | transition, period | rate-bound violation |
| `historic` | transition, period | distance from the historic outflow pattern |
| `shape`, `smoothness` | class, period | curvature violation |
| `target_over`, `target_under` | class | terminal fit either side of the target |
| `fairness` | none | worst per-class rate-bound violation |

## Constraint blocks

`initial`, `conservation` and `closure` are the mass balance;
`forbidden` and `rate_limits` are the hard statements about what a
transition may do; the rest carry slack variables and are paid for in
the objective. `blocks` records which are enabled, which enter a solve
and which enter the reachability precheck.

## Installation

Solving needs `lpSolve`, which evoland only suggests: most of the
package never solves a linear program. `trans_rate_lp$new()` fails with
an actionable message if it is missing, so install it before
constructing one – `install.packages("lpSolve")`.

## See also

[trans_rates_solver](https://ethzplus.github.io/evoland-plus/reference/trans_rates_solver.md),
[lp_problem](https://ethzplus.github.io/evoland-plus/reference/lp_problem.md),
[trans_rates_t](https://ethzplus.github.io/evoland-plus/reference/trans_rates_t.md)

## Super class

[`lp_problem`](https://ethzplus.github.io/evoland-plus/reference/lp_problem.md)
-\> `trans_rate_lp`

## Active bindings

- `blocks`:

  The constraint blocks, whether each is enabled, and which programs
  each takes part in.

- `scenario`:

  Per class: the initial and target areas, the elicited shape and the
  objective weight.

- `transitions`:

  Every ordered pair of classes with its rate bounds, whether it is
  viable, and whether the program forbids it.

- `steps`:

  The extrapolated periods, the state each starts from, and its length.

- `reachability`:

  Per class and period: the areas reachable under mass balance and hard
  historic rates, ignoring targets. Solved on demand and then cached.

- `areas`:

  The solved class areas, in cells, per class and state.

- `flows`:

  The solved transition flows, in cells.

- `rates`:

  The solved flows as rates of their anterior class, which is what
  [`adjusted_trans_pot_v()`](https://ethzplus.github.io/evoland-plus/reference/evoland_db_views.md)
  and the allocators consume.

- `diagnostics`:

  How far the solution had to depart from the target and from observed
  history. These are results, not debug output: a solver that does not
  report how far outside history it went is actively misleading.

## Methods

### Public methods

- [`trans_rate_lp$new()`](#method-trans_rate_lp-initialize)

- [`trans_rate_lp$add_all()`](#method-trans_rate_lp-add_all)

- [`trans_rate_lp$add_initial()`](#method-trans_rate_lp-add_initial)

- [`trans_rate_lp$add_conservation()`](#method-trans_rate_lp-add_conservation)

- [`trans_rate_lp$add_closure()`](#method-trans_rate_lp-add_closure)

- [`trans_rate_lp$add_forbidden()`](#method-trans_rate_lp-add_forbidden)

- [`trans_rate_lp$add_rate_limits()`](#method-trans_rate_lp-add_rate_limits)

- [`trans_rate_lp$add_rate_bounds()`](#method-trans_rate_lp-add_rate_bounds)

- [`trans_rate_lp$add_historic()`](#method-trans_rate_lp-add_historic)

- [`trans_rate_lp$add_target()`](#method-trans_rate_lp-add_target)

- [`trans_rate_lp$add_monotonicity()`](#method-trans_rate_lp-add_monotonicity)

- [`trans_rate_lp$add_shape()`](#method-trans_rate_lp-add_shape)

- [`trans_rate_lp$add_smoothness()`](#method-trans_rate_lp-add_smoothness)

- [`trans_rate_lp$add_fairness()`](#method-trans_rate_lp-add_fairness)

- [`trans_rate_lp$solve()`](#method-trans_rate_lp-solve)

- [`trans_rate_lp$print()`](#method-trans_rate_lp-print)

- [`trans_rate_lp$trans_rates_t()`](#method-trans_rate_lp-trans_rates_t)

- [`trans_rate_lp$clone()`](#method-trans_rate_lp-clone)

Inherited methods

- [`lp_problem$add_constraints()`](https://ethzplus.github.io/evoland-plus/reference/lp_problem.html#method-add_constraints)

------------------------------------------------------------------------

### `trans_rate_lp$new()`

Set up the program from an observed landscape and a scenario demand.
Constraint blocks are added immediately, so the object is ready to
solve. Requires the suggested `lpSolve` package to be installed.

#### Usage

    trans_rate_lp$new(
      lulc_data,
      bounds,
      periods,
      targets = NULL,
      shapes = NULL,
      lambda_bounds = 0.1,
      mu_shape = 15,
      mu_smooth = 1,
      mu_target = 1000,
      mu_historic = 0,
      margin = 0.01,
      terminal_band = NA,
      shape_strictness = 0,
      monotone = TRUE,
      fairness = TRUE,
      forbid_non_viable = TRUE,
      max_reachability_ratio = 10
    )

#### Arguments

- `lulc_data`:

  A
  [lulc_data_t](https://ethzplus.github.io/evoland-plus/reference/lulc_data_t.md)
  for a single `id_run`. The areas of its last observed period are the
  initial state.

- `bounds`:

  Per-transition rate bounds, see
  [`trans_rate_bounds()`](https://ethzplus.github.io/evoland-plus/reference/trans_rates_solver.md).

- `periods`:

  A
  [periods_t](https://ethzplus.github.io/evoland-plus/reference/periods_t.md).
  Its extrapolated periods are the steps to solve for, and their lengths
  set the time scale of the trajectory-shape constraints.

- `targets`:

  A data.table with `id_lulc` and either `area` (cells on the same grid
  as `lulc_data`) or `share` (of the landscape, rehydrated against it).
  Without targets the object can only answer `reachability`.

- `shapes`:

  A data.table with `id_lulc` and `shape`, one of `"instant growth"`,
  `"delayed growth"`, `"constant change"`, `"instant decline"`,
  `"delayed decline"`. Note that a straight line satisfies every one of
  these one-sided curvature constraints, so shapes only bind when
  `shape_strictness > 0`.

- `lambda_bounds`:

  Penalty weight on rate-bound violation.

- `mu_shape`:

  Penalty weight on trajectory-shape violation.

- `mu_smooth`:

  Penalty weight on the second difference of the trajectory; a
  tie-breaker among otherwise equivalent trajectories.

- `mu_target`:

  Penalty weight on the L1 distance between the solved terminal area and
  the target. Without it, a hard terminal band is treated as free real
  estate and every class parks on a band edge.

- `mu_historic`:

  Penalty weight on the L1 distance between flows and the historic
  outflow pattern (`ref_rate`). Zero by default; raising it keeps flows
  near the observed pattern where the target does not force otherwise.

- `margin`:

  Slack around the rate bounds before a violation is penalised.

- `terminal_band`:

  Relative half-width of a *hard* band around the terminal target, or
  `NA` (the default) to rely on `mu_target` alone. A hard band and
  `forbid_non_viable` together turn an out-of-reach target into an
  infeasible program rather than a near miss: on the SSP-CH demand that
  combination is infeasible for three of five scenarios, while the L1
  fit lands as close as the viable transitions allow and reports the
  shortfall.

- `shape_strictness`:

  Minimum curvature a shaped trajectory must exhibit, as a fraction of
  the class's mean per-step change.

- `monotone`:

  Whether to hard-constrain each class to move monotonically in the
  direction of `sign(target - init)`.

- `fairness`:

  Minimax bound on the worst per-class rate-bound violation. `TRUE` uses
  `lambda_bounds` as its weight; a number sets the weight explicitly.

- `forbid_non_viable`:

  Whether to hard-zero flows on transitions that
  [trans_meta_t](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md)
  marks as non-viable. Such flows have no
  [trans_pot_t](https://ethzplus.github.io/evoland-plus/reference/trans_pot_t.md)
  rows and would be silently dropped at allocation time, so the
  trajectory would not materialise.

- `max_reachability_ratio`:

  Refuse to solve if a target asks for more than this multiple of the
  historically achievable change. Everything below the threshold is
  reported, not gated.

#### Returns

A new `trans_rate_lp` object

------------------------------------------------------------------------

### `trans_rate_lp$add_all()`

Add every enabled constraint block, by calling the `add_` method of the
same name.

#### Usage

    trans_rate_lp$add_all()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$add_initial()`

The landscape starts in the observed state.

#### Usage

    trans_rate_lp$add_initial()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$add_conservation()`

The classes cover the whole landscape at every state. Redundant given
closure in both directions, but a cheap numerical anchor.

#### Usage

    trans_rate_lp$add_conservation()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$add_closure()`

Every cell of a class leaves it along exactly one transition, and every
cell of a state arrived along one. Together with `initial` this
conserves area exactly.

#### Usage

    trans_rate_lp$add_closure()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$add_forbidden()`

A non-viable transition carries no flow at all. It is zero, not merely
expensive: it has no
[trans_pot_t](https://ethzplus.github.io/evoland-plus/reference/trans_pot_t.md)
rows and would be dropped at allocation time. One row per transition and
period – a single row summing the periods says the same thing about
non-negative flows, but it is dense enough for
[`lpSolve::lp()`](https://rdrr.io/pkg/lpSolve/man/lp.html)'s default
scaling to fail on it numerically.

#### Usage

    trans_rate_lp$add_forbidden()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$add_rate_limits()`

No transition moves faster than it ever has, as a *hard* bound with no
slack and no margin, and with persistence left free. This is the loosest
honest question about what a class can reach, and it is the block
`reachability` solves over. It is deliberately absent from a solve,
where the same statement appears as the softly penalised `rate_bounds`.

#### Usage

    trans_rate_lp$add_rate_limits()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$add_rate_bounds()`

No transition moves much faster or much slower than it historically has,
as a softly penalised bound widened by `margin`. Soft because elicited
targets routinely require flows outside the observed envelope; how far
outside is reported in `diagnostics` rather than suppressed.

#### Usage

    trans_rate_lp$add_rate_bounds()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$add_historic()`

Flows stay near the historic outflow pattern, as an L1 penalty on
`|flow - ref_rate * area|`. This is the term that stops the program
inventing transitions that happen to be cheap.

#### Usage

    trans_rate_lp$add_historic()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$add_target()`

The landscape ends where the scenario asks it to: an L1 fit that
degrades gracefully, plus a hard band when `terminal_band` is set.

#### Usage

    trans_rate_lp$add_target()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$add_monotonicity()`

Each class moves in the direction of its target and does not turn back,
as a hard constraint with no tolerance.

#### Usage

    trans_rate_lp$add_monotonicity()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$add_shape()`

Each class trajectory curves the way its elicited shape says, as a
softly penalised one-sided constraint on the change in the per-year rate
of change. `"instant"` front-loads the change and `"delayed"` back-loads
it; `"constant change"` asks for both at once, which is what makes it an
equality.

#### Usage

    trans_rate_lp$add_shape()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$add_smoothness()`

Each class trajectory prefers a small second difference. A tie-breaker
among the many trajectories that satisfy everything else, not a
modelling statement.

#### Usage

    trans_rate_lp$add_smoothness()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$add_fairness()`

No class carries a much worse rate-bound violation than the others. A
minimax of linear expressions is itself linear, so this needs no
quadratic solver.

#### Usage

    trans_rate_lp$add_fairness()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$solve()`

Solve for the flows that take the landscape to its targets, after
checking that the targets are not grossly beyond what history supports.

#### Usage

    trans_rate_lp$solve()

#### Returns

The `trans_rate_lp` object, invisibly

------------------------------------------------------------------------

### `trans_rate_lp$print()`

Print a summary of the program.

#### Usage

    trans_rate_lp$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `trans_rate_lp$trans_rates_t()`

trans_rates_t The solved rates as a
[trans_rates_t](https://ethzplus.github.io/evoland-plus/reference/trans_rates_t.md)
for the indicated `id_run`. Persistence and non-viable transitions are
dropped.

#### Usage

    trans_rate_lp$trans_rates_t(id_run)

#### Arguments

- `id_run`:

  Integerish id_run to attach to this table

------------------------------------------------------------------------

### `trans_rate_lp$clone()`

The objects of this class are cloneable with this method.

#### Usage

    trans_rate_lp$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
periods <- create_periods_t("P10Y", "1990-01-01", "2020-01-01", "2040-01-01")
lulc_data <- as_lulc_data_t(data.table::data.table(
  id_run = 0L,
  id_coord = 1:10000,
  id_period = 4L,
  id_lulc = rep(1:2, c(6000, 4000))
))
bounds <- data.table::data.table(
  id_trans = c(NA, 1L, NA, 2L),
  id_lulc_anterior = c(1L, 1L, 2L, 2L),
  id_lulc_posterior = c(1L, 2L, 2L, 1L),
  min_rate = c(0.9, 0, 0.95, 0),
  max_rate = c(1, 0.1, 1, 0.05),
  ref_rate = c(0.95, 0.05, 0.97, 0.03),
  is_viable = TRUE
)

solver <- trans_rate_lp$new(
  lulc_data = lulc_data,
  bounds = bounds,
  periods = periods,
  targets = data.table::data.table(id_lulc = 1:2, share = c(0.5, 0.5))
)
solver$solve()
solver$areas
#>    id_lulc id_period  area
#>      <int>     <int> <num>
#> 1:       1         4  6000
#> 2:       2         4  4000
#> 3:       1         5  5500
#> 4:       2         5  4500
#> 5:       1         6  5000
#> 6:       2         6  5000
```
