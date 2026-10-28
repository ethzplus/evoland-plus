# Dinamica EGO Allocation Methods

Methods for running Dinamica EGO allocation and evaluating allocation
parameters, with two entry points for allocation:

- `db$alloc_dinamica(id_periods, ...)` runs a whole sequence of periods.
  After each period it commits the allocated map to `lulc_data_t` and
  recomputes the neighbour predictors the next period is predicted from.
  Use it for plain simulations.

- `alloc_dinamica_one_period(db, id_period_post, ...)` allocates one
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
`force_predict_trans_pot = TRUE`). They mirror
[`alloc_clumpy_one_period()`](https://ethzplus.github.io/evoland-plus/reference/alloc_clumpy.md)
and `db$alloc_clumpy()`.

`eval_alloc_params_t()` runs `alloc_dinamica()` for every run in
`alloc_params_t` over the observed periods and scores each against the
observation with fuzzy similarity.

## Usage

``` r
alloc_dinamica_one_period(
  db,
  id_period_post,
  select_score,
  select_maximize,
  work_dir = NULL,
  use_parent_trans_pot = FALSE,
  force_predict_trans_pot = FALSE
)

alloc_dinamica(
  self,
  id_periods,
  select_score,
  select_maximize,
  work_dir = NULL,
  keep_intermediate = FALSE,
  use_parent_trans_pot = FALSE,
  force_predict_trans_pot = FALSE,
  update_neighbors = TRUE
)

eval_alloc_params_t(
  self,
  select_score,
  select_maximize,
  work_dir = NULL,
  keep_intermediate = FALSE
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

- work_dir:

  Character or NULL; directory for Dinamica's input and output files. If
  `NULL` (default), a temporary directory is used and removed
  afterwards; a directory passed explicitly is kept.

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

  Integer vector of contiguous posterior period IDs to simulate; data
  from the period before the first is used as the anterior state.

- keep_intermediate:

  Logical, whether to keep intermediate files from simulations

- update_neighbors:

  Logical; whether to recompute neighbour predictors after the last
  requested period (default `TRUE`). Intermediate periods are always
  updated, since the next period's prediction reads them. Set `FALSE`
  when nothing is allocated onwards from the last period, e.g. for
  single-period ensembles.

## Value

`alloc_dinamica_one_period()`: an
[lulc_data_t](https://ethzplus.github.io/evoland-plus/reference/lulc_data_t.md)
with the simulated posterior LULC. `alloc_dinamica()`: called for its
side effects on `lulc_data_t` and `pred_data_t`.
`eval_alloc_params_t()`: the `alloc_params_t` rows, with `similarity`
filled in.

## Functions

- `alloc_dinamica_one_period()`: Allocate a single period and return the
  map without committing it; see Description for when to use which.

- `alloc_dinamica()`: Allocate a contiguous sequence of periods,
  committing each one and recomputing neighbour predictors in between;
  the method behind `db$alloc_dinamica()`.

- `eval_alloc_params_t()`: Evaluate allocation parameters using fuzzy
  similarity over different runs.
