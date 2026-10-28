# Figure of merit of simulated land use change

Compares simulated against observed change with Pontius' figure of merit
(Pontius et al. 2008). The initial and observed maps are read from a
reference run, the simulated maps from one or more other runs, each
through its own run lineage. Every cell that changed in either the
observed or a simulated map is one of:

- **hit**: observed change, simulated as change to the observed class;

- **wrong hit**: observed change, simulated as change to another class;

- **miss**: observed change, simulated as persistence;

- **false alarm**: observed persistence, simulated as change.

The figure of merit is
`hits / (misses + hits + wrong hits + false alarms)`, the share of the
union of observed and simulated change that the simulation got right.
Cells that persisted in both maps do not enter it, so, unlike overall
agreement, it is not inflated by a mostly unchanging landscape. Its
value scales with how much change there is to get right, so compare it
against `figure_of_merit_null`, the value expected when the simulated
quantity of each transition is placed at random among the cells of its
initial class.

Cells whose change is not the model's to get right, such as a class
whose transitions are imposed deterministically, can be left out with
`exclude_id_lulc`. Which cells are left out is decided on the reference
run alone: a cell is ignored, in every simulated run, if its class in
the reference run's initial or observed map is one of them. Its
simulated class does not matter.

For an ensemble of runs, the expected counts are the means of the
per-run counts; recompute the ratios from those means rather than
averaging the per-run ratios.

## Usage

``` r
figure_of_merit_v(
  self,
  id_period_anterior,
  id_period_post,
  id_run_reference,
  id_run_simulated = self$id_run,
  by_transition = FALSE,
  exclude_id_lulc = integer(0)
)
```

## Arguments

- self:

  An
  [evoland_db](https://ethzplus.github.io/evoland-plus/reference/evoland_db.md)
  instance

- id_period_anterior:

  Integer, period of the initial map

- id_period_post:

  Integer, period of the observed and simulated maps

- id_run_reference:

  Integer, run holding the initial and observed maps

- id_run_simulated:

  Integer vector, runs holding the simulated maps; defaults to the
  active run

- by_transition:

  Logical; if `TRUE`, report per transition instead of overall.

- exclude_id_lulc:

  Integer vector of classes; cells whose class in the reference run, at
  `id_period_anterior` or `id_period_post`, is one of them are ignored.
  Default none.

## Value

A
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with one row per simulated `id_run` (and transition). Overall
(`by_transition = FALSE`): `hits`, `wrong_hits`, `misses`,
`false_alarms`, `figure_of_merit`, `producers_accuracy` (`hits` over
observed change), `users_accuracy` (`hits` over simulated change) and
`figure_of_merit_null`. Per transition: `id_lulc_anterior`,
`id_lulc_posterior`, the `observed` and `simulated` cell counts, `hits`,
`figure_of_merit` (`hits` over the union of observed and simulated
change) and `figure_of_merit_null`. Cells missing from any of the three
maps are ignored.

## References

Pontius, R. G., Boersma, W., Castella, J.-C., et al. (2008). Comparing
the input, output, and validation maps for several models of land
change. The Annals of Regional Science, 42(1), 11-37.
https://doi.org/10.1007/s00168-007-0138-2
