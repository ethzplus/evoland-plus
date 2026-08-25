# Create Transition Rates Table

Creates a trans_rates_t table that stores transition rates
(probabilities) for each transition type in each time period. Historical
rates are calculated from observed transitions, and future rates are
extrapolated using linear regression.

## Usage

``` r
as_trans_rates_t(x)

get_obs_trans_rates(self)

extrapolate_trans_rates(obs_rates, periods, coord_count = NA_integer_)

# S3 method for class 'trans_rates_t'
print(x, nrow = 10, ...)

trans_rate_areas(lulc_data, rates, trans_meta)
```

## Arguments

- x:

  A list or data.frame coercible to a data.table

- self:

  a DB instance

- obs_rates:

  A trans_rates_t table of observed transition rates for historical
  periods

- periods:

  A periods_t table with is_extrapolated = TRUE for future periods

- coord_count:

  Optional integer specifying the number of coordinates (cells) for
  normalization

- nrow:

  see
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- lulc_data:

  A
  [lulc_data_t](https://ethzplus.github.io/evoland-plus/reference/lulc_data_t.md)
  for a single `id_run`; the areas of its last period are the state the
  replay starts from.

- rates:

  A trans_rates_t table for a single `id_run`.

- trans_meta:

  A
  [trans_meta_t](https://ethzplus.github.io/evoland-plus/reference/trans_meta_t.md)
  table, resolving `id_trans` to a pair of classes.

## Value

A data.table of class "trans_rates_t" with columns:

- `id_run`: Foreign key to runs_t

- `id_period`: Foreign key to periods_t

- `id_trans`: Foreign key to trans_meta_t

- `count`: Absolute number of transitioning cells for (id_trans,
  id_period)

- `rate`: Transition rate: count of transitions in (id_trans, id_period)
  over count of cells of id_lulc_anterior in id_period

`trans_rate_areas()` returns a data.table with `id_lulc`, `id_period`
and `area`; `id_period` is the period whose *state* the area describes,
so the initial state carries the last period of `lulc_data`.

## Methods (by generic)

- `print(trans_rates_t)`: Print a trans_rates_t object, passing params
  to data.table print

## Functions

- `get_obs_trans_rates()`: Calculate observed transition rates from
  historical data. For each period and transition type, calculates the
  rate as the proportion of id_lulc_anterior cells that transitioned to
  id_lulc_posterior.

- `extrapolate_trans_rates()`: Return future transition rates using
  linear regression. For each id_run + id_trans, fits a linear model of
  rate vs period number and extrapolates to future periods. Negative
  predicted rates are set to 0.

- `trans_rate_areas()`: Replay a rate table forward from an observed
  state to recover the class areas it implies. Transitions not recorded
  in `rates` are implied to be zero, so the residual `1 - sum(rate)` of
  each class persists. This is what makes a solved trajectory
  recoverable from a trans_rates_t alone, and therefore comparable
  against the areas an allocation run actually realised.
