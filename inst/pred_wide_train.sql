/*
Provides design matrix for transitions (did / did not transition) for
non-extrapolated periods. cross-joins the static predictors to all periods.
Assumes that the read expressions return a single run.

Interpolated by `ducklake_db$get_query()`, requiring
Filters:
- {id_trans}
- {id_pred*}
- {id_period_anterior_filter}: SQL condition on id_period, `true` to train on all periods
Columns:
- {pred_cols*}: the pivoted column names, `id_pred_<n>` for each of `id_pred`
Data sources:
- {lulc_data_read_expr}
- {period_read_expr}
- {pred_data_read_expr}
- {trans_meta_read_expr}
*/
with
  period_select as (
    -- only train on non-extrapolation periods
    -- exclude the last period, since we won't have lulc data for the subsequent period
    -- to determine transition status
    select
      id_period as id_period_anterior
    from
      {period_read_expr}
    where
      not is_extrapolated
    qualify
      lead (id_period) over (
        order by
          id_period
      ) is not null
      -- caller's restriction, e.g. to hold out the last observed transition; it is applied
      -- after the window, so that the last observed period is still recognised as such
      and {id_period_anterior_filter}
  ),
  trans_select as (
    select
      id_lulc_anterior,
      id_lulc_posterior
    from
      {trans_meta_read_expr}
    where
      id_trans = {id_trans}
  ),
  trans_result as (
    -- at extrapolation time, we know the data at ant.id_period; we train on the data
    -- associated with that period as well
    select
      ant.id_coord,
      ant.id_period,
      (ts.id_lulc_posterior = post.id_lulc) as did_transition
    from
      {lulc_data_read_expr} as ant
      -- anterior timesteps where a transition might be observed
      inner join period_select as ps on ant.id_period = ps.id_period_anterior
      -- coordinates where a transition might be observed
      inner join {lulc_data_read_expr} as post on post.id_coord = ant.id_coord
      and post.id_period = ant.id_period + 1
      -- land use class from which a transition might be observed
      inner join trans_select as ts on ant.id_lulc = ts.id_lulc_anterior
  ),
  -- concatenate dynamic and static predictor data
  pred_data_long as (
    select
      id_coord,
      id_period,
      id_pred,
      value,
      -- source period the value was read from; used to prefer period-specific
      -- data over the id_period = 0 static fallback below
      d.id_period as src_period
    from
      {pred_data_read_expr} d
      inner join period_select s on d.id_period = s.id_period_anterior
    where
      d.id_period >= 1
      and d.id_pred in ({id_pred*})
    union all
    select
      p0.id_coord,
      periods.id_period,
      p0.id_pred,
      p0.value,
      0 as src_period
    from
      {pred_data_read_expr} as p0
      -- cross join so the same static data is present in every period
      cross join (
        select distinct
          id_period
        from
          trans_result
        where
          id_period >= 1
      ) as periods
    where
      p0.id_period = 0
      and id_pred in ({id_pred*})
  ),
  -- a predictor may carry both a period-specific value and an id_period = 0
  -- fallback (e.g. a climate baseline overridden by a scenario projection).
  -- id_period = 0 is a *fallback*, so the period-specific slice must win; without
  -- this, first() below would pick either row nondeterministically.
  --
  -- Precedence is decided per *slice* -- one (id_pred, id_period) pair -- not per
  -- coordinate: if a predictor has any data at a training period, that whole slice
  -- is used and its period-0 fallback is ignored for that period. This mirrors the
  -- data_present / best_run logic in get_evoland_db_read_expr() and is the same
  -- tradeoff: falling through per id_coord would be harder to reason about and much
  -- more expensive.
  pred_src_present as (
    select
      id_pred,
      id_period,
      max(src_period) as src_period
    from
      pred_data_long
    group by
      id_pred,
      id_period
  ),
  pred_data_resolved as (
    select
      l.id_coord,
      l.id_period,
      l.id_pred,
      l.value
    from
      pred_data_long l
    semi join
      pred_src_present p
      using (id_pred, id_period, src_period)
  ),
  -- the explicit column list fixes the design matrix's columns, also for a predictor
  -- without data. The parentheses matter: without them, the IN list binds to id_pred
  -- alone, and DuckDB (>= 2.0) cannot infer the pivot values of a CTE.
  pred_wide as (
    pivot pred_data_resolved on ('id_pred_' || id_pred) in ({pred_cols*}) using first(value)
    group by
      id_coord,
      id_period
  )
-- id_coord and id_period_anterior identify each row. The fitting workers drop them before
-- training, but they are kept for spatial or temporal resampling (ethzplus/evoland-plus#90).
-- id_period_anterior could also become a predictor itself: as such, it would make the
-- transition model time-inhomogeneous (its probabilities change with the period); a
-- predictor counting the periods since a cell last changed would make it semi-Markov
-- (dependent on how long a cell has been in its class), which the current first-order
-- model, conditioned on the anterior class only, is not.
select
  tr.id_coord,
  tr.id_period as id_period_anterior,
  tr.did_transition,
  pdata.* exclude (id_coord, id_period)
from
  trans_result as tr
  left join pred_wide as pdata on tr.id_coord = pdata.id_coord
  and tr.id_period = pdata.id_period
-- DuckDB returns rows in no fixed order; subsampling and order-sensitive learners (ranger)
-- replay only on a fixed one
order by
  tr.id_coord,
  tr.id_period;
