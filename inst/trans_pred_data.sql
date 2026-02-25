/*
Provides design matrix for transitions (did / did not transition) for
non-extrapolated periods. cross-joins the static predictors to all periods.
Assumes that the read expressions return a single run.

Meant to be run with glue for interpolation, requiring
Filters:
- {id_trans}
- {toString(id_pred)}
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
      value
    from
      {pred_data_read_expr} d
      inner join period_select s on d.id_period = s.id_period_anterior
    where
      d.id_period >= 1
      and d.id_pred in ({toString(id_pred)})
    union all
    select
      p0.id_coord,
      periods.id_period,
      p0.id_pred,
      p0.value
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
      and id_pred in ({toString(id_pred)})
  ),
  pred_data_wide as (
    pivot pred_data_long on 'id_pred_' || id_pred using first(value)
    group by
      id_coord,
      id_period
  )
select
  tr.id_coord,
  tr.id_period as id_period_anterior,
  tr.did_transition,
  pdata.* exclude (id_coord, id_period)
from
  trans_result as tr
  left join pred_data_wide as pdata on tr.id_coord = pdata.id_coord
  and tr.id_period = pdata.id_period;
