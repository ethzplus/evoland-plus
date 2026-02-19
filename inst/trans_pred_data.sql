/*
Provides design matrix for transitions (did / did not transition) for
non-extrapolated periods. cross-joins the static predictors to all periods.

meant to be run with glue for interpolation, requiring
filters:
- {id_trans}
- {pred_filter}
data sources:
- {lulc_data_read_expr}
- {period_read_expr}
- {pred_data_read_expr}
- {trans_meta_read_expr}
*/
with
  period_select as (
    select
      id_period
    from
      {period_read_expr}
    where
      not is_extrapolated
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
    select
      -- curr.id_run,
      curr.id_coord,
      curr.id_period,
      (prev.id_lulc != curr.id_lulc) as did_transition
    from
      {lulc_data_read_expr} as prev
      inner join trans_select as ts on prev.id_lulc = ts.id_lulc_anterior
      inner join {lulc_data_read_expr} as curr on curr.id_coord = prev.id_coord
      -- and curr.id_run = prev.id_run
      and curr.id_period = prev.id_period + 1
    where
      curr.id_period in (
        from
          period_select
      )
  ),
  pred_data_long as (
    select
      -- id_run,
      id_coord,
      id_period,
      id_pred,
      value
    from
      {pred_data_read_expr}
    where
      id_period >= 1
      and id_period in (
        from
          period_select
      )
      and id_pred in ({toString (id_pred)})
    union all
    select
      -- p0.id_run,
      p0.id_coord,
      periods.id_period,
      p0.id_pred,
      p0.value
    from
      {pred_data_read_expr} as p0
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
      and id_pred in ({toString (id_pred)})
  ),
  pred_data_wide as (
    pivot pred_data_long on 'id_pred_' || id_pred using first(value)
    group by
      -- id_run,
      id_coord,
      id_period
  )
select
  -- tr.id_run,
  tr.id_coord,
  tr.id_period,
  tr.did_transition,
  pdata.* exclude (id_coord, id_period)
from
  trans_result as tr
  left join pred_data_wide as pdata on tr.id_coord = pdata.id_coord
  -- and tr.id_run = pdata.id_run
  and tr.id_period = pdata.id_period;
