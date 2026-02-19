/*
This query is used to detect absences of predictor data.

- id_pred, the predictor
- id_period, the period
- is_extrapolated, whether the period is extrapolated or not
- id_run_origin, the requested run
- id_run_actual, the run for which data is actually present; either id_run or an ancestor thereof
- actual_runperiod_has_data,
whether data is present for the given predictor, period, and run
true if data exists for the exact tuple, or if data exists for id_pred, id_run in period 0
- origin_runperiod_has_data
whether data is present for the given predictor, period, and origin run
true if data exists for any actual run corresponding to the origin run
or if data exists for id_pred, id_run in period 0
- origin_run_has_data
whether data is present for the given predictor and origin run, separated by
extrapolation status

meant to be run with glue for interpolation, requiring
data sources:
- {pred_data_read_expr}
- {periods_read_expr}
- {pred_meta_read_expr}
- {runs_read_expr}
*/
with recursive
  data_present as (
    select distinct
      id_run,
      id_pred,
      id_period
    from
      {pred_data_read_expr}
  ),
  periods_present as (
    select
      id_period,
      is_extrapolated
    from
      {periods_read_expr}
  ),
  meta_present as (
    select
      id_pred
    from
      {pred_meta_read_expr}
  ),
  lineage as (
    select
      id_run as id_run_origin,
      id_run,
      parent_id_run,
      0 as inheritance_distance
    from
      {runs_read_expr}
    union all
    select
      l.id_run_origin,
      r.id_run,
      r.parent_id_run,
      l.inheritance_distance + 1 as inheritance_distance
    from
      {runs_read_expr} r
      join lineage l on r.id_run = l.parent_id_run
  ),
  base as (
    select
      m.id_pred,
      p.id_period,
      p.is_extrapolated, -- is the period extrapolated or not?
      l.id_run_origin, -- what id_run was requested?
      l.id_run as id_run_actual, -- what id_run are we actually looking at?
      l.inheritance_distance, -- how far removed from the requested id_run have we found actual data?
      (d.id_run is not null) as has_data, -- there is data for the tuple on this row
      (d0.id_run is not null) as has_static_data -- there is data for this id_pred and id_run in period 0
    from
      periods_present p
      cross join lineage l
      cross join meta_present m
      left join data_present d on d.id_run = l.id_run
      and d.id_pred = m.id_pred
      and d.id_period = p.id_period
      left join data_present d0 on d0.id_run = l.id_run
      and d0.id_pred = m.id_pred
      and d0.id_period = 0
  )
select
  id_pred,
  id_period,
  is_extrapolated,
  id_run_origin,
  id_run_actual,
  inheritance_distance,
  (
    has_static_data
    or has_data
  ) as actual_runperiod_has_data,
  (
    bool_or(has_static_data) over (
      partition by
        id_pred,
        id_run_origin,
        id_period
    )
    or bool_or(has_data) over (
      partition by
        id_pred,
        id_run_origin,
        id_period
    )
  ) as origin_runperiod_has_data,
  (
    bool_or(has_static_data) over (
      partition by
        id_pred,
        id_run_origin
    )
    or bool_or(has_data) over (
      partition by
        id_pred,
        id_run_origin,
        is_extrapolated
    )
  ) as origin_run_has_data
from
  base
order by all;
