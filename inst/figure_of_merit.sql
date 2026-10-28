/*
Pontius' figure of merit of simulated against observed land use change, per simulated run and,
optionally, per transition. Cells missing from any of the three maps are ignored.

Interpolated by `ducklake_db$get_query()`, requiring
Filters:
- {id_period_anterior}
- {id_period_post}
- {exclude_filter}: empty, or `and` conditions on a (initial) and o (observed) classes
Data sources:
- {reference_read_expr}: lulc_data_t of the reference run, holding the initial and observed map
- {simulated_read_expr}: lulc_data_t rows (id_run, id_coord, id_lulc) of each simulated run's
  map at id_period_post
Output:
- {`result`}: "overall" or "per_transition"
*/
with
  reference as (
    select
      id_coord,
      id_period,
      id_lulc
    from
      {reference_read_expr}
    where
      id_period in ({id_period_anterior}, {id_period_post})
  ),
  cells as (
    select
      s.id_run,
      a.id_lulc as initial,
      o.id_lulc as observed,
      s.id_lulc as simulated
    from
      reference a
      inner join reference o on o.id_coord = a.id_coord
      and o.id_period = {id_period_post}
      inner join ({simulated_read_expr}) s on s.id_coord = a.id_coord
    where
      a.id_period = {id_period_anterior}
      {exclude_filter}
  ),
  n_initial as (
    select
      id_run,
      initial,
      count(*) as n_initial
    from
      cells
    group by
      id_run,
      initial
  ),
  observed_flows as (
    select
      id_run,
      initial,
      observed as posterior,
      count(*) as observed,
      count(*) filter (
        where
          simulated = observed
      ) as hits
    from
      cells
    where
      observed != initial
    group by
      id_run,
      initial,
      observed
  ),
  simulated_flows as (
    select
      id_run,
      initial,
      simulated as posterior,
      count(*) as simulated
    from
      cells
    where
      simulated != initial
    group by
      id_run,
      initial,
      simulated
  ),
  -- hits_null: hits expected when each transition's simulated quantity is placed at random
  -- among the cells of its initial class
  flows as (
    select
      id_run,
      initial,
      posterior,
      coalesce(o.observed, 0) as observed,
      coalesce(s.simulated, 0) as simulated,
      coalesce(o.hits, 0) as hits,
      n.n_initial,
      coalesce(o.observed, 0) * coalesce(s.simulated, 0) / n.n_initial as hits_null
    from
      observed_flows o
      full join simulated_flows s using (id_run, initial, posterior)
      inner join n_initial n using (id_run, initial)
  ),
  per_transition as (
    select
      id_run,
      initial as id_lulc_anterior,
      posterior as id_lulc_posterior,
      observed::integer as observed,
      simulated::integer as simulated,
      hits::integer as hits,
      hits / (observed + simulated - hits) as figure_of_merit,
      hits_null / (observed + simulated - hits_null) as figure_of_merit_null
    from
      flows
    order by
      id_run,
      initial,
      posterior
  ),
  -- union of observed and simulated change expected under random allocation, per initial class
  null_per_initial as (
    select
      id_run,
      sum(hits_null) as hits_null,
      sum(observed) + sum(simulated) - sum(observed) * sum(simulated) / any_value(n_initial)
        as union_null
    from
      flows
    group by
      id_run,
      initial
  ),
  components as (
    select
      id_run,
      count(*) filter (
        where
          observed != initial
          and simulated = observed
      ) as hits,
      count(*) filter (
        where
          observed != initial
          and simulated != initial
          and simulated != observed
      ) as wrong_hits,
      count(*) filter (
        where
          observed != initial
          and simulated = initial
      ) as misses,
      count(*) filter (
        where
          observed = initial
          and simulated != initial
      ) as false_alarms
    from
      cells
    group by
      id_run
  ),
  overall as (
    select
      c.id_run,
      c.hits::integer as hits,
      c.wrong_hits::integer as wrong_hits,
      c.misses::integer as misses,
      c.false_alarms::integer as false_alarms,
      c.hits / (c.hits + c.wrong_hits + c.misses + c.false_alarms) as figure_of_merit,
      c.hits / (c.hits + c.wrong_hits + c.misses) as producers_accuracy,
      c.hits / (c.hits + c.wrong_hits + c.false_alarms) as users_accuracy,
      n.hits_null / n.union_null as figure_of_merit_null
    from
      components c
      left join (
        select
          id_run,
          sum(hits_null) as hits_null,
          sum(union_null) as union_null
        from
          null_per_initial
        group by
          id_run
      ) n using (id_run)
    order by
      c.id_run
  )
select
  *
from
  {`result`}
