/*
Provides inference time design matrix for transitions for a chosen transition and period.
Reads one resolved slice per predictor, preferring period-specific data over the
id_period = 0 static fallback.
Assumes that the read expressions return a single run.

Meant to be run with glue for interpolation, requiring
Filters:
- {id_trans}
- {id_period_anterior}
Data sources:
- {trans_meta_read_expr}
- {trans_preds_read_expr}
- {lulc_data_read_expr}
- {pred_data_read_expr}
*/
with
  trans_select as (
    -- find anterior land cover for the given transition
    select
      id_lulc_anterior
    from
      {trans_meta_read_expr}
    where
      {if (is.na(id_trans)) "is_viable = TRUE" else paste("id_trans =", id_trans)}
  ),
  anterior_coords as (
    -- we only infer the transition potential where id_coord had the anterior land cover
    -- in the previous period
    select
      id_coord,
      id_lulc
    from
      {lulc_data_read_expr} d,
      trans_select s
    where
      d.id_period = {id_period_anterior}
      and d.id_lulc = s.id_lulc_anterior
  ),
  preds_select as (
    -- we only fetch the predictors that are of relevance to the transition
    select distinct
      id_pred
    from
      {trans_preds_read_expr}
    {if (is.na(id_trans)) "" else paste("where id_trans =", id_trans)}
  ),
  -- a predictor may carry both a period-specific value and an id_period = 0
  -- fallback (e.g. a climate baseline overridden by a scenario projection).
  -- id_period = 0 is a *fallback*, so the period-specific slice must win; without
  -- this, first() below would pick either row nondeterministically.
  --
  -- Precedence is decided per *slice*, not per coordinate: if a predictor has any
  -- data at the target period, that whole slice is used and its id_period = 0 rows
  -- are ignored. This mirrors the data_present / best_run logic in
  -- get_evoland_db_read_expr() and is the same tradeoff -- falling through per
  -- id_coord would be harder to reason about and much more expensive. Only two
  -- periods are in scope here, so max(id_period) is the more specific of them.
  --
  -- Resolved against pred_data_t as a whole, deliberately before anterior_coords is
  -- joined in: within one transition's coordinates, a predictor whose slice exists
  -- but covers none of them is indistinguishable from one that has no slice at all,
  -- and would silently fall back to id_period = 0.
  pred_slice as (
    select
      id_pred,
      max(id_period) as id_period
    from
      {pred_data_read_expr}
    where
      id_period in (0, {id_period_anterior})
      and id_pred in (
        from
          preds_select
      )
    group by
      id_pred
  ),
  pred_data_long as (
    select
      ac.id_coord,
      pd.id_pred,
      pd."value"
    from
      {pred_data_read_expr} pd
      -- pred_slice picks the winning one of the two, but restate the candidates so that
      -- the scan still prunes to their partitions rather than reading every period
      semi join pred_slice ps on ps.id_pred = pd.id_pred
      and ps.id_period = pd.id_period
      inner join anterior_coords ac on ac.id_coord = pd.id_coord
    where
      pd.id_period in (0, {id_period_anterior})
      and pd.id_pred in (
        from
          preds_select
      )
  ),
  -- a resolved slice need not cover any of the coordinates selected above. Record
  -- those predictors as explicit NULLs, so that the pivot still yields a column for
  -- them: dropping out entirely would leave the model short of a feature, which is a
  -- worse failure than an absent value.
  pred_data_complete as (
    select
      id_coord,
      id_pred,
      "value"
    from
      pred_data_long
    union all
    select
      ac.id_coord,
      missing.id_pred,
      null as "value"
    from
      (
        select
          ps.id_pred
        from
          pred_slice ps
        anti join
          pred_data_long l on l.id_pred = ps.id_pred
      ) as missing
      cross join anterior_coords ac
  ),
  pred_data_wide as (
    pivot pred_data_complete on 'id_pred_' || id_pred using first("value")
    group by
      id_coord
  )
-- left join, so that a coordinate none of the resolved slices cover is still returned,
-- with NULL predictors, rather than going missing from the design matrix
select
  ac.id_coord,
  ac.id_lulc,
  pdata.* exclude (id_coord)
from
  anterior_coords ac
  left join pred_data_wide pdata on pdata.id_coord = ac.id_coord
