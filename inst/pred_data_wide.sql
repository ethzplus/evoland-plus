/*
Provides inference time design matrix for transitions for a chosen transition and period.
Cross-joins the static predictors to all periods.
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
      id_trans = {id_trans}
  ),
  anterior_coords as (
    -- we only infer the transition potential where id_coord had the anterior land cover
    -- in the previous period
    select
      id_coord
    from
      {lulc_data_read_expr}
    where
      id_period = {id_period_anterior}
      and id_lulc = (
        from
          trans_select
      )
  ),
  preds_select as (
    -- we only fetch the predictors that are of relevance to the transition
    select
      id_pred
    from
      {trans_preds_read_expr}
    where
      id_trans = {id_trans}
  ),
  pred_data_long as (
    select
      ac.id_coord,
      pd.id_pred,
      pd.id_period,
      "value"
    from
      {pred_data_read_expr} pd
      inner join anterior_coords ac on ac.id_coord = pd.id_coord
    where
      -- always include static data along with the target period
      id_period in (0, {id_period_anterior})
      and id_pred in (
        from
          preds_select
      )
  ),
  -- a predictor may carry both a period-specific value and an id_period = 0
  -- fallback (e.g. a climate baseline overridden by a scenario projection).
  -- id_period = 0 is a *fallback*, so the period-specific row must win; without
  -- this, first() below would pick either row nondeterministically.
  pred_data_resolved as (
    select
      id_coord,
      id_pred,
      "value"
    from
      pred_data_long
    qualify
      row_number() over (
        partition by
          id_coord,
          id_pred
        order by
          id_period desc
      ) = 1
  )
pivot pred_data_resolved on 'id_pred_' || id_pred using first("value")
group by
  id_coord
