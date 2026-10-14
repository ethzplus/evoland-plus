#' Validation of simulated land use change
#'
#' @description
#' Compare a simulated map against the observed one, both starting from the same initial map:
#'
#' - [lulc_crosstab_v()] (`evoland_db$lulc_crosstab_v()`): cross-tabulation of land use between
#'   two periods, of one run or of two;
#' - [figure_of_merit_v()] (`evoland_db$figure_of_merit_v()`): Pontius' figure of merit and its
#'   components, cell by cell, with the value expected from random allocation for comparison;
#' - [calc_transition_similarity()]: fuzzy similarity of differences for one transition, which
#'   tolerates change placed near, rather than exactly on, the observed change.
#'
#' @name validation
NULL

#' Where land use changed between two maps
#'
#' @param initial_map,final_map SpatRasters on the same grid
#' @param from_class,to_class Integer, optional: only count this transition. Give both or
#'   neither.
#'
#' @return Logical SpatRaster: `TRUE` where the land use changed (from `from_class` to
#'   `to_class`, if given), `FALSE` where it did not, `NA` where either map is `NA`.
#' @keywords internal
create_change_map <- function(initial_map, final_map, from_class = NULL, to_class = NULL) {
  stopifnot(
    "Both from_class and to_class must be specified, or both must be NULL" = {
      is.null(from_class) == is.null(to_class)
    }
  )
  if (is.null(from_class)) {
    return(initial_map != final_map)
  }
  initial_map == from_class & final_map == to_class
}

#' Cross-tabulation of land use between two periods
#'
#' @description
#' Counts the cells by their land use class in one period and in another: the transition
#' matrix, in long form. Each period is read from its own run, through that run's lineage, so
#' the same call gives observed change (both periods from one run), simulated change (a
#' simulated run from its initial period), or agreement between an observed and a simulated
#' map of the same period (two runs, one period).
#'
#' @param self An [evoland_db] instance
#' @param id_period_anterior,id_period_post Integer, the two periods to compare
#' @param id_run_anterior Integer, run to read `id_period_anterior` from; defaults to the active
#'   run
#' @param id_run_post Integer, run to read `id_period_post` from; defaults to `id_run_anterior`
#'
#' @return A [data.table::data.table()] with `id_lulc_anterior`, `id_lulc_posterior` and
#'   `n_cells`, one row per pair of classes that occurs. Cells missing from either map are
#'   ignored. Use [data.table::dcast()] for the matrix form.
#'
#' @keywords internal
lulc_crosstab_v <- function(
  self,
  id_period_anterior,
  id_period_post,
  id_run_anterior = self$id_run,
  id_run_post = id_run_anterior
) {
  stopifnot(
    "id_period_anterior must be a single integer" = {
      length(id_period_anterior) == 1L && as.integer(id_period_anterior) == id_period_anterior
    },
    "id_period_post must be a single integer" = {
      length(id_period_post) == 1L && as.integer(id_period_post) == id_period_post
    },
    "id_run_anterior must be a single integer; set an active run or pass it" = {
      length(id_run_anterior) == 1L && as.integer(id_run_anterior) == id_run_anterior
    },
    "id_run_post must be a single integer" = {
      length(id_run_post) == 1L && as.integer(id_run_post) == id_run_post
    }
  )

  self$get_query(
    r"{
    select
      a.id_lulc as id_lulc_anterior,
      p.id_lulc as id_lulc_posterior,
      count(*)::integer as n_cells
    from
      {anterior_read_expr} a
      inner join {post_read_expr} p using (id_coord)
    where
      a.id_period = {id_period_anterior}
      and p.id_period = {id_period_post}
    group by
      a.id_lulc,
      p.id_lulc
    order by
      a.id_lulc,
      p.id_lulc
    }",
    anterior_read_expr = get_run_read_expr(self, "lulc_data_t", id_run_anterior),
    post_read_expr = get_run_read_expr(self, "lulc_data_t", id_run_post),
    id_period_anterior = as.integer(id_period_anterior),
    id_period_post = as.integer(id_period_post)
  )
}

#' Figure of merit of simulated land use change
#'
#' @description
#' Compares simulated against observed change with Pontius' figure of merit (Pontius et al.
#' 2008). The initial and observed maps are read from a reference run, the simulated maps from
#' one or more other runs, each through its own run lineage. Every cell that changed in either
#' the observed or a simulated map is one of:
#'
#' - **hit**: observed change, simulated as change to the observed class;
#' - **wrong hit**: observed change, simulated as change to another class;
#' - **miss**: observed change, simulated as persistence;
#' - **false alarm**: observed persistence, simulated as change.
#'
#' The figure of merit is `hits / (misses + hits + wrong hits + false alarms)`, the share of the
#' union of observed and simulated change that the simulation got right. Cells that persisted in
#' both maps do not enter it, so, unlike overall agreement, it is not inflated by a mostly
#' unchanging landscape. Its value scales with how much change there is to get right, so compare
#' it against `figure_of_merit_null`, the value expected when the simulated quantity of each
#' transition is placed at random among the cells of its initial class.
#'
#' Cells whose change is not the model's to get right, such as a class whose transitions are
#' imposed deterministically, can be left out with `exclude_id_lulc`: a cell is ignored if its
#' initial or its observed class is one of them.
#'
#' For an ensemble of runs, the expected counts are the means of the per-run counts; recompute
#' the ratios from those means rather than averaging the per-run ratios.
#'
#' @param self An [evoland_db] instance
#' @param id_period_anterior Integer, period of the initial map
#' @param id_period_post Integer, period of the observed and simulated maps
#' @param id_run_reference Integer, run holding the initial and observed maps
#' @param id_run_simulated Integer vector, runs holding the simulated maps; defaults to the
#'   active run
#' @param by_transition Logical; if `TRUE`, report per transition instead of overall.
#' @param exclude_id_lulc Integer vector of classes; cells whose initial or observed class is
#'   one of them are ignored. Default none.
#'
#' @return A [data.table::data.table()] with one row per simulated `id_run` (and transition).
#'   Overall (`by_transition = FALSE`): `hits`, `wrong_hits`, `misses`, `false_alarms`,
#'   `figure_of_merit`, `producers_accuracy` (`hits` over observed change), `users_accuracy`
#'   (`hits` over simulated change) and `figure_of_merit_null`. Per transition:
#'   `id_lulc_anterior`, `id_lulc_posterior`, the `observed` and `simulated` cell counts,
#'   `hits`, `figure_of_merit` (`hits` over the union of observed and simulated change) and
#'   `figure_of_merit_null`. Cells missing from any of the three maps are ignored.
#'
#' @references
#' Pontius, R. G., Boersma, W., Castella, J.-C., et al. (2008). Comparing the input, output, and
#' validation maps for several models of land change. The Annals of Regional Science, 42(1),
#' 11-37. https://doi.org/10.1007/s00168-007-0138-2
#'
#' @keywords internal
figure_of_merit_v <- function(
  self,
  id_period_anterior,
  id_period_post,
  id_run_reference,
  id_run_simulated = self$id_run,
  by_transition = FALSE,
  exclude_id_lulc = integer(0)
) {
  stopifnot(
    "id_period_anterior must be a single integer" = {
      length(id_period_anterior) == 1L && as.integer(id_period_anterior) == id_period_anterior
    },
    "id_period_post must be a single integer" = {
      length(id_period_post) == 1L && as.integer(id_period_post) == id_period_post
    },
    "id_run_reference must be a single integer" = {
      length(id_run_reference) == 1L && as.integer(id_run_reference) == id_run_reference
    },
    "id_run_simulated must be an integer vector" = {
      length(id_run_simulated) >= 1L && all(as.integer(id_run_simulated) == id_run_simulated)
    },
    "exclude_id_lulc must be an integer vector" = {
      all(as.integer(exclude_id_lulc) == exclude_id_lulc)
    }
  )

  # each run is read through its own lineage
  lulc_read_expr <- function(id_run) get_run_read_expr(self, "lulc_data_t", id_run)
  simulated_read_expr <- glue::glue_sql_collapse(
    lapply(id_run_simulated, function(id_run) {
      glue::glue_sql(
        "select {id_run}::integer as id_run, id_coord, id_lulc
        from {lulc_read_expr(id_run)} where id_period = {id_period_post}",
        .con = self$connection
      )
    }),
    sep = " union all "
  )
  exclude_filter <- if (length(exclude_id_lulc) > 0L) {
    glue::glue_sql(
      "and a.id_lulc not in ({exclude_id_lulc*}) and o.id_lulc not in ({exclude_id_lulc*})",
      exclude_id_lulc = as.integer(exclude_id_lulc),
      .con = self$connection
    )
  } else {
    DBI::SQL("")
  }

  self$get_query(
    read_sql("figure_of_merit.sql"),
    reference_read_expr = lulc_read_expr(id_run_reference),
    simulated_read_expr = simulated_read_expr,
    exclude_filter = exclude_filter,
    id_period_anterior = as.integer(id_period_anterior),
    id_period_post = as.integer(id_period_post),
    result = if (by_transition) "per_transition" else "overall"
  )
}

#' Compute fuzzy similarity of differences for transition validation
#'
#' @description
#' Implements the "similarity of differences" approach from Dinamica EGO (Soares-Filho et al.
#' 2009), after the fuzzy set comparison of Hagen (2003). Compares the spatial pattern of a
#' specific transition in observed vs simulated maps, allowing for spatial tolerance.
#'
#' @details
#' Both maps are reduced to where the transition happened. For each cell that changed in one
#' map, its membership in the other map's change is the distance decay to the nearest cell that
#' changed there, within the window: 1 for a change in the same cell, `exp(-d / decay_divisor)`
#' for one `d` cells away (or 1 anywhere in the window without decay), 0 if there is none. The
#' directional similarity is the mean of these memberships over the changed cells of the first
#' map only; cells that changed in neither map do not enter it. The overall similarity is the
#' minimum of the two directions, so a simulation cannot score well by placing much more (or
#' much less) change than was observed.
#'
#' @param initial_map SpatRaster, initial LULC state
#' @param observed_map SpatRaster, observed final LULC state
#' @param simulated_map SpatRaster, simulated final LULC state
#' @param from_class Integer, initial class of transition
#' @param to_class Integer, final class of transition
#' @param window_size Integer, size of moving window (must be odd). Default 11.
#' @param use_exp_decay Logical, use exponential decay? Default TRUE.
#' @param decay_divisor Numeric, attenuation factor. Default 2.
#'
#' @return List with:
#'   - observed_change: Logical SpatRaster, `TRUE` where the transition was observed
#'   - simulated_change: Logical SpatRaster, `TRUE` where the transition was simulated
#'   - similarity: Minimum of the two directional similarities; `NA` if the transition
#'     happened in neither map
#'   - similarity_observed_to_simulated: Mean membership of observed changes in the simulated change
#'   - similarity_simulated_to_observed: Mean membership of simulated changes in the observed change
#'   - similarity_map: SpatRaster of the observed-to-simulated membership at each observed
#'     change (`NA` elsewhere)
#'   - n_observed: Number of cells with observed transition
#'   - n_simulated: Number of cells with simulated transition
#'
#' @references
#' Hagen, A. (2003). Fuzzy set approach to assessing similarity of categorical maps.
#' International Journal of Geographical Information Science, 17(3), 235-249.
#' https://doi.org/10.1080/13658810210157822
#'
#' Soares-Filho, B. S., Rodrigues, H. O., & Costa, W. L. (2009). Modeling Environmental
#' Dynamics with Dinamica EGO. Centro de Sensoriamento Remoto, UFMG.
#'
#' @export
calc_transition_similarity <- function(
  initial_map,
  observed_map,
  simulated_map,
  from_class,
  to_class,
  window_size = 11L, # TODO insert warning if domain is too small for window
  use_exp_decay = TRUE,
  decay_divisor = 2.0
) {
  stopifnot(
    "window_size must be odd" = (window_size %% 2L) == 1L,
    "window_size must be positive" = window_size > 0L,
    "decay_divisor must be positive" = decay_divisor > 0
  )

  obs_change <- create_change_map(initial_map, observed_map, from_class, to_class)
  sim_change <- create_change_map(initial_map, simulated_map, from_class, to_class)

  n_obs <- terra::global(obs_change, "sum", na.rm = TRUE)[1, 1]
  n_sim <- terra::global(sim_change, "sum", na.rm = TRUE)[1, 1]

  if (n_obs == 0 && n_sim == 0) {
    return(list(
      observed_change = obs_change,
      simulated_change = sim_change,
      similarity = NA_real_,
      similarity_observed_to_simulated = NA_real_,
      similarity_simulated_to_observed = NA_real_,
      similarity_map = NULL,
      n_observed = 0,
      n_simulated = 0
    ))
  }

  weights <- .build_weight_matrix(window_size, use_exp_decay, decay_divisor)
  obs_membership <- .change_membership(obs_change, weights)
  sim_membership <- .change_membership(sim_change, weights)

  obs_to_sim_map <- terra::mask(sim_membership, obs_change, maskvalues = c(NA, FALSE))
  sim_to_obs_map <- terra::mask(obs_membership, sim_change, maskvalues = c(NA, FALSE))

  mean_over_changes <- function(membership_map, n_changed) {
    if (n_changed == 0) {
      return(NA_real_)
    }
    terra::global(membership_map, "mean", na.rm = TRUE)[1, 1]
  }
  similarity_observed_to_simulated <- mean_over_changes(obs_to_sim_map, n_obs)
  similarity_simulated_to_observed <- mean_over_changes(sim_to_obs_map, n_sim)

  list(
    observed_change = obs_change,
    simulated_change = sim_change,
    similarity = min(
      similarity_observed_to_simulated,
      similarity_simulated_to_observed,
      na.rm = TRUE
    ),
    similarity_observed_to_simulated = similarity_observed_to_simulated,
    similarity_simulated_to_observed = similarity_simulated_to_observed,
    similarity_map = obs_to_sim_map,
    n_observed = n_obs,
    n_simulated = n_sim
  )
}

#' Distance-decayed membership in a change map
#'
#' @param change_map Logical SpatRaster, `TRUE` where the change happened, see
#'   [create_change_map()]
#' @param weight_matrix Matrix of distance weights, see [.build_weight_matrix()]
#'
#' @return SpatRaster with, for every cell, the weight of the nearest change within the window
#'   (0 if there is none)
#' @keywords internal
.change_membership <- function(change_map, weight_matrix) {
  changed <- terra::ifel(change_map, 1, 0)
  changed <- terra::classify(changed, cbind(NA, 0))
  terra::focal(changed, w = weight_matrix, fun = "max", na.rm = TRUE, fillvalue = 0)
}

#' Build distance weight matrix for the fuzzy similarity window
#'
#' @param window_size Integer, size of window (must be odd)
#' @param use_exp_decay Logical, use exponential decay?
#' @param decay_divisor Numeric, attenuation factor
#'
#' @return Matrix of weights: `exp(-d / decay_divisor)` for Euclidean distance `d` from the
#'   centre cell, or 1 throughout without decay
#' @keywords internal
.build_weight_matrix <- function(window_size, use_exp_decay, decay_divisor) {
  offsets <- seq_len(window_size) - (window_size + 1) / 2
  distance <- sqrt(outer(offsets^2, offsets^2, "+"))
  if (use_exp_decay) exp(-distance / decay_divisor) else distance * 0 + 1
}
