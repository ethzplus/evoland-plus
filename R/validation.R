#' Validation of simulated land use change
#'
#' @description
#' Compare a simulated map against the observed one, both starting from the same initial map:
#'
#' - [calc_figure_of_merit()]: Pontius' figure of merit and its components, cell by cell, with
#'   the value expected from random allocation for comparison;
#' - [calc_transition_similarity()]: fuzzy similarity of differences for one transition, which
#'   tolerates change placed near, rather than exactly on, the observed change;
#' - [create_change_map()]: the change map both of them start from.
#'
#' @name validation
NULL

#' Create change/difference map between two time periods
#'
#' @description
#' Creates a map showing only cells that changed between two time periods.
#' Cells that did not change are set to NA. This is used in the
#' "similarity of differences" approach.
#'
#' @param initial_map SpatRaster, initial time period
#' @param final_map SpatRaster, final time period
#' @param from_class Integer, optional filter for initial class (for specific transition)
#' @param to_class Integer, optional filter for final class (for specific transition)
#'
#' @return SpatRaster showing only changed cells. For general changes, values
#'   are the final class. For specific transitions, values indicate presence (1)
#'   or absence (NA) of that transition.
#'
#' @export
create_change_map <- function(
  initial_map,
  final_map,
  from_class = NULL,
  to_class = NULL
) {
  if (!is.null(from_class) && !is.null(to_class)) {
    # Specific transition: mark cells that transitioned from -> to
    change_map <- terra::ifel(
      initial_map == from_class & final_map == to_class,
      1L, # Mark as 1 where transition occurred
      NA_integer_ # NA elsewhere
    )
  } else if (is.null(from_class) && is.null(to_class)) {
    # General changes: show final class where changed, NA where unchanged
    change_map <- terra::ifel(
      initial_map != final_map,
      final_map,
      NA_integer_
    )
  } else {
    stop("Both from_class and to_class must be specified, or both must be NULL")
  }

  change_map
}

#' Figure of merit of simulated land use change
#'
#' @description
#' Compares simulated against observed change, both starting from the same initial map, with
#' Pontius' figure of merit (Pontius et al. 2008). Every cell that changed in either map is one
#' of:
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
#' @param initial,observed Initial and observed final land use: integer vectors of class IDs,
#'   aligned cell by cell, or single-layer SpatRasters on the same grid.
#' @param simulated Simulated final land use, in the same form. A list of them is treated as an
#'   ensemble: each member is weighted `1 / length(simulated)`, so the counts are expected
#'   counts over the ensemble.
#' @param by_transition Logical; if `TRUE`, report per transition instead of overall.
#'
#' @return A [data.table::data.table()]. Overall (`by_transition = FALSE`), one row with
#'   `hits`, `wrong_hits`, `misses`, `false_alarms`, `figure_of_merit`, `producers_accuracy`
#'   (`hits` over observed change), `users_accuracy` (`hits` over simulated change) and
#'   `figure_of_merit_null`. Per transition, one row per observed or simulated transition with
#'   `id_lulc_anterior`, `id_lulc_posterior`, the `observed` and `simulated` cell counts, `hits`,
#'   `figure_of_merit` (`hits` over the union of observed and simulated change) and
#'   `figure_of_merit_null`. Cells that are `NA` in any input are ignored.
#'
#' @references
#' Pontius, R. G., Boersma, W., Castella, J.-C., et al. (2008). Comparing the input, output, and
#' validation maps for several models of land change. The Annals of Regional Science, 42(1),
#' 11-37. https://doi.org/10.1007/s00168-007-0138-2
#'
#' @export
calc_figure_of_merit <- function(initial, observed, simulated, by_transition = FALSE) {
  as_classes <- function(x) {
    if (inherits(x, "SpatRaster")) as.vector(terra::values(x)) else as.vector(x)
  }
  members <- if (is.list(simulated) && !inherits(simulated, "SpatRaster")) {
    simulated
  } else {
    list(simulated)
  }
  initial <- as_classes(initial)
  observed <- as_classes(observed)
  stopifnot(
    "initial and observed must have the same number of cells" = {
      length(initial) == length(observed)
    }
  )

  cells <- data.table::rbindlist(lapply(members, function(member) {
    simulated <- as_classes(member)
    stopifnot(
      "every simulated map must have as many cells as initial" = {
        length(simulated) == length(initial)
      }
    )
    data.table::data.table(initial, observed, simulated, weight = 1 / length(members))
  }))
  cells <- stats::na.omit(cells)

  flows <- Reduce(
    function(x, y) merge(x, y, by = c("initial", "posterior"), all = TRUE),
    list(
      cells[
        observed != initial,
        .(observed = sum(weight)),
        by = .(initial, posterior = observed)
      ],
      cells[
        simulated != initial,
        .(simulated = sum(weight)),
        by = .(initial, posterior = simulated)
      ],
      cells[
        observed != initial & simulated == observed,
        .(hits = sum(weight)),
        by = .(initial, posterior = observed)
      ]
    )
  )
  for (column in c("observed", "simulated", "hits")) {
    data.table::set(flows, which(is.na(flows[[column]])), column, 0)
  }
  flows <- cells[, .(n_initial = sum(weight)), by = initial][flows, on = "initial"]
  flows[, hits_null := observed * simulated / n_initial]

  if (by_transition) {
    return(flows[
      order(initial, posterior),
      .(
        id_lulc_anterior = initial,
        id_lulc_posterior = posterior,
        observed,
        simulated,
        hits,
        figure_of_merit = hits / (observed + simulated - hits),
        figure_of_merit_null = hits_null / (observed + simulated - hits_null)
      )
    ])
  }

  union_null <- flows[,
    .(union = sum(observed) + sum(simulated) - sum(observed) * sum(simulated) / n_initial[1]),
    by = initial
  ][["union"]]

  cells[, {
    observed_change <- observed != initial
    simulated_change <- simulated != initial
    hits <- sum(weight[observed_change & simulated == observed])
    wrong_hits <- sum(weight[observed_change & simulated_change & simulated != observed])
    misses <- sum(weight[observed_change & !simulated_change])
    false_alarms <- sum(weight[!observed_change & simulated_change])
    .(
      hits = hits,
      wrong_hits = wrong_hits,
      misses = misses,
      false_alarms = false_alarms,
      figure_of_merit = hits / (hits + wrong_hits + misses + false_alarms),
      producers_accuracy = hits / (hits + wrong_hits + misses),
      users_accuracy = hits / (hits + wrong_hits + false_alarms),
      figure_of_merit_null = sum(flows[["hits_null"]]) / sum(union_null)
    )
  }]
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
#'   - observed_change: SpatRaster of observed changes
#'   - simulated_change: SpatRaster of simulated changes
#'   - similarity: Minimum of the two directional similarities; `NA` if the transition
#'     happened in neither map
#'   - sim_obs_to_sim: Mean membership of observed changes in the simulated change
#'   - sim_sim_to_obs: Mean membership of simulated changes in the observed change
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

  n_obs <- terra::global(!is.na(obs_change), "sum", na.rm = FALSE)[1, 1]
  n_sim <- terra::global(!is.na(sim_change), "sum", na.rm = FALSE)[1, 1]

  if (n_obs == 0 && n_sim == 0) {
    return(list(
      observed_change = obs_change,
      simulated_change = sim_change,
      similarity = NA_real_,
      sim_obs_to_sim = NA_real_,
      sim_sim_to_obs = NA_real_,
      similarity_map = NULL,
      n_observed = 0,
      n_simulated = 0
    ))
  }

  weights <- .build_weight_matrix(window_size, use_exp_decay, decay_divisor)
  obs_membership <- .change_membership(obs_change, weights)
  sim_membership <- .change_membership(sim_change, weights)

  obs_to_sim_map <- terra::mask(sim_membership, obs_change)
  sim_to_obs_map <- terra::mask(obs_membership, sim_change)

  mean_over_changes <- function(membership_map, n_changed) {
    if (n_changed == 0) {
      return(NA_real_)
    }
    terra::global(membership_map, "mean", na.rm = TRUE)[1, 1]
  }
  sim_obs_to_sim <- mean_over_changes(obs_to_sim_map, n_obs)
  sim_sim_to_obs <- mean_over_changes(sim_to_obs_map, n_sim)

  list(
    observed_change = obs_change,
    simulated_change = sim_change,
    similarity = min(sim_obs_to_sim, sim_sim_to_obs, na.rm = TRUE),
    sim_obs_to_sim = sim_obs_to_sim,
    sim_sim_to_obs = sim_sim_to_obs,
    similarity_map = obs_to_sim_map,
    n_observed = n_obs,
    n_simulated = n_sim
  )
}

#' Distance-decayed membership in a change map
#'
#' @param change_map SpatRaster, non-`NA` where the change happened
#' @param weight_matrix Matrix of distance weights, see [.build_weight_matrix()]
#'
#' @return SpatRaster with, for every cell, the weight of the nearest change within the window
#'   (0 if there is none)
#' @keywords internal
.change_membership <- function(change_map, weight_matrix) {
  changed <- terra::ifel(is.na(change_map), 0, 1)
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
