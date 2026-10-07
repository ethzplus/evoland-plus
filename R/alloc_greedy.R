#' Greedy (rank-and-fill) Allocation Methods
#'
#' @description
#' Deterministic allocation that fills each transition's demanded quantity with the cells of
#' highest transition potential: rank the candidate (cell, transition) pairs, then walk the
#' ranking and accept a pair when its cell has not changed yet and its transition still has
#' quantity left. Every cell changes at most once, and no transition exceeds its demand. There
#' is no randomness and no patch geometry, so the result is one map, the same on every run.
#'
#' This is the allocator behind lulcc's Ordered model (Fuchs et al. 2013) and SEALS'
#' rank-and-fill, and a useful comparator for the stochastic allocators: it concentrates
#' change on the most probable cells, which a single-map score such as the figure of merit
#' rewards, whereas an ensemble of unbiased realisations ([alloc_clumpy()]) reproduces where
#' change occurs.
#'
#' Two ways to settle the competition for a cell between transitions (`arbitration`):
#' * `"joint"` (default): one ranking of all pairs by adjusted potential, across transitions;
#'   a cell goes to whichever of its transitions comes first.
#' * `"ordered"`: transitions take their cells in turn, in the order given by `order`, each
#'   taking its best remaining cells; earlier transitions win contested cells. This is the
#'   Ordered procedure of Fuchs et al. (2013), with transitions in place of classes.
#'
#' The allocation works on `id_coord` rows only, not on a raster, so it applies to any
#' tessellation `coords_t` can describe.
#'
#' Entry points mirror [alloc_clumpy()]: `db$alloc_greedy(id_periods, ...)` runs and commits a
#' sequence of periods; `alloc_greedy_one_period(db, id_period_post, ...)` returns one period's
#' map without committing it.
#'
#' The ranking uses the allocation-ready potentials of [adjusted_trans_pot_v()], which are
#' scaled to each transition's rate, so that they are comparable across transitions. The
#' quantity per transition is the `count` in `trans_rates_t`, or, where it is missing, the
#' `rate` times the number of cells of the anterior class. Ties are broken by `id_trans`, then
#' `id_coord`, so the result does not depend on the order rows are read in.
#'
#' @return `alloc_greedy_one_period()`: an [lulc_data_t] with the simulated posterior LULC.
#'   `alloc_greedy()`: called for its side effects on `lulc_data_t` and `pred_data_t`.
#'
#' @references Fuchs, R., Herold, M., Verburg, P. H., & Clevers, J. G. P. W. (2013). A
#'   high-resolution and harmonized model approach for reconstructing and analysing historic
#'   land changes in Europe. Biogeosciences, 10(3), 1543-1559.
#'   https://doi.org/10.5194/bg-10-1543-2013
#'
#' @name alloc_greedy
#' @include alloc_clumpy.R
NULL

#' @describeIn alloc_greedy Allocate a single period and return the map without committing
#' it.
#'
#' @param db An [evoland_db] instance; uses its active `id_run`.
#' @param id_period_post Integer posterior period ID.
#' @param select_score Character; mlr3 measure ID for model selection.
#' @param select_maximize Logical; whether to maximise `select_score`.
#' @param arbitration Character, `"joint"` (default) or `"ordered"`; see Description.
#' @param order Integer vector of `id_trans`, the priority for `arbitration = "ordered"`;
#'   viable transitions not listed follow in `id_trans` order. Ignored for `"joint"`.
#' @param use_parent_trans_pot Logical; if TRUE, predict (or reuse) transition potentials
#'   under the parent run.
#' @param force_predict_trans_pot Logical; if TRUE, recompute transition potentials even if
#'   `trans_pot_t` already holds them for this run and period.
#' @export
alloc_greedy_one_period <- function(
  db,
  id_period_post,
  select_score,
  select_maximize,
  arbitration = c("joint", "ordered"),
  order = NULL,
  use_parent_trans_pot = FALSE,
  force_predict_trans_pot = FALSE
) {
  arbitration <- match.arg(arbitration)
  id_period_ant <- id_period_post - 1L

  predict_trans_pot_for_alloc(
    db = db,
    id_period_post = id_period_post,
    select_score = select_score,
    select_maximize = select_maximize,
    use_parent_trans_pot = use_parent_trans_pot,
    force = force_predict_trans_pot
  )

  viable_trans <- db$trans_meta_t[
    is_viable == TRUE,
    .(id_trans, id_lulc_anterior, id_lulc_posterior)
  ]
  data.table::setorder(viable_trans, id_trans)

  anterior <- db$fetch(
    "lulc_data_t",
    cols = c("id_coord", "id_lulc"),
    where = glue::glue("id_period = {id_period_ant}")
  )
  stopifnot("No LULC data for the anterior period" = nrow(anterior) > 0L)

  # demanded quantity per transition
  rates <- db$trans_rates_t[id_period == id_period_post, .(id_trans, count, rate)]
  n_anterior <- anterior[, .(n_anterior = .N), by = .(id_lulc_anterior = id_lulc)]
  quota <- rates[viable_trans, on = "id_trans"][
    n_anterior,
    on = "id_lulc_anterior",
    nomatch = NULL
  ][,
    quota := data.table::fifelse(
      is.na(count),
      as.integer(round(rate * n_anterior)),
      as.integer(count)
    )
  ][is.na(quota), quota := 0L]
  quota <- quota[viable_trans[, .(id_trans)], on = "id_trans"][is.na(quota), quota := 0L]

  # candidates: cells of the anterior class with a positive adjusted potential
  candidates <- db$adjusted_trans_pot_v(id_period_post)[
    value > 0,
    .(id_trans, id_coord, value)
  ][viable_trans, on = "id_trans", nomatch = NULL][
    anterior,
    on = .(id_coord, id_lulc_anterior = id_lulc),
    nomatch = NULL
  ]

  if (arbitration == "joint") {
    data.table::setorder(candidates, -value, id_trans, id_coord)
  } else {
    priority <- c(order, setdiff(viable_trans$id_trans, order))
    stopifnot("order must list viable id_trans only" = all(order %in% viable_trans$id_trans))
    candidates[, rank_trans := match(id_trans, priority)]
    data.table::setorder(candidates, rank_trans, -value, id_coord)
  }

  message(glue::glue(
    "Running greedy allocation ({arbitration}): period {id_period_ant} -> {id_period_post}"
  ))

  accepted <- greedy_fill(candidates, quota)

  posterior <- data.table::copy(anterior)
  posterior[
    candidates[accepted, .(id_coord, id_lulc_posterior)],
    on = "id_coord",
    id_lulc := i.id_lulc_posterior
  ]

  allocated <- candidates[accepted, .N, by = id_trans][quota, on = "id_trans"]
  short <- allocated[is.na(N) | N < quota]
  if (nrow(short) > 0L) {
    warning(glue::glue(
      "Not enough candidate cells to meet the demand of id_trans ",
      "{toString(short$id_trans)}; allocated {toString(data.table::fcoalesce(short$N, 0L))} ",
      "of {toString(short$quota)} cells"
    ))
  }
  message(glue::glue("  Changed {sum(accepted)} of {nrow(posterior)} cells"))

  data.table::data.table(
    id_run = db$id_run,
    id_coord = posterior$id_coord,
    id_lulc = as.integer(posterior$id_lulc),
    id_period = id_period_post
  ) |>
    as_lulc_data_t()
}

#' @describeIn alloc_greedy Allocate a contiguous sequence of periods, committing each one and
#' recomputing neighbour predictors in between; the method behind `db$alloc_greedy()`.
#'
#' @param self An [evoland_db] instance.
#' @param id_periods Integer vector of contiguous posterior period IDs to simulate.
#' @param update_neighbors Logical; whether to recompute neighbour predictors after the last
#'   requested period (default `TRUE`). Intermediate periods are always updated.
alloc_greedy <- function(
  self,
  id_periods,
  select_score,
  select_maximize,
  arbitration = c("joint", "ordered"),
  order = NULL,
  use_parent_trans_pot = FALSE,
  force_predict_trans_pot = FALSE,
  update_neighbors = TRUE
) {
  stopifnot(
    "id_periods must be a numeric vector" = is.numeric(id_periods),
    "id_periods must be contiguous" = all(diff(id_periods) == 1L),
    "id_run must be set" = !is.null(self$id_run),
    "id_periods must be in periods_t" = all(id_periods %in% self$periods_t$id_period)
  )
  arbitration <- match.arg(arbitration)

  for (id_period_post in id_periods) {
    lulc_result <- alloc_greedy_one_period(
      db = self,
      id_period_post = id_period_post,
      select_score = select_score,
      select_maximize = select_maximize,
      arbitration = arbitration,
      order = order,
      use_parent_trans_pot = use_parent_trans_pot,
      force_predict_trans_pot = force_predict_trans_pot
    )
    self$commit(lulc_result, "lulc_data_t", method = "upsert")
    if (update_neighbors || id_period_post != max(id_periods)) {
      self$upsert_new_neighbors(id_period_post)
    }
  }

  invisible(NULL)
}

#' Rank-and-fill as repeated top-n queries
#'
#' Accepts candidates as if walking them in row order, accepting a candidate when its cell is
#' still unclaimed and its transition still has quota left, but in rounds of top-n queries:
#' each transition takes its top `quota` remaining candidates; a cell taken by several keeps
#' the earliest. The picks are final up to the first candidate a transition that lost a cell
#' would take next (the "horizon"): before it, the walk sees exactly these picks. Picks before
#' the horizon are accepted, their cells and quotas removed, and the next round starts. Each
#' round accepts at least the earliest pick, and usually all of them.
#'
#' @param candidates data.table with `id_trans` and `id_coord`, unique per pair, ordered by
#'   allocation priority.
#' @param quota data.table with `id_trans` and `quota`, the number of cells per transition.
#' @return Logical vector along `candidates`, `TRUE` for the accepted rows.
#' @keywords internal
#' @noRd
greedy_fill <- function(candidates, quota) {
  cand <- candidates[, .(ord = .I, id_trans, id_coord)]
  remaining <- quota[quota > 0L, .(id_trans, quota)]
  accepted <- logical(nrow(candidates))
  while (nrow(remaining) > 0L) {
    cand <- cand[id_trans %in% remaining$id_trans]
    if (nrow(cand) == 0L) {
      break
    }
    cand[, rank_trans := data.table::rowid(id_trans)]
    cand[remaining, on = "id_trans", n_quota := i.quota]
    picks <- cand[rank_trans <= n_quota]
    picks[, wins := ord == min(ord), by = id_coord]
    losing <- picks[wins == FALSE, unique(id_trans)]
    horizon <- cand[id_trans %in% losing & rank_trans == n_quota + 1L, min(ord, Inf)]
    take <- picks[wins & ord < horizon]
    accepted[take$ord] <- TRUE
    remaining[take[, .N, by = id_trans], on = "id_trans", quota := quota - i.N]
    remaining <- remaining[quota > 0L]
    cand <- cand[!id_coord %in% take$id_coord]
  }
  accepted
}
