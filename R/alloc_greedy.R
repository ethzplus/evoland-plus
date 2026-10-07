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

  # make sure trans_pot_t holds raw potentials for this period (predicting them if needed);
  # adjusted_trans_pot_v() below reads them back scaled to the transition rates
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

  # the map we start from: one row per cell, its class in the anterior period
  anterior <- db$fetch(
    "lulc_data_t",
    cols = c("id_coord", "id_lulc"),
    where = glue::glue("id_period = {id_period_ant}")
  )
  stopifnot("No LULC data for the anterior period" = nrow(anterior) > 0L)

  # --- Quota: how many cells each transition must change ---
  # trans_rates_t holds a cell count where it is known (e.g. observed demand); otherwise the
  # rate, a share of the anterior class, which we turn into a count. Transitions without a
  # rate get quota 0, so they take no cells.
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
  # one row per viable transition, also those whose anterior class is absent from the map
  quota <- quota[viable_trans[, .(id_trans)], on = "id_trans"][is.na(quota), quota := 0L]

  # --- Candidates: (transition, cell) pairs that could be accepted ---
  # a transition can only happen in cells of its anterior class, and only where its adjusted
  # potential is positive
  candidates <- db$adjusted_trans_pot_v(id_period_post)[
    value > 0,
    .(id_trans, id_coord, value)
  ][viable_trans, on = "id_trans", nomatch = NULL][
    anterior,
    on = .(id_coord, id_lulc_anterior = id_lulc),
    nomatch = NULL
  ]

  # --- Order: the walk accepts candidates first come, first served ---
  # joint: one ranking of all pairs by potential, so a contested cell goes to whichever
  # transition is most likely there; ties broken by id_trans, then id_coord, so the result does
  # not depend on the order the rows were read in.
  # ordered: transition by transition in priority order, each by potential, so earlier
  # transitions win contested cells.
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

  # --- Walk: accept candidates while their cell and their transition's quota are free ---
  accepted <- greedy_fill(candidates, quota)

  # the posterior map: the anterior map with the accepted transitions applied
  posterior <- data.table::copy(anterior)
  posterior[
    candidates[accepted, .(id_coord, id_lulc_posterior)],
    on = "id_coord",
    id_lulc := i.id_lulc_posterior
  ]

  # a transition falls short of its quota when it runs out of candidate cells, e.g. because
  # other transitions took them first
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

#' Rank-and-fill: walk the candidates in order, accept while cell and quota are free
#'
#' Accepts a candidate (a transition at a cell) when its cell has not changed yet and its
#' transition still has quantity left, walking the candidates in row order. The caller sets the
#' order: by adjusted potential across all transitions for joint arbitration, or by transition
#' priority, then potential, for ordered arbitration.
#'
#' @param candidates data.table with `id_trans` and `id_coord` (positive integers), one row
#'   per pair, ordered by allocation priority.
#' @param quota data.table with `id_trans` and `quota`, the number of cells each transition may
#'   take; must list every `id_trans` in `candidates`.
#' @return Logical vector along `candidates`, `TRUE` for the accepted rows.
#' @keywords internal
#' @noRd
greedy_fill <- function(candidates, quota) {
  # The walk is inherently sequential (whether a candidate is accepted depends on every
  # candidate before it), so it runs as a single loop in C++, see greedy_fill_cpp(). An
  # equivalent formulation as repeated top-n queries in data.table was about 10x slower on
  # 4 M cells.

  # The C++ loop keeps its bookkeeping in plain arrays indexed from 1:
  # - cells: id_coord is already a positive integer, so it serves as the index directly; the
  #   array of claimed cells is as long as the largest id_coord (one bit per cell)
  # - transitions: the row of each candidate's id_trans in `quota`, whose quotas the loop
  #   counts down
  trans_index <- match(candidates[["id_trans"]], quota[["id_trans"]])
  stopifnot("quota must list every id_trans in candidates" = !anyNA(trans_index))

  greedy_fill_cpp(
    cell = candidates[["id_coord"]],
    trans = trans_index,
    quota = quota[["quota"]],
    n_cells = max(0L, candidates[["id_coord"]])
  )
}
