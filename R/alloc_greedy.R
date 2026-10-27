#' Greedy (rank-and-fill) allocator
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
#' rewards, whereas an ensemble of unbiased realisations ([alloc_clumpy_spec()]) reproduces
#' where change occurs.
#'
#' Two ways to settle the competition for a cell between transitions (`arbitration`):
#' * `"joint"` (default): one ranking of all pairs by adjusted potential, across transitions;
#'   a cell goes to whichever of its transitions comes first.
#' * `"ordered"`: transitions take their cells in turn, in the order given by `order`, each
#'   taking its best remaining cells; earlier transitions win contested cells. This is the
#'   Ordered procedure of Fuchs et al. (2013), with transitions in place of classes.
#'
#' **Tie-breaking is arbitrary.** To be deterministic, the ranking needs a total order, so pairs
#' with equal potential are ranked by secondary keys that carry no meaning:
#' * `"joint"`: by `id_trans` (lower first), then `id_coord`. Where two transitions have equal
#'   potential at a cell, the one with the lower `id_trans` gets it; where equally ranked pairs
#'   compete for the last cells of a quota, those with the lower `id_coord` are taken. The
#'   `"joint"` ranking is therefore not symmetric in the transitions: renumbering them can change
#'   the result wherever potentials tie.
#' * `"ordered"`: within a transition, by `id_coord`.
#'
#' Ties are rare for continuous potentials, but common for coarse ones (e.g. classification
#' trees, or potentials rounded on input), and `id_coord` usually follows the raster's row
#' order, so tied cells are taken from the top of the map first.
#'
#' The allocation works on `id_coord` rows only, not on a raster, so it applies to any
#' tessellation `coords_t` can describe. The ranking uses the allocation-ready potentials of
#' [adjusted_trans_pot_v()], which are scaled to each transition's rate, so that they are
#' comparable across transitions.
#'
#' @param arbitration Character, `"joint"` (default) or `"ordered"`; how transitions compete for
#'   a cell. Both break ties in potential by ids, which is arbitrary; see Description.
#' @param order Integer vector of `id_trans`, the priority for `arbitration = "ordered"`;
#'   viable transitions not listed follow in `id_trans` order. Ignored for `"joint"`.
#'
#' @return An allocator specification for [allocate()].
#'
#' @references Fuchs, R., Herold, M., Verburg, P. H., & Clevers, J. G. P. W. (2013). A
#'   high-resolution and harmonized model approach for reconstructing and analysing historic
#'   land changes in Europe. Biogeosciences, 10(3), 1543-1559.
#'   https://doi.org/10.5194/bg-10-1543-2013
#'
#' @include allocate.R
#' @export
alloc_greedy_spec <- function(arbitration = c("joint", "ordered"), order = NULL) {
  arbitration <- match.arg(arbitration)
  stopifnot(
    "order must be NULL or a vector of integers" = is.null(order) || is_integerish(order)
  )

  new_allocator_spec(
    name = glue::glue("greedy ({arbitration})"),
    args = list(arbitration = arbitration, order = order),
    one_period = function(ctx) alloc_greedy_one_period(ctx, arbitration, order)
  )
}

alloc_greedy_one_period <- function(ctx, arbitration, order) {
  viable_trans <- ctx$viable_trans
  stopifnot(
    "order must list viable id_trans only" = all(order %in% viable_trans[["id_trans"]])
  )

  # a transition can only happen in cells of its anterior class, and only where its adjusted
  # potential is positive
  candidates <-
    ctx$potentials[value > 0, .(id_trans, id_coord, value)][
      viable_trans,
      on = "id_trans",
      nomatch = NULL
    ][
      ctx$anterior,
      on = .(id_coord, id_lulc_anterior = id_lulc),
      nomatch = NULL
    ]

  # The walk accepts candidates first come, first served. joint: one ranking of all pairs by
  # potential, so a contested cell goes to whichever transition is most likely there. ordered:
  # transition by transition in priority order, so earlier transitions win contested cells.
  # Ties are broken by ids: arbitrary, but needed for a result that does not depend on the
  # order the rows were read in (documented as such, see Description).
  if (arbitration == "joint") {
    data.table::setorder(candidates, -value, id_trans, id_coord)
  } else {
    priority <- c(order, setdiff(viable_trans[["id_trans"]], order))
    candidates[, rank_trans := match(id_trans, priority)]
    data.table::setorder(candidates, rank_trans, -value, id_coord)
  }

  accepted <- greedy_fill(candidates, ctx$quota)

  # a transition falls short of its quota when it runs out of candidate cells, e.g. because
  # other transitions took them first
  short <-
    candidates[accepted, .N, by = id_trans][ctx$quota, on = "id_trans"][is.na(N) | N < quota]
  if (nrow(short) > 0L) {
    warning(glue::glue(
      "Not enough candidate cells to meet the demand of id_trans ",
      "{toString(short$id_trans)}; allocated {toString(data.table::fcoalesce(short$N, 0L))} ",
      "of {toString(short$quota)} cells"
    ))
  }

  # the anterior map with the accepted transitions applied
  data.table::copy(ctx$anterior)[
    candidates[accepted, .(id_coord, id_lulc_posterior)],
    on = "id_coord",
    id_lulc := i.id_lulc_posterior
  ]
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
