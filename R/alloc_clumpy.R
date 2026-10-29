#' CLUMPY allocator
#'
#' @description
#' Stochastic, unbiased allocation after Mazy (2022, Ch. 3), reimplemented from
#' [CLUMPY](https://github.com/mmyrte/clumpy). The whole pivot-selection and patch-growth
#' routine runs in C++ ([allocate_clumpy_cpp()]). The method is chosen from the patch
#' parameters in `alloc_params_t`:
#'
#' * **uSAM** (Unbiased Simple Allocation Method, Mazy sec. 3.4.1) when every transition is
#'   mono-pixel (`mean_patch_size == 1` and `patch_size_variance == 0`): one MuST (Multinomial
#'   Sampling Test, Mazy App. 3.B; the same test the reference `clumpy` calls "GART") pass per
#'   anterior class, each selected pivot allocated as a single cell. Quantity of change is
#'   enforced in expectation.
#' * **uPAM** (Unbiased Patch Allocation Method, Mazy sec. 3.4.2, Fig. 3.2) otherwise: iterative
#'   MuST with a per-transition pixel quota and sampling without replacement. Affordable here
#'   because evoland's potentials come from a fixed fitted model, so the marginal density does
#'   not need to be re-estimated between patches.
#'
#' Multi-pixel patches require uPAM; "uSAM with patches larger than one pixel" is not a valid
#' method, hence the automatic selection rather than a user switch.
#'
#' The per-cell pivot probability is divided by the mean patch area (the 1/E(sigma) factor,
#' Mazy Fig. 3.2) so that the allocated quantity of change matches the target transition rate;
#' without it allocation over-shoots by roughly the mean patch size.
#'
#' The patch parameters are read from `alloc_params_t`: `mean_patch_size` and
#' `patch_size_variance` give the patch-area distribution, `patch_elongation` its shape.
#'
#' The allocator works on a raster built from `coords_t`, so it needs coordinate points on a
#' regular grid. R's random number generator drives it, so [set.seed()] makes a realisation
#' replay.
#'
#' @param area_dist Character; patch-area distribution, `"lognormal"` (default) or `"normal"`
#'   (Gaussian with sd = `sqrt(patch_size_variance)`, clamped to >= 1).
#' @param avoid_aggregation Logical; if `TRUE` (default) uPAM patches that would merge with an
#'   existing patch fail and allocate nothing (clumpy `GaussianPatcher` semantics). Ignored for
#'   the mono-pixel uSAM path.
#' @param batch_size Integer; uPAM pivots attempted per MuST re-draw. `0` (default) auto-scales
#'   to ~1% of each class's source pool, bounding the number of MuST passes so large rasters
#'   stay tractable; `> 0` is an explicit cap (1 = strict uPAM); `< 0` processes all candidates
#'   in a single pass. Ignored for uSAM.
#' @param default_elongation Numeric in \[0, 1\]; patch elongation for transitions whose
#'   `patch_elongation` is missing (0 isometric, 1 linear).
#'
#' @return An allocator specification for [allocate()].
#'
#' @references Mazy, 2022 (\url{https://theses.hal.science/tel-04382012v1}), Ch. 3.
#'
#' @include allocate.R
#' @export
alloc_clumpy_spec <- function(
  area_dist = c("lognormal", "normal"),
  avoid_aggregation = TRUE,
  batch_size = 0L,
  default_elongation = 0.5
) {
  area_dist <- match.arg(area_dist)
  stopifnot(
    "avoid_aggregation must be TRUE or FALSE" = is_scalar_logical(avoid_aggregation),
    "batch_size must be a single integer" = is_scalar_integerish(batch_size),
    "default_elongation must be a number in [0, 1]" = {
      is_scalar_numeric(default_elongation) && default_elongation >= 0 && default_elongation <= 1
    }
  )
  args <- list(
    area_dist = area_dist,
    avoid_aggregation = avoid_aggregation,
    batch_size = as.integer(batch_size),
    default_elongation = default_elongation
  )

  new_allocator_spec(
    name = "CLUMPY",
    args = args,
    one_period = function(ctx) alloc_clumpy_one_period(ctx, args)
  )
}

alloc_clumpy_one_period <- function(ctx, args) {
  grid <- ctx$grid
  cells <- grid[["cells"]]
  viable_trans <- ctx$viable_trans

  # the anterior map as a raster in row-major cell order, NA where there is no coordinate point
  landscape <- grid_values(grid, ctx$anterior, "id_lulc")

  # Sparse per-transition potential columns: the cells carrying a nonzero adjusted potential
  # and their values. The adjusted potentials are already sparse, so they are passed as
  # per-transition lists rather than a dense n_cells x n_trans matrix (prohibitive at >1e7
  # cells).
  potentials <- ctx$potentials[value > 0][cells, on = "id_coord", cell := i.cell]
  # one element per viable transition, in their order, empty for a transition without
  # potentials
  by_trans <- factor(potentials[["id_trans"]], levels = viable_trans[["id_trans"]])
  prob_cell <- unname(split(potentials[["cell"]], by_trans))
  prob_value <- unname(split(potentials[["value"]], by_trans))

  # patch parameters and rates in the order of viable_trans
  params <- ctx$alloc_params[viable_trans[, .(id_trans)], on = "id_trans"]
  rates <- ctx$quota[viable_trans[, .(id_trans)], on = "id_trans"]
  area_mean <- as.numeric(params[["mean_patch_size"]])
  area_var <- as.numeric(params[["patch_size_variance"]])
  elongation <- data.table::fcoalesce(
    as.numeric(params[["patch_elongation"]]),
    args[["default_elongation"]]
  )

  # every transition mono-pixel -> uSAM, otherwise uPAM
  is_mono <- all(!is.na(area_mean) & area_mean == 1 & (is.na(area_var) | area_var == 0))
  message(glue::glue("  CLUMPY method: {if (is_mono) 'uSAM' else 'uPAM'}"))

  posterior <- allocate_clumpy_cpp(
    landscape = landscape,
    nrow = terra::nrow(grid[["template"]]),
    ncol = terra::ncol(grid[["template"]]),
    trans_from = viable_trans[["id_lulc_anterior"]],
    trans_to = viable_trans[["id_lulc_posterior"]],
    prob_cell = prob_cell,
    prob_value = prob_value,
    area_mean = area_mean,
    area_var = area_var,
    elongation = elongation,
    target_rate = rates[["rate"]],
    method = if (is_mono) 0L else 1L,
    batch_size = args[["batch_size"]],
    rarefy = TRUE,
    shuffle = TRUE,
    avoid_aggregation = args[["avoid_aggregation"]],
    # integer code used by allocate_clumpy_cpp: 0 = log-normal, 1 = normal
    area_dist = if (args[["area_dist"]] == "lognormal") 0L else 1L
  )

  cells[ctx$anterior[, .(id_coord)], on = "id_coord"][,
    .(id_coord, id_lulc = as.integer(posterior[cell]))
  ]
}
