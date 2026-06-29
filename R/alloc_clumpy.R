#' CLUMPY-style Allocation Methods
#'
#' @description
#' Methods for running CLUMPY-style LULC allocation.  The algorithm works in
#' three stages per period:
#'
#' 1. **Prediction** – raw transition potentials are predicted and stored in
#'    `trans_pot_t` via [predict_trans_pot()].
#' 2. **Adjustment** – the adjusted view [adjusted_trans_pot_v()] rescales
#'    potentials to match target rates and closes rows to \[0, 1\].
#' 3. **Allocation** – the whole pivot-selection + patch-growth routine runs in
#'    C++ ([allocate_clumpy_cpp()]).  The method is chosen automatically from the
#'    patch parameters:
#'    * **uSAM** (Unbiased Simple Allocation Method, Mazy sec. 3.4.1) when every
#'      transition is mono-pixel (`area_mean == 1` and `area_var == 0`): one MuST
#'      (Multinomial Sampling Test, Mazy App. 3.B; the same test the reference
#'      `clumpy` calls "GART") pass per anterior class, each selected pivot
#'      allocated as a single cell.  Quantity of change is enforced in
#'      expectation.
#'    * **uPAM** (Unbiased Patch Allocation Method, Mazy sec. 3.4.2, Fig. 3.2)
#'      otherwise: iterative MuST with a per-transition pixel quota and sampling
#'      without replacement.  Affordable here because evoland's potentials come
#'      from a fixed fitted model, so the marginal density does not need to be
#'      re-estimated between patches.
#'
#'    (Multi-pixel patches require uPAM; "uSAM with patches larger than one
#'    pixel" is not a valid method, hence the automatic selection rather than a
#'    user switch.)
#'
#'    The per-cell pivot probability is divided by the mean patch area (the
#'    1/E(sigma) factor, Mazy Fig. 3.2) so the allocated quantity of change
#'    matches the target transition rate; without it allocation over-shoots by
#'    roughly the mean patch size.
#'
#' @references Mazy, 2022 (\url{https://theses.hal.science/tel-04382012v1}), Ch. 3.
#'
#' @name alloc_clumpy
#' @include trans_models_t.R alloc_params_t.R alloc_dinamica.R
NULL

# Map the patch-area distribution name to the integer code used by
# allocate_clumpy_cpp (0 = log-normal, 1 = normal).
.clumpy_area_dist_code <- function(area_dist) {
  area_dist <- match.arg(area_dist, c("lognormal", "normal"))
  if (area_dist == "lognormal") 0L else 1L
}

# ---------------------------------------------------------------------------
# Single-period CLUMPY allocation
# ---------------------------------------------------------------------------

#' @describeIn alloc_clumpy
#' Allocate LULC changes for a single period using the CLUMPY algorithm.
#'
#' @param self An [evoland_db] instance.
#' @param id_period_ant Integer anterior period ID.
#' @param id_period_post Integer posterior period ID.
#' @param anterior_rast [terra::SpatRaster] of the anterior LULC state.
#' @param select_score Character; mlr3 measure ID for model selection.
#' @param select_maximize Logical; whether to maximise `select_score`.
#' @param area_dist Character; patch-area distribution, `"lognormal"` (default)
#'   or `"normal"` (Gaussian with sd = `sqrt(area_var)`, clamped to >= 1).
#' @param avoid_aggregation Logical; if `TRUE` (default) uPAM patches that would
#'   merge with an existing patch fail and allocate nothing (clumpy
#'   `GaussianPatcher` semantics).  Ignored for the mono-pixel uSAM path.
#' @param batch_size Integer; uPAM pivots attempted per MuST re-draw. `0`
#'   (default) auto-scales to ~1% of each class's source pool, bounding the
#'   number of MuST passes so large rasters stay tractable; `> 0` is an explicit
#'   cap (1 = strict uPAM); `< 0` processes all candidates in a single pass.
#' @return An [lulc_data_t] with the simulated posterior LULC.
#' @keywords internal
alloc_clumpy_one_period <- function(
  self,
  id_period_ant,
  id_period_post,
  anterior_rast,
  select_score,
  select_maximize,
  area_dist = "lognormal",
  avoid_aggregation = TRUE,
  batch_size = 0L
) {
  area_dist_code <- .clumpy_area_dist_code(area_dist)

  # 1. Predict and store raw transition potentials
  self$predict_trans_pot(
    id_period_post = id_period_post,
    select_score = select_score,
    select_maximize = select_maximize
  )

  # 2. Retrieve adjusted potentials, patch params and target rates
  adj_pots <- self$adjusted_trans_pot_v(id_period_post)
  clumpy_params <- self$alloc_params_clumpy_v()
  rates <- self$trans_rates_t[
    id_period == id_period_post,
    .(id_trans, rate)
  ]

  # 3. Viable transitions (stable order: by anterior class, then transition id)
  viable_trans <- self$trans_meta_t[is_viable == TRUE]
  data.table::setorder(viable_trans, id_lulc_anterior, id_trans)
  n_trans <- nrow(viable_trans)
  if (n_trans == 0L) {
    stop("No viable transitions found in trans_meta_t")
  }

  # 4. Raster representation (row-major, 1-based cell indices)
  nrow_r <- terra::nrow(anterior_rast)
  ncol_r <- terra::ncol(anterior_rast)
  n_cells <- nrow_r * ncol_r
  ant_vec <- as.integer(terra::values(anterior_rast))

  # 5. id_coord <-> raster cell mapping
  coords_minimal <- self$coords_minimal
  xy_mat <- as.matrix(coords_minimal[, .(lon, lat)])
  cell_idx <- terra::cellFromXY(anterior_rast, xy_mat)
  coord_to_cell <- stats::setNames(cell_idx, coords_minimal$id_coord)

  # 6. Sparse per-transition potential columns: for each transition, the cells
  #    (1-based raster index) carrying a nonzero adjusted potential and the
  #    matching values. The adjusted-potential table is already sparse, so we
  #    pass it as per-transition lists and avoid materialising a dense
  #    n_cells x n_trans matrix (prohibitive at >1e7 cells).
  prob_cell <- vector("list", n_trans)
  prob_value <- vector("list", n_trans)
  for (t in seq_len(n_trans)) {
    id_trans_t <- viable_trans$id_trans[t]
    pots_t <- adj_pots[id_trans == id_trans_t, .(id_coord, value)]
    if (nrow(pots_t) == 0L) {
      prob_cell[[t]] <- integer(0)
      prob_value[[t]] <- numeric(0)
      next
    }
    cells_t <- as.integer(coord_to_cell[as.character(pots_t$id_coord)])
    ok <- !is.na(cells_t) & cells_t >= 1L & cells_t <= n_cells
    prob_cell[[t]] <- cells_t[ok]
    prob_value[[t]] <- as.numeric(pots_t$value[ok])
  }

  # 7. Align patch params / rates to the viable-transition order
  key <- data.table::data.table(id_trans = viable_trans$id_trans)
  params_aligned <- clumpy_params[key, on = "id_trans"]
  rates_aligned <- rates[key, on = "id_trans"]

  area_mean <- as.numeric(params_aligned$area_mean)
  area_var <- as.numeric(params_aligned$area_var)
  elongation <- as.numeric(params_aligned$elongation)
  elongation[is.na(elongation)] <- 0.5
  target_rate <- as.numeric(rates_aligned$rate)
  target_rate[is.na(target_rate)] <- 0.0

  # 8. Select the method from the patch parameters: every transition mono-pixel
  #    (area_mean == 1 & area_var == 0) -> uSAM, otherwise uPAM.
  is_mono <- all(!is.na(area_mean) & area_mean == 1 &
    (is.na(area_var) | area_var == 0))
  method_code <- if (is_mono) 0L else 1L
  method_name <- if (is_mono) "uSAM" else "uPAM"

  message(glue::glue(
    "Running CLUMPY allocation ({method_name}): ",
    "period {id_period_ant} -> {id_period_post}"
  ))

  # 9. Run the full allocation routine in C++
  post_vec <- allocate_clumpy_cpp(
    landscape = ant_vec,
    nrow = nrow_r,
    ncol = ncol_r,
    trans_from = as.integer(viable_trans$id_lulc_anterior),
    trans_to = as.integer(viable_trans$id_lulc_posterior),
    prob_cell = prob_cell,
    prob_value = prob_value,
    area_mean = area_mean,
    area_var = area_var,
    elongation = elongation,
    target_rate = target_rate,
    method = method_code,
    batch_size = as.integer(batch_size),
    rarefy = TRUE,
    shuffle = TRUE,
    avoid_aggregation = avoid_aggregation,
    area_dist = area_dist_code
  )

  # 10. Convert result vector back to lulc_data_t
  message("  Converting posterior vector to lulc_data_t...")
  coord_ids <- as.integer(names(coord_to_cell))
  cell_ids <- as.integer(coord_to_cell)
  valid <- !is.na(cell_ids) &
    cell_ids >= 1L &
    cell_ids <= n_cells &
    !is.na(post_vec[cell_ids])

  lulc_result <- data.table::data.table(
    id_run = self$id_run,
    id_coord = coord_ids[valid],
    id_lulc = as.integer(post_vec[cell_ids[valid]]),
    id_period = id_period_post
  ) |>
    as_lulc_data_t()

  message(glue::glue("  Allocated {nrow(lulc_result)} cells"))

  lulc_result
}

# ---------------------------------------------------------------------------
# Multi-period orchestration
# ---------------------------------------------------------------------------

#' @describeIn alloc_clumpy
#' Run CLUMPY-style allocation over multiple periods.
#'
#' @param self An [evoland_db] instance.
#' @param id_periods Integer vector of posterior period IDs to simulate.
#' @param select_score Character; mlr3 measure ID for model selection.
#' @param select_maximize Logical; whether to maximise `select_score`.
#' @param area_dist Character; patch-area distribution, `"lognormal"` (default)
#'   or `"normal"`.
#' @param avoid_aggregation Logical; uPAM merge avoidance (default `TRUE`).
#' @param batch_size Integer; uPAM pivots attempted per MuST re-draw. `0`
#'   (default) auto-scales with the source pool; see [alloc_clumpy_one_period()].
#' @param seed Optional integer random seed for reproducibility.
alloc_clumpy <- function(
  self,
  id_periods,
  select_score,
  select_maximize,
  area_dist = "lognormal",
  avoid_aggregation = TRUE,
  batch_size = 0L,
  seed = NULL
) {
  stopifnot(
    "id_periods must be a numeric vector" = is.numeric(id_periods),
    "id_periods must be contiguous" = all(diff(id_periods) == 1L),
    "id_run must be set" = !is.null(self$id_run)
  )
  area_dist <- match.arg(area_dist, c("lognormal", "normal"))

  available_periods <- self$periods_t$id_period
  missing_periods <- setdiff(id_periods, available_periods)
  if (length(missing_periods) > 0L) {
    stop(glue::glue(
      "Periods not found in periods_t: {paste(missing_periods, collapse = ', ')}"
    ))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  message(glue::glue(
    "Starting CLUMPY allocation simulation\n",
    "  Periods: {paste(id_periods, collapse = ' -> ')}\n",
    "  Run: {self$id_run}"
  ))

  current_rast <- self$lulc_data_as_rast(id_period = id_periods[1L] - 1L)

  i <- 1L
  for (id_period_post in id_periods) {
    id_period_ant <- id_period_post - 1L

    message(glue::glue("\n=== Iteration {i}/{length(id_periods)} ==="))

    lulc_result <- alloc_clumpy_one_period(
      self = self,
      id_period_ant = id_period_ant,
      id_period_post = id_period_post,
      anterior_rast = current_rast,
      select_score = select_score,
      select_maximize = select_maximize,
      area_dist = area_dist,
      avoid_aggregation = avoid_aggregation,
      batch_size = batch_size
    )

    self$commit(lulc_result, "lulc_data_t", method = "upsert")
    self$upsert_new_neighbors(id_period_post)
    current_rast <- self$lulc_data_as_rast(id_period = id_period_post)

    message(glue::glue("Iteration {i} complete"))
    i <- i + 1L
  }

  message("CLUMPY allocation complete!")
  invisible(NULL)
}
