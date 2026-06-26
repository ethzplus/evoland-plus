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
#'    C++ ([allocate_clumpy_cpp()]).  Two methods are available (see `method`):
#'    * **uSAM** (Unbiased Simple Allocation Method, Mazy sec. 3.4.1): one GART
#'      (Generalized Allocation Rejection Test) pass per anterior class; every
#'      cell that draws a change becomes a patch pivot.  Quantity of change is
#'      enforced only in expectation.  Cheapest.
#'    * **uPAM** (Unbiased Patch Allocation Method, Mazy sec. 3.4.2, Fig. 3.2):
#'      iterative GART with a per-transition pixel quota and sampling without
#'      replacement.  `batch_size` trades speed for fidelity (1 = strict uPAM,
#'      one pivot per GART draw).  Affordable here because evoland's potentials
#'      come from a fixed fitted model, so the marginal density does not need to
#'      be re-estimated between patches.
#'
#'    In both methods the per-cell pivot probability is divided by the mean patch
#'    area (the 1/E(sigma) factor, Mazy Fig. 3.2) so the allocated quantity of
#'    change matches the target transition rate; without it allocation
#'    over-shoots by roughly the mean patch size.
#'
#' @references Mazy, 2022 (\url{https://theses.hal.science/tel-04382012v1}), Ch. 3.
#'
#' @name alloc_clumpy
#' @include trans_models_t.R alloc_params_t.R alloc_dinamica.R
NULL

# Map a user-facing method name to the integer code used by allocate_clumpy_cpp.
.clumpy_method_code <- function(method) {
  method <- match.arg(method, c("usam", "upam"))
  if (method == "usam") 0L else 1L
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
#' @param method Character; `"usam"` (single pass) or `"upam"` (iterative).
#' @param batch_size Integer; uPAM pivots processed per GART re-draw
#'   (1 = strict uPAM; `<= 0` = all candidates per pass).
#' @return An [lulc_data_t] with the simulated posterior LULC.
#' @keywords internal
alloc_clumpy_one_period <- function(
  self,
  id_period_ant,
  id_period_post,
  anterior_rast,
  select_score,
  select_maximize,
  method = "usam",
  batch_size = 1L
) {
  method_code <- .clumpy_method_code(method)

  message(glue::glue(
    "Running CLUMPY allocation ({method}): ",
    "period {id_period_ant} -> {id_period_post}"
  ))

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

  # 6. Per-transition probability columns (n_cells x n_trans), 0 where absent
  probs_mat <- matrix(0.0, nrow = n_cells, ncol = n_trans)
  for (t in seq_len(n_trans)) {
    id_trans_t <- viable_trans$id_trans[t]
    pots_t <- adj_pots[id_trans == id_trans_t, .(id_coord, value)]
    if (nrow(pots_t) == 0L) {
      next
    }
    cells_t <- as.integer(coord_to_cell[as.character(pots_t$id_coord)])
    ok <- !is.na(cells_t) & cells_t >= 1L & cells_t <= n_cells
    probs_mat[cbind(cells_t[ok], t)] <- pots_t$value[ok]
  }

  # 7. Align patch params / rates to the viable-transition order
  key <- data.table::data.table(id_trans = viable_trans$id_trans)
  params_aligned <- clumpy_params[key, on = "id_trans"]
  rates_aligned <- rates[key, on = "id_trans"]

  area_mean <- as.numeric(params_aligned$area_mean)
  area_var <- as.numeric(params_aligned$area_var)
  elongation <- as.numeric(params_aligned$eccentricity) # patch_elongation alias
  elongation[is.na(elongation)] <- 0.5
  target_rate <- as.numeric(rates_aligned$rate)
  target_rate[is.na(target_rate)] <- 0.0

  from_classes <- sort(unique(as.integer(viable_trans$id_lulc_anterior)))

  # 8. Run the full allocation routine in C++
  post_vec <- allocate_clumpy_cpp(
    landscape = ant_vec,
    ant_landscape = ant_vec,
    nrow = nrow_r,
    ncol = ncol_r,
    from_classes = from_classes,
    trans_from = as.integer(viable_trans$id_lulc_anterior),
    trans_to = as.integer(viable_trans$id_lulc_posterior),
    probs = probs_mat,
    area_mean = area_mean,
    area_var = area_var,
    elongation = elongation,
    target_rate = target_rate,
    method = method_code,
    batch_size = as.integer(batch_size),
    rarefy = TRUE,
    shuffle = TRUE
  )

  # 9. Convert result vector back to lulc_data_t
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
#' @param method Character; `"usam"` (single pass) or `"upam"` (iterative).
#' @param batch_size Integer; uPAM pivots processed per GART re-draw.
#' @param seed Optional integer random seed for reproducibility.
alloc_clumpy <- function(
  self,
  id_periods,
  select_score,
  select_maximize,
  method = "usam",
  batch_size = 1L,
  seed = NULL
) {
  stopifnot(
    "id_periods must be a numeric vector" = is.numeric(id_periods),
    "id_periods must be contiguous" = all(diff(id_periods) == 1L),
    "id_run must be set" = !is.null(self$id_run)
  )
  method <- match.arg(method, c("usam", "upam"))

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
    "  Method: {method} (batch_size = {batch_size})\n",
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
      method = method,
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
