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
#' 3. **Allocation** – for each anterior land-use class:
#'    a. GART (Generalized Allocation Rejection Test) samples a pivot transition
#'       for each cell.
#'    b. From each pivot cell a patch is grown using [grow_patch_cpp()] with a
#'       log-normal patch-size distribution and eccentricity guidance from
#'       [alloc_params_clumpy_v()].
#'
#' @references Mazy, 2022 (\url{https://theses.hal.science/tel-04382012v1}), Ch. 2.
#'
#' @name alloc_clumpy
#' @include trans_models_t.R alloc_params_t.R alloc_dinamica.R
NULL

# ---------------------------------------------------------------------------
# GART – Generalized Allocation Rejection Test (R translation of _gart.py)
# ---------------------------------------------------------------------------

#' @describeIn alloc_clumpy
#' Vectorised random assignment of final states given a probability matrix.
#'
#' @param P Numeric matrix (n_cells × n_states).  Each row must already include
#'   the "stay" column so that row sums equal 1.
#' @param states Integer vector of length ncol(P) with the state ID for each
#'   column.
#' @return Integer vector of length nrow(P) with the sampled state per cell.
#' @keywords internal
gart <- function(P, states) {
  stopifnot(is.matrix(P), length(states) == ncol(P))
  n <- nrow(P)
  k <- ncol(P)

  # Cumulative sums across columns (row-wise)
  cs <- t(apply(P, 1, cumsum))

  u <- stats::runif(n)

  y <- integer(n)
  # Iterate in REVERSE column order so that later overwrite earlier assignments
  # (mirrors the Python implementation which sets y[u < cs[:, inv_j]] = states[inv_j])
  for (j in k:1L) {
    y[u < cs[, j]] <- states[j]
  }
  y
}

# ---------------------------------------------------------------------------
# Log-normal patch size sampler
# ---------------------------------------------------------------------------

#' @describeIn alloc_clumpy
#' Draw a patch target area from a log-normal distribution.
#'
#' @param area_mean Numeric; mean of the log-normal distribution (cells).
#' @param area_var  Numeric; variance of the log-normal distribution (cells^2).
#' @return Integer >= 1L, sampled patch area in cells.
#' @keywords internal
sample_lognorm_area <- function(area_mean, area_var) {
  if (is.na(area_mean) || area_mean <= 0) return(1L)
  E <- area_mean
  V <- if (is.na(area_var) || area_var <= 0) 1 else area_var
  mu <- log(E^2 / sqrt(V + E^2))
  sigma <- sqrt(log(V / E^2 + 1))
  max(1L, as.integer(round(stats::rlnorm(1L, mu, sigma))))
}

# ---------------------------------------------------------------------------
# Raster neighbor precomputation helpers
# ---------------------------------------------------------------------------

#' @describeIn alloc_clumpy
#' Pre-compute rook-adjacency neighbor index vectors for a raster.
#'
#' Each output vector has one entry per raster cell (row-major, 1-based).
#' A value of 0 means "no neighbor" (edge cell).
#'
#' @param nrow_r Integer; number of raster rows.
#' @param ncol_r Integer; number of raster columns.
#' @return Named list with elements `above`, `below`, `left`, `right`.
#' @keywords internal
raster_neighbors <- function(nrow_r, ncol_r) {
  n <- nrow_r * ncol_r
  rows <- ceiling(seq_len(n) / ncol_r)
  cols <- ((seq_len(n) - 1L) %% ncol_r) + 1L

  list(
    above = ifelse(rows > 1L,      seq_len(n) - ncol_r, 0L),
    below = ifelse(rows < nrow_r,  seq_len(n) + ncol_r, 0L),
    left  = ifelse(cols > 1L,      seq_len(n) - 1L,     0L),
    right = ifelse(cols < ncol_r,  seq_len(n) + 1L,     0L)
  )
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
#' @return An [lulc_data_t] with the simulated posterior LULC.
#' @keywords internal
alloc_clumpy_one_period <- function(
  self,
  id_period_ant,
  id_period_post,
  anterior_rast,
  select_score,
  select_maximize
) {
  message(glue::glue(
    "Running CLUMPY allocation: period {id_period_ant} -> {id_period_post}"
  ))

  # 1. Predict and store raw transition potentials
  self$predict_trans_pot(
    id_period_post = id_period_post,
    select_score = select_score,
    select_maximize = select_maximize
  )

  # 2. Retrieve adjusted potentials
  adj_pots <- self$adjusted_trans_pot_v(id_period_post)

  # 3. CLUMPY allocation parameters
  clumpy_params <- self$alloc_params_clumpy_v()

  # 4. Viable transitions
  viable_trans <- self$trans_meta_t[is_viable == TRUE]

  # 5. Prepare raster representation
  nrow_r <- terra::nrow(anterior_rast)
  ncol_r <- terra::ncol(anterior_rast)
  n_cells <- nrow_r * ncol_r

  ant_vec <- as.integer(terra::values(anterior_rast))
  post_vec <- ant_vec  # will be modified in-place

  neighbors <- raster_neighbors(nrow_r, ncol_r)

  # 6. Build id_coord -> raster cell (1-based, row-major) mapping
  coords_minimal <- self$coords_minimal
  xy_mat <- as.matrix(coords_minimal[, .(lon, lat)])
  cell_idx <- terra::cellFromXY(anterior_rast, xy_mat)

  coord_to_cell <- stats::setNames(cell_idx, coords_minimal$id_coord)

  # 7. For each anterior LULC class, run GART + patch growth
  from_classes <- sort(unique(viable_trans[["id_lulc_anterior"]]))

  for (from_class in from_classes) {
    trans_for_class <- viable_trans[id_lulc_anterior == from_class]
    to_classes <- trans_for_class[["id_lulc_posterior"]]

    # Cells currently in from_class (1-based raster index)
    from_cells <- which(!is.na(ant_vec) & ant_vec == from_class)
    if (length(from_cells) == 0L) next

    # Build probability matrix: rows = from_cells, cols = to_classes
    # Probability values come from adjusted_trans_pot_v, keyed by id_coord
    P_change <- matrix(0.0, nrow = length(from_cells), ncol = length(to_classes))

    # Reverse-map raster cells to id_coord
    cell_to_coord <- stats::setNames(coords_minimal$id_coord, coord_to_cell)

    for (j in seq_along(to_classes)) {
      id_trans_j <- trans_for_class$id_trans[j]
      pots_j <- adj_pots[id_trans == id_trans_j, .(id_coord, value)]
      if (nrow(pots_j) == 0L) next

      # Map from_cells -> id_coord -> value
      id_coord_j <- as.integer(cell_to_coord[as.character(from_cells)])
      matched <- pots_j[data.table::data.table(id_coord = id_coord_j), on = "id_coord"]
      P_change[, j] <- ifelse(is.na(matched$value), 0.0, matched$value)
    }

    # Add "stay" column so rows sum to 1
    row_sums <- rowSums(P_change)
    stay_prob <- pmax(0.0, 1.0 - row_sums)
    P_full <- cbind(P_change, stay_prob)
    states_full <- c(to_classes, from_class)

    # GART: sample a final state for each cell
    sampled_states <- gart(P_full, states_full)

    # 8. For each to_class, grow patches from pivot cells
    for (j in seq_along(to_classes)) {
      to_class <- to_classes[j]
      id_trans_j <- trans_for_class$id_trans[j]

      pivot_cells <- from_cells[sampled_states == to_class]
      if (length(pivot_cells) == 0L) next

      # Shuffle pivots for unbiased ordering
      pivot_cells <- sample(pivot_cells)

      # Per-transition probability vector over the full raster (for patch grower)
      pots_full_j <- adj_pots[id_trans == id_trans_j, .(id_coord, value)]
      prob_vec <- rep(0.0, n_cells)
      if (nrow(pots_full_j) > 0L) {
        cell_idx_j <- as.integer(coord_to_cell[as.character(pots_full_j$id_coord)])
        valid <- !is.na(cell_idx_j) & cell_idx_j >= 1L & cell_idx_j <= n_cells
        prob_vec[cell_idx_j[valid]] <- pots_full_j$value[valid]
      }

      # Patch parameters
      params_j <- clumpy_params[id_trans == id_trans_j]
      area_mean <- if (nrow(params_j) > 0L) params_j$area_mean[1L] else NA_real_
      area_var  <- if (nrow(params_j) > 0L) params_j$area_var[1L]  else NA_real_
      eccentricity_target       <- if (nrow(params_j) > 0L && !is.na(params_j$eccentricity[1L])) {
        params_j$eccentricity[1L]
      } else {
        0.5
      }

      for (pivot in pivot_cells) {
        if (is.na(post_vec[pivot]) || post_vec[pivot] != from_class) next

        target_area <- sample_lognorm_area(area_mean, area_var)

        patch_cells <- grow_patch_cpp(
          landscape    = post_vec,
          ant_landscape = ant_vec,
          probs        = prob_vec,
          nbr_above    = neighbors$above,
          nbr_below    = neighbors$below,
          nbr_left     = neighbors$left,
          nbr_right    = neighbors$right,
          pivot        = pivot,
          target_area  = target_area,
          from_class   = from_class,
          to_class     = to_class,
          eccentricity = eccentricity_target,
          ncol         = ncol_r
        )

        # grow_patch_cpp modifies landscape in-place (the IntegerVector is
        # passed by reference in Rcpp).  Sync post_vec accordingly.
        if (length(patch_cells) > 0L) {
          post_vec[patch_cells] <- to_class
        }
      }
    }
  }

  # 9. Convert result vector back to lulc_data_t
  message("  Converting posterior vector to lulc_data_t...")

  # Map raster cells back to id_coord
  coord_ids <- as.integer(names(coord_to_cell))
  cell_ids  <- as.integer(coord_to_cell)

  valid <- !is.na(cell_ids) & cell_ids >= 1L & cell_ids <= n_cells &
    !is.na(post_vec[cell_ids])

  lulc_result <- data.table::data.table(
    id_run   = self$id_run,
    id_coord = coord_ids[valid],
    id_lulc  = as.integer(post_vec[cell_ids[valid]]),
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
#' @param seed Optional integer random seed for reproducibility.
alloc_clumpy <- function(
  self,
  id_periods,
  select_score,
  select_maximize,
  seed = NULL
) {
  stopifnot(
    "id_periods must be a numeric vector" = is.numeric(id_periods),
    "id_periods must be contiguous" = all(diff(id_periods) == 1L),
    "id_run must be set" = !is.null(self$id_run)
  )

  available_periods <- self$periods_t$id_period
  missing_periods <- setdiff(id_periods, available_periods)
  if (length(missing_periods) > 0L) {
    stop(glue::glue(
      "Periods not found in periods_t: {paste(missing_periods, collapse = ', ')}"
    ))
  }

  if (!is.null(seed)) set.seed(seed)

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
      self           = self,
      id_period_ant  = id_period_ant,
      id_period_post = id_period_post,
      anterior_rast  = current_rast,
      select_score   = select_score,
      select_maximize = select_maximize
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
