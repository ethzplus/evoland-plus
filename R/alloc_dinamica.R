#' Dinamica EGO Allocation Methods
#'
#' @description
#' Methods for running Dinamica EGO allocation and evaluating allocation parameters, with two
#' entry points for allocation:
#' * `db$alloc_dinamica(id_periods, ...)` runs a whole sequence of periods. After each period
#'   it commits the allocated map to `lulc_data_t` and recomputes the neighbour predictors the
#'   next period is predicted from. Use it for plain simulations.
#' * `alloc_dinamica_one_period(db, id_period_post, ...)` allocates one period for the active
#'   `id_run` and returns the map **without committing it**. Use it to write your own loop:
#'   edit `trans_pot_t` between prediction and allocation (e.g. interventions), edit the
#'   allocated map afterwards, or draw an ensemble of single-period realisations. The caller
#'   then commits the map with `db$commit(x, "lulc_data_t", method = "upsert")` and, if a
#'   later period follows, runs `db$upsert_new_neighbors(id_period_post)`, in that order.
#'
#' Both reuse transition potentials already in `trans_pot_t` for the run and period, and only
#' predict them when none exist (or when `force_predict_trans_pot = TRUE`). They mirror
#' [alloc_clumpy_one_period()] and `db$alloc_clumpy()`.
#'
#' `eval_alloc_params_t()` runs `alloc_dinamica()` for every run in `alloc_params_t` over the
#' observed periods and scores each against the observation with fuzzy similarity.
#'
#' @return `alloc_dinamica_one_period()`: an [lulc_data_t] with the simulated posterior LULC.
#'   `alloc_dinamica()`: called for its side effects on `lulc_data_t` and `pred_data_t`.
#'   `eval_alloc_params_t()`: the `alloc_params_t` rows, with `similarity` filled in.
#'
#' @name alloc_dinamica
#' @include trans_models_t.R alloc_params_t.R
NULL

# Write the input files for a single Dinamica allocation iteration into work_dir: rates,
# expansion and patcher tables, anterior map and one probability map per viable transition.
# Returns a list of the paths written.
alloc_dinamica_setup_inputs <- function(db, id_period_post, anterior_rast, work_dir) {
  # Get metadata
  coords_meta <- db$get_table_metadata("coords_t")
  epsg <- coords_meta[["epsg"]]

  # Get viable transitions
  viable_trans <- db$trans_meta_t[is_viable == TRUE]

  stopifnot(
    "No viable transitions found" = nrow(viable_trans) > 0L,
    "coords_t must have epsg metadata" = !is.null(epsg)
  )

  # Sort transitions consistently (by id_lulc_anterior, then id_lulc_posterior)
  data.table::setorder(viable_trans, id_lulc_anterior, id_lulc_posterior)

  # 1. Write transition rates
  trans_rates <- db$trans_rates_dinamica_v(id_period_post)

  # Ensure same sort order as viable_trans
  data.table::setorder(trans_rates, `From*`, `To*`)

  trans_rates_path <- file.path(work_dir, "trans_rates.csv")
  data.table::fwrite(trans_rates, trans_rates_path)

  message(glue::glue("  Wrote transition rates to {basename(trans_rates_path)}"))

  # 2. Get allocation parameters for this run
  alloc_params_full <-
    db$alloc_params_t |>
    merge(
      viable_trans[, .(id_trans, id_lulc_anterior, id_lulc_posterior)],
      by = "id_trans"
    )

  # Sort to match transition order
  data.table::setorder(alloc_params_full, id_lulc_anterior, id_lulc_posterior)

  # 3. Write expansion table
  expansion_table <- alloc_params_full[, .(
    `From*` = id_lulc_anterior,
    `To*` = id_lulc_posterior,
    # dinamica alloc cannot do full expansion or patching, hence constrain to (0, 1)
    Frac_expander = pmax(1e-6, pmin(1 - 1e-6, frac_expander))
  )]

  expansion_path <- file.path(work_dir, "expansion_table.csv")
  data.table::fwrite(expansion_table, expansion_path)

  message(glue::glue("  Wrote expansion table to {basename(expansion_path)}"))

  # 4. Write patcher table
  patcher_table <- alloc_params_full[, .(
    `From*` = id_lulc_anterior,
    `To*` = id_lulc_posterior,
    Mean_Patch_Size = ifelse(is.na(mean_patch_size), 1, mean_patch_size),
    Patch_Size_Variance = ifelse(is.na(patch_size_variance), 0, patch_size_variance),
    Patch_Isometry = ifelse(is.na(patch_isometry), 1, patch_isometry)
  )]

  patcher_path <- file.path(work_dir, "patcher_table.csv")
  data.table::fwrite(patcher_table, patcher_path)

  message(glue::glue("  Wrote patcher table to {basename(patcher_path)}"))

  # 5. Write anterior.tif
  anterior_path <- file.path(work_dir, "anterior.tif")
  terra::writeRaster(
    anterior_rast,
    anterior_path,
    overwrite = TRUE,
    datatype = "INT1U",
    NAflag = 255 # because dinamica cannot handle nan
  )

  message(glue::glue("  Wrote anterior LULC to {basename(anterior_path)}"))

  # 6. Generate probability maps from the adjusted transition potentials
  prob_map_dir <-
    file.path(work_dir, "probability_map_dir") |>
    ensure_dir()

  message("  Writing probability maps...")
  coords_minimal <- db$coords_minimal

  adj_trans_pots <- db$adjusted_trans_pot_v(id_period_post)

  # Iterate over viable transitions and write probability maps
  for (i in seq_len(nrow(viable_trans))) {
    id_trans_sel <- viable_trans$id_trans[i]
    prob_spatial <- coords_minimal[
      adj_trans_pots[id_trans == id_trans_sel],
      .(lon, lat, value),
      on = "id_coord"
    ]

    # prefixing with 001, 002... so these files are sorted the same as the transition,
    # expansion, and patcher tables on all sorts of filesystems
    prob_path <- file.path(
      prob_map_dir,
      glue::glue("{sprintf('%03d', i)}_id_trans_{id_trans_sel}.tif")
    )

    terra::rasterize(
      x = prob_spatial[, .(lon, lat)],
      y = anterior_rast,
      values = prob_spatial[["value"]],
      fun = "first"
    ) |>
      terra::writeRaster(
        filename = prob_path,
        overwrite = TRUE,
        NAflag = -999 # because dinamica cannot handle nan
      )
  }

  list(
    trans_rates_path = trans_rates_path,
    expansion_path = expansion_path,
    patcher_path = patcher_path,
    anterior_path = anterior_path,
    prob_map_dir = prob_map_dir
  )
}

#' @describeIn alloc_dinamica Allocate a single period and return the map without
#' committing it; see Description for when to use which.
#'
#' @param db An [evoland_db] instance; uses its active `id_run`.
#' @param id_period_post Integer posterior period ID.
#' @param select_score Character; mlr3 measure ID for model selection.
#' @param select_maximize Logical; whether to maximise `select_score`.
#' @param work_dir Character or NULL; directory for Dinamica's input and output files. If
#'   `NULL` (default), a temporary directory is used and removed afterwards; a directory
#'   passed explicitly is kept.
#' @param use_parent_trans_pot Logical; if TRUE, predict (or reuse) transition potentials
#'   under the parent run, so sibling runs share one set, e.g. for Monte-Carlo ensembles.
#' @param force_predict_trans_pot Logical; if TRUE, recompute transition potentials even if
#'   `trans_pot_t` already holds them for this run and period.
#' @export
alloc_dinamica_one_period <- function(
  db,
  id_period_post,
  select_score,
  select_maximize,
  work_dir = NULL,
  use_parent_trans_pot = FALSE,
  force_predict_trans_pot = FALSE
) {
  id_period_ant <- id_period_post - 1L
  if (is.null(work_dir)) {
    work_dir <- tempfile("dinamica_")
    on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  }
  ensure_dir(work_dir)

  message(glue::glue(
    "Running Dinamica allocation: period {id_period_ant} -> {id_period_post}"
  ))

  predict_trans_pot_for_alloc(
    db = db,
    id_period_post = id_period_post,
    select_score = select_score,
    select_maximize = select_maximize,
    use_parent_trans_pot = use_parent_trans_pot,
    force = force_predict_trans_pot
  )

  anterior_rast <- db$lulc_data_as_rast(id_period = id_period_ant)
  alloc_dinamica_setup_inputs(
    db = db,
    id_period_post = id_period_post,
    anterior_rast = anterior_rast,
    work_dir = work_dir
  )

  gc() # just in case

  message("  Executing Dinamica EGO...")
  run_alloc_dinamica(
    work_dir = work_dir,
    echo = FALSE,
    write_logfile = TRUE
  )

  message("  Converting posterior raster to lulc_data_t...")
  posterior_rast <- terra::rast(file.path(work_dir, "posterior.tif"))
  coords_t <- db$coords_t
  terra::crs(posterior_rast) <- paste0("epsg:", attr(coords_t, "epsg"))
  extracted <- extract_using_coords_t(posterior_rast, coords_t, na_omit = TRUE)

  lulc_result <-
    data.table::data.table(
      id_run = db$id_run,
      id_coord = extracted$id_coord,
      id_lulc = as.integer(extracted$value),
      id_period = id_period_post
    ) |>
    as_lulc_data_t()

  message(glue::glue("  Extracted {nrow(lulc_result)} cells"))

  lulc_result
}

#' @describeIn alloc_dinamica Allocate a contiguous sequence of periods, committing each one
#' and recomputing neighbour predictors in between; the method behind `db$alloc_dinamica()`.
#' @param self An [evoland_db] instance.
#' @param id_periods Integer vector of contiguous posterior period IDs to simulate; data from
#'   the period before the first is used as the anterior state.
#' @param keep_intermediate Logical, whether to keep intermediate files from simulations
alloc_dinamica <- function(
  self,
  id_periods,
  select_score,
  select_maximize,
  work_dir = NULL,
  keep_intermediate = FALSE,
  use_parent_trans_pot = FALSE,
  force_predict_trans_pot = FALSE
) {
  stopifnot(
    "id_periods must be a numeric vector" = is.numeric(id_periods),
    "id_periods must be contiguous" = all(diff(id_periods) == 1L),
    "id_run must be set" = !is.null(self$id_run),
    "id_periods must be in periods_t" = all(id_periods %in% self$periods_t$id_period)
  )

  base_work_dir <-
    file.path(
      work_dir %||% tempfile("dinamica_"),
      sprintf("run_%s", self$id_run)
    ) |>
    ensure_dir()

  message(glue::glue(
    "Starting Dinamica allocation simulation\n",
    "  Periods: {paste(id_periods, collapse = ' -> ')}\n",
    "  Run: {self$id_run}\n",
    "  Work directory: {base_work_dir}"
  ))

  for (id_period_post in id_periods) {
    i <- which(id_period_post == id_periods)
    message(glue::glue("\n=== Period {i}/{length(id_periods)} ==="))

    lulc_result <- alloc_dinamica_one_period(
      db = self,
      id_period_post = id_period_post,
      select_score = select_score,
      select_maximize = select_maximize,
      work_dir = file.path(
        base_work_dir,
        glue::glue("iteration_{i}_period_{id_period_post - 1L}_to_{id_period_post}")
      ),
      use_parent_trans_pot = use_parent_trans_pot,
      force_predict_trans_pot = force_predict_trans_pot
    )

    self$commit(lulc_result, "lulc_data_t", method = "upsert")
    self$upsert_new_neighbors(id_period_post)
  }

  message("Dinamica allocation complete!")

  if (!keep_intermediate) {
    unlink(base_work_dir, recursive = TRUE)
  } else {
    message(glue::glue("Intermediate files retained in: {base_work_dir}"))
  }

  invisible(NULL)
}

#' @describeIn alloc_dinamica Evaluate allocation parameters using fuzzy
#' similarity over different runs.
eval_alloc_params_t <- function(
  self,
  select_score,
  select_maximize,
  work_dir = NULL,
  keep_intermediate = FALSE
) {
  # Get historical periods
  historical_periods <- self$periods_t[is_extrapolated == FALSE & id_period > 0]
  # exclude first period since it has no anterior period
  posterior_historical_periods <- historical_periods$id_period[-1]
  orig_id_run <- self$id_run
  on.exit(self$id_run <- orig_id_run, add = TRUE)

  # Get runs to evaluate
  self$id_run <- NULL
  runs_defined <- self$runs_t[, id_run]
  unfiltered_alloc_params_t <- self$alloc_params_t
  runs_required <- unique(unfiltered_alloc_params_t[, id_run])
  self$id_run <- orig_id_run

  stopifnot(
    "select_score must be a single string" = {
      is.character(select_score) && length(select_score) == 1L
    },
    "select_maximize must be TRUE or FALSE" = (select_maximize || !select_maximize),
    "need at least 2 historical periods for evaluation" = {
      length(posterior_historical_periods) >= 1L
    },
    "all runs in alloc_params_t must be defined in runs_t" = {
      all(runs_required %in% runs_defined)
    }
    # TODO do we want to check that no data exists for runs_required?
  )

  # Get initial and final periods
  id_period_initial <- min(historical_periods$id_period)
  id_period_final <- max(historical_periods$id_period)

  message(glue::glue(
    "Evaluating allocation parameters with fuzzy similarity\n",
    "  Runs: {paste(runs_required, collapse = ', ')}\n",
    "  Initial period: {id_period_initial}\n",
    "  Final period: {id_period_final}"
  ))

  # Get observed data for initial and final periods
  rast_initial <- self$lulc_data_as_rast(id_period = id_period_initial)
  rast_obs_final <- self$lulc_data_as_rast(id_period = id_period_final)

  viable_trans <- self$trans_meta_t[is_viable == TRUE]

  # Storage for per-transition similarity results
  all_similarity_results <- list()

  # reset run afterwards
  id_run_init <- self$id_run
  on.exit(self$id_run <- id_run_init, add = TRUE)

  # Evaluate each run
  for (id_run in runs_required) {
    message(glue::glue("\n=== Evaluating run {id_run} ==="))
    self$id_run <- id_run

    all_similarity_results[[id_run]] <-
      data.table::data.table(
        id_run = id_run,
        id_trans = viable_trans[["id_trans"]],
        similarity = NA_real_
      )

    tryCatch(
      {
        # Run simulation
        self$alloc_dinamica(
          id_periods = posterior_historical_periods,
          work_dir = work_dir,
          keep_intermediate = keep_intermediate,
          select_score = select_score,
          select_maximize = select_maximize
        )

        # Get simulated data for final period
        rast_sim_final <- self$lulc_data_as_rast(id_period = id_period_final)

        message("  Computing per-transition fuzzy similarity...")

        # Compute fuzzy similarity per transition
        similarities <- mapply(
          FUN = calc_transition_similarity,
          from_class = viable_trans[["id_lulc_anterior"]],
          to_class = viable_trans[["id_lulc_posterior"]],
          MoreArgs = list(
            initial_map = rast_initial,
            observed_map = rast_obs_final,
            simulated_map = rast_sim_final,
            window_size = 11L,
            use_exp_decay = TRUE,
            decay_divisor = 2.0
          ),
          SIMPLIFY = FALSE
        )

        all_similarity_results[[id_run]] <-
          data.table::data.table(
            id_run = id_run,
            id_trans = viable_trans[["id_trans"]],
            similarity = pluck_wildcard(similarities, NA, "similarity") |> unlist()
          )
      },
      error = function(e) {
        warning(glue::glue(
          "Failed to evaluate run {id_run}: {e$message}"
        ))
      }
    )
  }

  message("\n=== Evaluation Complete ===")

  # Combine all similarity results
  similarity_dt <- data.table::rbindlist(all_similarity_results)

  # Augment alloc_params_t with similarity metrics
  result_params <- merge(
    unfiltered_alloc_params_t[, -"similarity"],
    similarity_dt,
    by = c("id_run", "id_trans"),
    all.x = TRUE
  )

  result_params
}
