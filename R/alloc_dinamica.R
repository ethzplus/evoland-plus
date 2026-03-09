#' Dinamica EGO Allocation Methods
#'
#' @description
#' Methods for running Dinamica EGO allocation simulations and evaluating
#' allocation parameters. These methods are added to the `evoland_db` class.
#'
#' @name alloc_dinamica
#' @include trans_models_t.R alloc_params_t.R
NULL

#' @describeIn alloc_dinamica Private helper: Set up input files for a single Dinamica
#' allocation iteration
#' @param self The evoland_db instance
#' @param id_period_ant Integer, anterior period ID
#' @param id_period_post Integer, posterior period ID
#' @param anterior_rast SpatRast with anterior LULC state
#' @param temp_dir Character, path to temporary directory
#' @return List with paths to created files
#' @keywords internal
alloc_dinamica_setup_inputs <- function(
  self,
  id_period_ant,
  id_period_post,
  anterior_rast,
  temp_dir,
  gof_criterion,
  gof_maximize
) {
  # Get metadata
  coords_meta <- self$get_table_metadata("coords_t")
  epsg <- coords_meta[["epsg"]]

  # Get viable transitions
  viable_trans <- self$trans_meta_t[is_viable == TRUE]

  stopifnot(
    "No viable transitions found" = nrow(viable_trans) > 0L,
    "coords_t must have epsg metadata" = !is.null(epsg)
  )

  # Sort transitions consistently (by id_lulc_anterior, then id_lulc_posterior)
  data.table::setorder(viable_trans, id_lulc_anterior, id_lulc_posterior)

  # 1. Write transition rates
  trans_rates <- self$trans_rates_dinamica_v(id_period_post)

  # Ensure same sort order as viable_trans
  data.table::setorder(trans_rates, `From*`, `To*`)

  trans_rates_path <- file.path(temp_dir, "trans_rates.csv")
  data.table::fwrite(trans_rates, trans_rates_path)

  message(glue::glue("  Wrote transition rates to {basename(trans_rates_path)}"))

  # 2. Get allocation parameters for this run
  alloc_params_full <-
    self$alloc_params_t |>
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

  expansion_path <- file.path(temp_dir, "expansion_table.csv")
  data.table::fwrite(expansion_table, expansion_path)

  message(glue::glue("  Wrote expansion table to {basename(expansion_path)}"))

  # 4. Write patcher table
  patcher_table <- alloc_params_full[, .(
    `From*` = id_lulc_anterior,
    `To*` = id_lulc_posterior,
    Mean_Patch_Size = ifelse(is.na(mean_patch_size), 0, mean_patch_size),
    Patch_Size_Variance = ifelse(is.na(patch_size_variance), 0, patch_size_variance),
    Patch_Isometry = ifelse(is.na(patch_isometry), 0, patch_isometry)
  )]

  patcher_path <- file.path(temp_dir, "patcher_table.csv")
  data.table::fwrite(patcher_table, patcher_path)

  message(glue::glue("  Wrote patcher table to {basename(patcher_path)}"))

  # 5. Write anterior.tif
  anterior_path <- file.path(temp_dir, "anterior.tif")
  terra::writeRaster(
    anterior_rast,
    anterior_path,
    overwrite = TRUE,
    datatype = "INT1U",
    NAflag = 255 # because dinamica cannot handle nan
  )

  message(glue::glue("  Wrote anterior LULC to {basename(anterior_path)}"))

  # 6. Generate probability maps
  prob_map_dir <-
    file.path(temp_dir, "probability_map_dir") |>
    ensure_dir()

  message("  Writing probability maps...")
  coords_minimal <- self$coords_minimal

  trans_pots_t <- self$predict_trans_pot(
    id_period_post = id_period_post,
    gof_criterion = gof_criterion,
    gof_maximize = gof_maximize
  )

  # Iterate over viable transitions and write probability maps
  for (i in seq_len(nrow(viable_trans))) {
    id_trans_sel <- viable_trans$id_trans[i]
    prob_spatial <- coords_minimal[
      trans_pots_t[id_trans == id_trans_sel],
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

#' @describeIn alloc_dinamica Private helper: Run a single Dinamica allocation iteration
#' @param iteration_dir Character, path to iteration directory
#' @return lulc_data_t table with simulated results
#' @keywords internal
alloc_dinamica_one_period <- function(
  self,
  id_period_ant,
  id_period_post,
  anterior_rast,
  iteration_dir,
  gof_criterion,
  gof_maximize
) {
  message(glue::glue(
    "Running Dinamica allocation: period {id_period_ant} -> {id_period_post}"
  ))

  # Set up input files
  input_files <- alloc_dinamica_setup_inputs(
    self = self,
    id_period_ant = id_period_ant,
    id_period_post = id_period_post,
    anterior_rast = anterior_rast,
    temp_dir = iteration_dir,
    gof_criterion = gof_criterion,
    gof_maximize = gof_maximize
  )

  gc() # just in case

  # Run Dinamica
  message("  Executing Dinamica EGO...")

  run_alloc_dinamica(
    work_dir = iteration_dir,
    echo = FALSE,
    write_logfile = TRUE
  )

  # Read posterior.tif
  posterior_rast <-
    file.path(iteration_dir, "posterior.tif") |>
    terra::rast()

  message("  Converting posterior raster to lulc_data_t...")

  # Extract using coords_t
  coords_t <- self$coords_t

  # Set CRS for extraction
  terra::crs(posterior_rast) <- paste0("epsg:", attr(coords_t, "epsg"))

  extracted <- extract_using_coords_t(posterior_rast, coords_t, na_omit = TRUE)

  # Convert to lulc_data_t format
  lulc_result <-
    data.table::data.table(
      id_run = self$id_run,
      id_coord = extracted$id_coord,
      id_lulc = as.integer(extracted$value),
      id_period = id_period_post
    ) |>
    as_lulc_data_t()

  message(glue::glue("  Extracted {nrow(lulc_result)} cells"))

  lulc_result
}


#' @describeIn alloc_dinamica Run Dinamica EGO allocation over multiple periods
#' @param id_periods Integer vector of posterior period IDs to simulate (must be
#' contiguous; e.g. if simulating period 4, data from period 3 will be used as
#' anterior data)
#' @param work_dir Character, path to working directory for simulations
#' @param keep_intermediate Logical, whether to keep intermediate files from simulations
alloc_dinamica <- function(
  self,
  id_periods,
  gof_criterion,
  gof_maximize,
  work_dir = "dinamica_rundir",
  keep_intermediate = FALSE
) {
  # Validate inputs
  stopifnot(
    "id_periods must be an integer vector" = is.numeric(id_periods),
    "id_periods must be contiguous" = all(diff(id_periods) == 1L),
    "id_run must be set" = !is.null(self$id_run)
  )

  # Check that periods exist
  available_periods <- self$periods_t$id_period
  missing_periods <- setdiff(id_periods, available_periods)
  if (length(missing_periods) > 0L) {
    stop(glue::glue(
      "Periods not found in periods_t: {paste(missing_periods, collapse = ', ')}"
    ))
  }

  # Create base work directory
  base_work_dir <-
    file.path(
      work_dir,
      sprintf("run_%s", self$id_run)
    ) |>
    ensure_dir()

  message(glue::glue(
    "Starting Dinamica allocation simulation\n",
    "  Periods: {paste(id_periods, collapse = ' -> ')}\n",
    "  Run: {self$id_run}\n",
    "  Work directory: {base_work_dir}"
  ))

  # Initialize with first period as observed data
  message(glue::glue("Loading origin period {id_periods[1]} from lulc_data_t..."))
  current_rast <- self$lulc_data_as_rast(id_period = id_periods[1] - 1L)

  # Iterate through periods
  i <- 1L
  for (id_period_post in id_periods) {
    id_period_ant <- id_period_post - 1L

    # Create iteration directory
    iteration_dir <-
      file.path(
        base_work_dir,
        glue::glue("iteration_{i}_period_{id_period_ant}_to_{id_period_post}")
      ) |>
      ensure_dir()

    message(glue::glue("\n=== Iteration {i}/{length(id_periods) - 1L} ==="))

    # Run single iteration
    lulc_result <- alloc_dinamica_one_period(
      self = self,
      id_period_ant = id_period_ant,
      id_period_post = id_period_post,
      anterior_rast = current_rast,
      iteration_dir = iteration_dir,
      gof_criterion = gof_criterion,
      gof_maximize = gof_maximize
    )

    # Store result
    self$commit(lulc_result, "lulc_data_t", method = "upsert")
    # Recompute neighbors for next period
    self$upsert_new_neighbors(id_period_post)
    # Update current rast for next iteration
    current_rast <- self$lulc_data_as_rast(id_period = id_period_post)

    message(glue::glue("Iteration {i} complete\n"))
    i <- i + 1L
  }

  message(glue::glue(
    "Simulation complete!\n",
    "  Results written to: lulc_data_t\n"
  ))

  # Clean up if requested
  if (!keep_intermediate) {
    message("Cleaning up intermediate files...")
    unlink(base_work_dir, recursive = TRUE)
  } else {
    message(glue::glue("Intermediate files retained in: {base_work_dir}"))
  }

  invisible(NULL)
}

#' @describeIn alloc_dinamica Evaluate allocation parameters using fuzzy
#' similarity over different runs.
#' @param work_dir Character, path to working directory for simulations
#' @param keep_intermediate Logical, whether to keep intermediate files from simulations
eval_alloc_params_t <- function(
  self,
  gof_criterion,
  gof_maximize,
  work_dir = "dinamica_rundir",
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
    "gof_criterion must be a single string" = {
      is.character(gof_criterion) && length(gof_criterion) == 1L
    },
    "gof_maximize must be TRUE or FALSE" = (gof_maximize || !gof_maximize),
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

  # Evaluate each run
  for (id_run in runs_required) {
    message(glue::glue("\n=== Evaluating run {id_run} ==="))
    self$id_run <- id_run

    tryCatch(
      {
        # Run simulation
        self$alloc_dinamica(
          id_periods = posterior_historical_periods,
          work_dir = work_dir,
          keep_intermediate = keep_intermediate,
          gof_criterion = gof_criterion,
          gof_maximize = gof_maximize
        )

        # Get simulated data for final period
        rast_sim_final <- self$lulc_data_as_rast(id_period = id_period_final)

        message("  Computing per-transition fuzzy similarity...")

        # Compute fuzzy similarity per transition
        for (i in seq_len(nrow(viable_trans))) {
          id_trans <- viable_trans$id_trans[i]
          id_lulc_ant <- viable_trans$id_lulc_anterior[i]
          id_lulc_post <- viable_trans$id_lulc_posterior[i]

          # Compute fuzzy similarity for this transition
          trans_sim <- calc_transition_similarity(
            initial_map = rast_initial,
            observed_map = rast_obs_final,
            simulated_map = rast_sim_final,
            from_class = id_lulc_ant,
            to_class = id_lulc_post,
            window_size = 11L,
            use_exp_decay = TRUE,
            decay_divisor = 2.0
          )

          # Store result
          all_similarity_results[[length(all_similarity_results) + 1L]] <- data.table::data.table(
            id_run = id_run,
            id_trans = id_trans,
            similarity = trans_sim$similarity
          )
        }

        rm(rast_sim_final)
        gc()
      },
      error = function(e) {
        warning(glue::glue(
          "Failed to evaluate run {id_run}: {e$message}"
        ))

        # Add NA results for this run
        for (i in seq_len(nrow(viable_trans))) {
          all_similarity_results[[length(all_similarity_results) + 1L]] <- data.table::data.table(
            id_run = id_run,
            id_trans = viable_trans$id_trans[i],
            similarity = NA_real_
          )
        }
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
