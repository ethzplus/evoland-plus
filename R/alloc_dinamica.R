#' Dinamica EGO Allocation Methods
#'
#' @description
#' Methods for running Dinamica EGO allocation simulations and evaluating
#' allocation parameters. These methods are added to the `evoland_db` class.
#'
#' @name alloc_dinamica
#' @include evoland_db.R trans_models_t.R alloc_params_t.R
NULL

#' Private helper: Set up input files for a single Dinamica allocation iteration
#'
#' @param self The evoland_db instance
#' @param id_period_ant Integer, anterior period ID
#' @param id_period_post Integer, posterior period ID
#' @param id_perturbation Integer, perturbation ID for selecting allocation parameters
#' @param anterior_rast SpatRast with anterior LULC state
#' @param temp_dir Character, path to temporary directory
#'
#' @return List with paths to created files
#' @keywords internal
alloc_dinamica_setup_inputs <- function(
  self,
  id_period_ant,
  id_period_post,
  id_perturbation,
  anterior_rast,
  temp_dir
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

  # 2. Get allocation parameters for this perturbation
  alloc_params <- self$alloc_params_t[id_perturbation == !!id_perturbation]

  # Join with trans_meta to get From/To columns
  alloc_params_full <- merge(
    alloc_params,
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

  message("  Generating probability maps...")

  # Get trans_models_t
  trans_models <- self$trans_models_t

  # Get predictor IDs for each transition from trans_preds_t
  trans_preds <- self$trans_preds_t

  for (i in seq_len(nrow(viable_trans))) {
    id_trans_sel <- viable_trans$id_trans[i]
    id_lulc_ant <- viable_trans$id_lulc_anterior[i]
    id_lulc_post <- viable_trans$id_lulc_posterior[i]

    # Get model for this transition
    model_row <- trans_models[id_trans == id_trans_sel]

    if (nrow(model_row) == 0L) {
      stop(glue::glue("No model found for id_trans={id_trans_sel}"))
    } else if (nrow(model_row) > 1) {
      stop(glue::glue(
        "Multiple models found for id_trans={id_trans_sel}, ",
        "edit trans_models_t to have only one per transition"
      ))
    }

    # Deserialize full model
    model_obj <- qs2::qs_deserialize(model_row$model_obj_full[[1]])

    # Get predictor IDs for this transition
    id_preds <- trans_preds$id_pred[trans_preds$id_trans == id_trans_sel]

    if (length(id_preds) == 0L) {
      warning(glue::glue("No predictors for id_trans={id_trans_sel}, skipping probability map"))
      next
    }

    # Get predictor data for posterior period
    pred_data <- self$trans_pred_data_v(
      id_trans = id_trans_sel,
      id_pred = id_preds,
      na_value = 0 # Replace NAs with 0 for prediction
    )

    # Filter to posterior period only
    pred_data_post <- pred_data[id_period == id_period_post]

    if (nrow(pred_data_post) == 0L) {
      warning(glue::glue(
        "No predictor data for id_trans={id_trans_sel}, id_period={id_period_post}"
      ))
      next
    }

    # Predict probabilities
    # Drop id_coord, id_period, result columns for prediction
    pred_cols <- grep("^id_pred_", names(pred_data_post), value = TRUE)

    # Predict - assuming model has predict() method that returns probabilities
    tryCatch(
      {
        probs <- predict(model_obj, newdata = pred_data_post[, ..pred_cols], type = "response")
        # Ensure probabilities are in [0, 1]
        probs <- pmax(0, pmin(1, probs))

        # Create a data.table with id_coord and probability
        prob_dt <- data.table::data.table(
          id_coord = pred_data_post$id_coord,
          probability = probs
        )

        # Join with coords to get spatial locations
        coords_minimal <- self$coords_minimal
        prob_spatial <- merge(coords_minimal, prob_dt, by = "id_coord")

        # Create raster template matching anterior_rast
        prob_rast <- terra::rast(anterior_rast)

        # Rasterize probabilities
        prob_rast <- terra::rasterize(
          x = prob_spatial[, .(lon, lat)],
          y = prob_rast,
          values = prob_spatial$probability,
          fun = "first"
        )

        # Use numerical prefix to ensure correct file ordering
        # Prefix format: 0001, 0002, etc.
        prefix <- sprintf("%04d", i)
        prob_path <- file.path(
          prob_map_dir,
          glue::glue("{prefix}_trans_{id_lulc_ant}_to_{id_lulc_post}.tif")
        )

        terra::writeRaster(
          prob_rast,
          prob_path,
          overwrite = TRUE,
          NAflag = -999 # because dinamica cannot handle nan
        )

        message(glue::glue(
          "    [{i}/{nrow(viable_trans)}] Probability map: {id_lulc_ant} -> {id_lulc_post}"
        ))
      },
      error = function(e) {
        warning(glue::glue(
          "Failed to generate probability map for id_trans={id_trans_sel}: {e$message}"
        ))
      }
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

#' Private helper: Run a single Dinamica allocation iteration
#'
#' @param self The evoland_db instance
#' @param id_period_ant Integer, anterior period ID
#' @param id_period_post Integer, posterior period ID
#' @param id_perturbation Integer, perturbation ID
#' @param anterior_rast SpatRast with anterior LULC state
#' @param iteration_dir Character, path to iteration directory
#'
#' @return lulc_data_t table with simulated results
#' @keywords internal
alloc_dinamica_single_iteration <- function(
  self,
  id_period_ant,
  id_period_post,
  id_perturbation,
  anterior_rast,
  iteration_dir
) {
  message(glue::glue(
    "Running Dinamica allocation: period {id_period_ant} -> {id_period_post}"
  ))

  # Set up input files
  input_files <- alloc_dinamica_setup_inputs(
    self = self,
    id_period_ant = id_period_ant,
    id_period_post = id_period_post,
    id_perturbation = id_perturbation,
    anterior_rast = anterior_rast,
    temp_dir = iteration_dir
  )

  # Run Dinamica
  message("  Executing Dinamica EGO...")

  tryCatch(
    {
      run_alloc_dinamica(
        work_dir = iteration_dir,
        echo = FALSE,
        write_logfile = TRUE
      )
    },
    error = function(e) {
      stop(glue::glue(
        "Dinamica EGO failed for period {id_period_ant} -> {id_period_post}: {e$message}"
      ))
    }
  )

  # Read posterior.tif
  posterior_path <- file.path(iteration_dir, "posterior.tif")

  if (!file.exists(posterior_path)) {
    stop(glue::glue(
      "Expected output file not found: {posterior_path}"
    ))
  }

  posterior_rast <- terra::rast(posterior_path)

  message("  Converting posterior raster to lulc_data_t...")

  # Extract using coords_t
  coords_t <- self$coords_t

  # Get EPSG from metadata
  coords_meta <- self$get_table_metadata("coords_t")
  epsg <- coords_meta[["epsg"]]

  # Set CRS for extraction
  terra::crs(posterior_rast) <- paste0("epsg:", epsg)

  extracted <- extract_using_coords_t(posterior_rast, coords_t, na_omit = TRUE)

  # Convert to lulc_data_t format
  # extracted has columns: id_coord, layer, value
  # We need: id_coord, id_lulc, id_period
  lulc_result <- data.table::data.table(
    id_coord = extracted$id_coord,
    id_lulc = as.integer(extracted$value),
    id_period = id_period_post
  )

  # Cast to lulc_data_t
  lulc_result <- as_lulc_data_t(lulc_result)

  message(glue::glue("  Extracted {nrow(lulc_result)} cells"))

  lulc_result
}

#' Run Dinamica EGO Allocation Simulation
#'
#' @description
#' Runs a path-dependent Monte Carlo simulation using Dinamica EGO for land use
#' allocation. Iterates through a sequence of contiguous periods, using the
#' simulated output from one period as the input to the next.
#'
#' @param id_periods Integer vector of contiguous period IDs to simulate.
#'   Must be in sequential order. The first period is used as the origin state.
#' @param id_perturbation Integer, perturbation ID for selecting allocation
#'   parameters from `alloc_params_t`
#' @param work_dir Character, base directory for Dinamica runs. A subdirectory
#'   will be created for this simulation. Default: "dinamica_rundir"
#' @param keep_intermediate Logical, keep intermediate files after successful
#'   completion? Default: FALSE
#'
#' @return Invisibly returns the table name where results were written
#'   (e.g., "lulc_data_t_perturbation_1")
#'
#' @section Requirements:
#' - `trans_models_t` must have full models fitted
#' - `alloc_params_t` must exist with specified `id_perturbation`
#' - `periods_t` must contain all specified `id_periods`
#' - Dinamica EGO must be installed and `DinamicaConsole` must be on PATH
#'
#' @examples
#' \dontrun{
#' db <- evoland_db$new("path/to/db")
#' # Simulate historical periods with perturbation 1
#' db$alloc_dinamica(
#'   id_periods = 1:3,
#'   id_perturbation = 1L
#' )
#' }
#'
#' @name alloc_dinamica
NULL

evoland_db$set(
  "public",
  "alloc_dinamica",
  function(
    id_periods,
    id_perturbation,
    work_dir = "dinamica_rundir",
    keep_intermediate = FALSE
  ) {
    # Validate inputs
    stopifnot(
      "id_periods must be an integer vector" = is.numeric(id_periods),
      "id_periods must be contiguous" = all(diff(id_periods) == 1L),
      "id_periods must have at least 2 elements" = length(id_periods) >= 2L,
      "id_perturbation must be a single integer" = {
        length(id_perturbation) == 1L && is.numeric(id_perturbation)
      }
    )

    id_periods <- as.integer(id_periods)
    id_perturbation <- as.integer(id_perturbation)

    # Check that periods exist
    available_periods <- self$periods_t$id_period
    missing_periods <- setdiff(id_periods, available_periods)

    if (length(missing_periods) > 0L) {
      stop(glue::glue(
        "Periods not found in periods_t: {paste(missing_periods, collapse = ', ')}"
      ))
    }

    # Check that perturbation exists
    available_perturbations <- unique(self$alloc_params_t$id_perturbation)
    if (!id_perturbation %in% available_perturbations) {
      stop(glue::glue(
        "id_perturbation={id_perturbation} not found in alloc_params_t. ",
        "Available: {paste(available_perturbations, collapse = ', ')}"
      ))
    }

    # Create base work directory
    base_work_dir <-
      file.path(
        work_dir,
        sprintf("perturbation_%s", id_perturbation)
      ) |>
      ensure_dir()

    message(glue::glue(
      "Starting Dinamica allocation simulation\n",
      "  Periods: {paste(id_periods, collapse = ' -> ')}\n",
      "  Perturbation: {id_perturbation}\n",
      "  Work directory: {base_work_dir}"
    ))

    # Get metadata for raster creation
    coords_meta <- self$get_table_metadata("coords_t")
    resolution <- coords_meta[["resolution"]]
    epsg <- coords_meta[["epsg"]]

    stopifnot(
      "coords_t must have resolution metadata" = !is.null(resolution),
      "coords_t must have epsg metadata" = !is.null(epsg)
    )

    # Initialize with first period as observed data
    id_period_origin <- id_periods[1]

    message(glue::glue("Loading origin period {id_period_origin} from lulc_data_t..."))

    current_rast <- self$lulc_data_as_rast(
      resolution = resolution,
      id_period = id_period_origin
    )

    # Store all results
    all_results <- list()

    # Iterate through periods
    for (i in seq_len(length(id_periods) - 1L)) {
      id_period_ant <- id_periods[i]
      id_period_post <- id_periods[i + 1L]

      # Create iteration directory
      iteration_dir <- file.path(
        base_work_dir,
        glue::glue("iteration_{i}_period_{id_period_ant}_to_{id_period_post}")
      )
      dir.create(iteration_dir, showWarnings = FALSE, recursive = TRUE)

      message(glue::glue("\n=== Iteration {i}/{length(id_periods) - 1L} ==="))

      # Run single iteration
      lulc_result <- alloc_dinamica_single_iteration(
        self = self,
        id_period_ant = id_period_ant,
        id_period_post = id_period_post,
        id_perturbation = id_perturbation,
        anterior_rast = current_rast,
        iteration_dir = iteration_dir
      )

      # Store result
      all_results[[length(all_results) + 1L]] <- lulc_result

      # Update current rast for next iteration
      # Copy posterior.tif to a common location for next iteration
      posterior_path <- file.path(iteration_dir, "posterior.tif")
      current_path <- file.path(base_work_dir, "current.tif")
      file.copy(posterior_path, current_path, overwrite = TRUE)

      # Load as current_rast for next iteration
      current_rast <- terra::rast(current_path)

      message(glue::glue("Iteration {i} complete\n"))
    }

    # Combine all results
    message("Combining results...")
    final_results <- data.table::rbindlist(all_results)

    # Write to database as new table
    table_name <- glue::glue("lulc_data_t_perturbation_{id_perturbation}")

    message(glue::glue("Writing results to {table_name}..."))

    self$commit(final_results, table_name, method = "overwrite")

    message(glue::glue(
      "Simulation complete!\n",
      "  Results written to: {table_name}\n",
      "  Total cells simulated: {nrow(final_results)}"
    ))

    # Clean up if requested
    if (!keep_intermediate) {
      message("Cleaning up intermediate files...")
      unlink(base_work_dir, recursive = TRUE)
    } else {
      message(glue::glue("Intermediate files retained in: {base_work_dir}"))
    }

    invisible(table_name)
  }
)

#' Evaluate Allocation Parameters with Fuzzy Similarity
#'
#' @description
#' Evaluates allocation parameters by running simulations over historical
#' periods and comparing results against observed data using fuzzy similarity.
#' For each perturbation:
#' 1. Runs `alloc_dinamica()` using historical periods to produce simulated final period
#' 2. Compares initial vs final observed and initial vs final simulated using fuzzy similarity
#' 3. Returns `alloc_params_t` augmented with per-transition similarity metrics
#'
#' @param id_perturbations Integer vector of perturbation IDs to evaluate.
#'   If NULL (default), evaluates all perturbations in `alloc_params_t`.
#' @param work_dir Character path for Dinamica working directory. Default "dinamica_rundir".
#' @param keep_intermediate Logical, keep intermediate Dinamica files? Default FALSE.
#'
#' @details
#' The evaluation uses fuzzy similarity with spatial tolerance (11x11 window,
#' exponential decay with divisor=2). For each transition, compares the spatial
#' pattern of changes from initial to final period between observed and simulated.
#'
#' Returns the `alloc_params_t` table augmented with:
#' - `similarity`: Fuzzy similarity per transition (0-1, NA if no observed transitions)
#'
#' @examples
#' \dontrun{
#' db <- evoland_db$new("path/to/db")
#' # Evaluate all perturbations
#' evaluated_params <- db$eval_alloc_params_t()
#'
#' # Evaluate specific perturbations
#' evaluated_params <- db$eval_alloc_params_t(id_perturbations = 1:3)
#' }
#'
#' @name eval_alloc_params_t
NULL

evoland_db$set(
  "public",
  "eval_alloc_params_t",
  function(
    id_perturbations = NULL,
    work_dir = "dinamica_rundir",
    keep_intermediate = FALSE
  ) {
    # Get historical periods
    historical_periods <- self$periods_t[is_extrapolated == FALSE & id_period > 0]
    data.table::setorder(historical_periods, id_period)

    id_periods_hist <- historical_periods$id_period

    if (length(id_periods_hist) < 2L) {
      stop("Need at least 2 historical periods for evaluation")
    }

    # Get perturbations to evaluate
    if (is.null(id_perturbations)) {
      id_perturbations <- unique(self$alloc_params_t$id_perturbation)
    } else {
      id_perturbations <- as.integer(id_perturbations)
    }

    # Get initial and final periods
    id_period_initial <- min(id_periods_hist)
    id_period_final <- max(id_periods_hist)

    message(glue::glue(
      "Evaluating allocation parameters with fuzzy similarity\n",
      "  Perturbations: {paste(id_perturbations, collapse = ', ')}\n",
      "  Initial period: {id_period_initial}\n",
      "  Final period: {id_period_final}"
    ))

    # Get observed data for initial and final periods
    observed_initial <-
      self$fetch("lulc_data_t", where = glue::glue("id_period = {id_period_initial}")) |>
      as_lulc_data_t()

    observed_final <-
      self$fetch("lulc_data_t", where = glue::glue("id_period = {id_period_final}")) |>
      as_lulc_data_t()

    # Get viable transitions
    viable_trans <- self$trans_meta_t[is_viable == TRUE]
    stopifnot("No viable transitions found" = nrow(viable_trans) > 0L)

    # Storage for per-transition similarity results
    all_similarity_results <- list()

    # Evaluate each perturbation
    for (id_pert in id_perturbations) {
      message(glue::glue("\n=== Evaluating perturbation {id_pert} ==="))

      tryCatch(
        {
          # Run simulation
          sim_table_name <- self$alloc_dinamica(
            id_periods = id_periods_hist,
            id_perturbation = id_pert,
            work_dir = work_dir,
            keep_intermediate = keep_intermediate
          )

          # Get simulated data for final period
          sim_final <-
            self$fetch(sim_table_name, where = glue::glue("id_period = {id_period_final}")) |>
            as_lulc_data_t()

          message("  Converting to rasters...")

          # Convert tabular data to rasters
          rast_initial <- tabular_to_raster(observed_initial, self$coords_t)
          rast_obs_final <- tabular_to_raster(observed_final, self$coords_t)
          rast_sim_final <- tabular_to_raster(sim_final, self$coords_t)

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

            message(glue::glue(
              "    Transition {id_trans} ({id_lulc_ant}->{id_lulc_post}): ",
              "similarity = {ifelse(is.na(trans_sim$similarity), 'NA', round(trans_sim$similarity, 4))}"
            ))

            # Store result
            all_similarity_results[[length(all_similarity_results) + 1L]] <- data.table::data.table(
              id_perturbation = id_pert,
              id_trans = id_trans,
              similarity = trans_sim$similarity
            )
          }

          # Clean up rasters
          rm(rast_initial, rast_obs_final, rast_sim_final)
          gc()
        },
        error = function(e) {
          warning(glue::glue(
            "Failed to evaluate perturbation {id_pert}: {e$message}"
          ))

          # Add NA results for this perturbation
          for (i in seq_len(nrow(viable_trans))) {
            all_similarity_results[[length(all_similarity_results) + 1L]] <- data.table::data.table(
              id_perturbation = id_pert,
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
      self$alloc_params_t,
      similarity_dt,
      by = c("id_perturbation", "id_trans"),
      all.x = TRUE
    )

    message(glue::glue("Returning augmented alloc_params_t with {nrow(result_params)} rows"))

    result_params
  }
)
