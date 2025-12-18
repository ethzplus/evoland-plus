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
    Frac_expander = frac_expander
  )]

  expansion_path <- file.path(temp_dir, "expansion_table.csv")
  data.table::fwrite(expansion_table, expansion_path)

  message(glue::glue("  Wrote expansion table to {basename(expansion_path)}"))

  # 4. Write patcher table
  patcher_table <- alloc_params_full[, .(
    `From*` = id_lulc_anterior,
    `To*` = id_lulc_posterior,
    Mean_Patch_Size = mean_patch_size,
    Patch_Size_Variance = patch_size_variance,
    Patch_Isometry = patch_isometry
  )]

  patcher_path <- file.path(temp_dir, "patcher_table.csv")
  data.table::fwrite(patcher_table, patcher_path)

  message(glue::glue("  Wrote patcher table to {basename(patcher_path)}"))

  # 5. Write anterior.tif
  anterior_path <- file.path(temp_dir, "anterior.tif")
  terra::writeRaster(anterior_rast, anterior_path, overwrite = TRUE)

  message(glue::glue("  Wrote anterior LULC to {basename(anterior_path)}"))

  # 6. Generate probability maps
  prob_map_dir <- file.path(temp_dir, "probability_maps")
  dir.create(prob_map_dir, showWarnings = FALSE, recursive = TRUE)

  message("  Generating probability maps...")

  # Get trans_models_t
  trans_models <- self$trans_models_t

  # Get predictor IDs for each transition from trans_preds_t
  trans_preds <- self$trans_preds_t

  for (i in seq_len(nrow(viable_trans))) {
    id_trans <- viable_trans$id_trans[i]
    id_lulc_ant <- viable_trans$id_lulc_anterior[i]
    id_lulc_post <- viable_trans$id_lulc_posterior[i]

    # Get model for this transition
    model_row <- trans_models[id_trans == !!id_trans]

    if (nrow(model_row) == 0L) {
      stop(glue::glue("No model found for id_trans={id_trans}"))
    }

    # Deserialize full model
    model_obj <- qs2::qs_deserialize(model_row$model_obj_full[[1]])

    # Get predictor IDs for this transition
    id_preds <- trans_preds$id_pred[trans_preds$id_trans == id_trans]

    if (length(id_preds) == 0L) {
      warning(glue::glue("No predictors for id_trans={id_trans}, skipping probability map"))
      next
    }

    # Get predictor data for posterior period
    pred_data <- self$trans_pred_data_v(
      id_trans = id_trans,
      id_pred = id_preds,
      na_value = 0 # Replace NAs with 0 for prediction
    )

    # Filter to posterior period only
    pred_data_post <- pred_data[id_period == id_period_post]

    if (nrow(pred_data_post) == 0L) {
      warning(glue::glue(
        "No predictor data for id_trans={id_trans}, id_period={id_period_post}"
      ))
      next
    }

    # Predict probabilities
    # Drop id_coord, id_period, result columns for prediction
    pred_cols <- grep("^id_pred_", names(pred_data_post), value = TRUE)
    pred_df <- as.data.frame(pred_data_post[, ..pred_cols])

    # Predict - assuming model has predict() method that returns probabilities
    tryCatch(
      {
        probs <- predict(model_obj, newdata = pred_df, type = "response")

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

        terra::writeRaster(prob_rast, prob_path, overwrite = TRUE)

        message(glue::glue(
          "    [{i}/{nrow(viable_trans)}] Probability map: {id_lulc_ant} -> {id_lulc_post}"
        ))
      },
      error = function(e) {
        warning(glue::glue(
          "Failed to generate probability map for id_trans={id_trans}: {e$message}"
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
      "id_periods must be contiguous and sequential" = {
        all(diff(id_periods) == 1L)
      },
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
    base_work_dir <- file.path(
      work_dir,
      format(Sys.time(), "perturbation_%s_%Y%m%d_%H%M%S") |>
        sprintf(id_perturbation)
    )
    dir.create(base_work_dir, showWarnings = FALSE, recursive = TRUE)

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

    self[[table_name]] <- final_results

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

#' Evaluate Allocation Parameters
#'
#' @description
#' Evaluates allocation parameters by running historical simulations and comparing
#' the final simulated period against observed data. Computes per-transition
#' accuracy metrics.
#'
#' @param id_perturbations Integer vector of perturbation IDs to evaluate.
#'   If NULL (default), evaluates all perturbations in `alloc_params_t`.
#' @param work_dir Character, base directory for Dinamica runs. Default: "dinamica_rundir"
#' @param keep_intermediate Logical, keep intermediate simulation files? Default: FALSE
#'
#' @return An `alloc_params_t` table with added goodness-of-fit columns:
#'   - `accuracy_overall`: Overall cell-by-cell accuracy
#'   - `accuracy_per_trans`: Per-transition accuracy (for cells that transitioned)
#'   - `n_cells_simulated`: Number of cells in simulation
#'   - `n_cells_matched`: Number of cells that matched observations
#'
#' @section Details:
#' The evaluation workflow:
#' 1. Identifies all historical (non-extrapolated) periods from `periods_t`
#' 2. For each `id_perturbation`:
#'    - Runs `alloc_dinamica()` over historical periods
#'    - Compares final simulated period against observed `lulc_data_t`
#'    - Computes accuracy metrics per transition
#' 3. Returns updated `alloc_params_t` with goodness-of-fit metrics
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
    historical_periods <- self$periods_t[is_extrapolated == FALSE]
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

    message(glue::glue(
      "Evaluating allocation parameters\n",
      "  Perturbations: {paste(id_perturbations, collapse = ', ')}\n",
      "  Historical periods: {paste(id_periods_hist, collapse = ' -> ')}"
    ))

    # Get observed data for final period
    id_period_final <- max(id_periods_hist)
    observed_final <- self$lulc_data_t[id_period == id_period_final]

    # Storage for evaluation results
    eval_results <- list()

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
          sim_final <- self$get_table(sim_table_name)[id_period == id_period_final]

          # Compare simulated vs observed
          message("Computing accuracy metrics...")

          # Merge on id_coord
          comparison <- merge(
            observed_final,
            sim_final,
            by = "id_coord",
            suffixes = c("_obs", "_sim")
          )

          n_cells <- nrow(comparison)
          n_matched <- sum(comparison$id_lulc_obs == comparison$id_lulc_sim)
          accuracy_overall <- n_matched / n_cells

          message(glue::glue(
            "  Overall accuracy: {round(accuracy_overall * 100, 2)}% ",
            "({n_matched}/{n_cells} cells)"
          ))

          # Per-transition accuracy
          # Get transitions for the final period
          id_period_penultimate <- id_periods_hist[length(id_periods_hist) - 1L]

          observed_penultimate <- self$lulc_data_t[id_period == id_period_penultimate]

          # Create transition table: penultimate -> final (observed)
          trans_obs <- merge(
            observed_penultimate[, .(id_coord, id_lulc_ant = id_lulc)],
            observed_final[, .(id_coord, id_lulc_post = id_lulc)],
            by = "id_coord"
          )

          # Add simulated final period
          trans_comp <- merge(
            trans_obs,
            sim_final[, .(id_coord, id_lulc_sim = id_lulc)],
            by = "id_coord"
          )

          # Compute per-transition accuracy
          viable_trans <- self$trans_meta_t[is_viable == TRUE]

          trans_accuracy_list <- list()

          for (i in seq_len(nrow(viable_trans))) {
            id_trans <- viable_trans$id_trans[i]
            id_lulc_ant <- viable_trans$id_lulc_anterior[i]
            id_lulc_post <- viable_trans$id_lulc_posterior[i]

            # Cells that transitioned (observed)
            trans_cells <- trans_comp[
              id_lulc_ant == !!id_lulc_ant & id_lulc_post == !!id_lulc_post
            ]

            if (nrow(trans_cells) > 0L) {
              n_trans <- nrow(trans_cells)
              n_trans_matched <- sum(trans_cells$id_lulc_sim == id_lulc_post)
              acc_trans <- n_trans_matched / n_trans

              trans_accuracy_list[[length(trans_accuracy_list) + 1L]] <- data.table::data.table(
                id_perturbation = id_pert,
                id_trans = id_trans,
                n_trans_cells = n_trans,
                n_trans_matched = n_trans_matched,
                accuracy_trans = acc_trans
              )
            }
          }

          if (length(trans_accuracy_list) > 0L) {
            trans_accuracy <- data.table::rbindlist(trans_accuracy_list)
          } else {
            trans_accuracy <- data.table::data.table(
              id_perturbation = integer(0),
              id_trans = integer(0),
              n_trans_cells = integer(0),
              n_trans_matched = integer(0),
              accuracy_trans = numeric(0)
            )
          }

          # Store overall metrics for this perturbation
          eval_results[[length(eval_results) + 1L]] <- list(
            id_perturbation = id_pert,
            accuracy_overall = accuracy_overall,
            n_cells_total = n_cells,
            n_cells_matched = n_matched,
            trans_accuracy = trans_accuracy
          )

          message(glue::glue("Perturbation {id_pert} evaluation complete"))
        },
        error = function(e) {
          warning(glue::glue(
            "Failed to evaluate perturbation {id_pert}: {e$message}"
          ))
        }
      )
    }

    if (length(eval_results) == 0L) {
      stop("No perturbations could be evaluated")
    }

    # Combine results with original alloc_params_t
    message("\nCombining evaluation results with allocation parameters...")

    # Start with original alloc_params_t
    result_params <- data.table::copy(self$alloc_params_t)

    # Add overall accuracy metrics
    overall_metrics <- data.table::rbindlist(lapply(eval_results, function(x) {
      data.table::data.table(
        id_perturbation = x$id_perturbation,
        accuracy_overall = x$accuracy_overall,
        n_cells_total = x$n_cells_total,
        n_cells_matched = x$n_cells_matched
      )
    }))

    result_params <- merge(
      result_params,
      overall_metrics,
      by = "id_perturbation",
      all.x = TRUE
    )

    # Add per-transition accuracy
    all_trans_accuracy <- data.table::rbindlist(
      lapply(eval_results, function(x) x$trans_accuracy)
    )

    result_params <- merge(
      result_params,
      all_trans_accuracy,
      by = c("id_perturbation", "id_trans"),
      all.x = TRUE
    )

    # Cast back to alloc_params_t
    result_params <- as_alloc_params_t(result_params)

    message(glue::glue(
      "Evaluation complete!\n",
      "  Evaluated {length(eval_results)} perturbations\n",
      "  Mean overall accuracy: {round(mean(overall_metrics$accuracy_overall) * 100, 2)}%"
    ))

    result_params
  }
)
