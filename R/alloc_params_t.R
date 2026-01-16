#' Create Allocation Parameters Table
#'
#' Creates a alloc_params_t table for storing transition model metadata and
#' serialized model objects. This function creates an empty table with proper
#' structure for storing fitted models.
#'
#' @name alloc_params_t
#' @include evoland_db.R
#'
#' @param x A list or data.frame coercible to a data.table
#'
#' @return A data.table of class "alloc_params_t" with columns:
#'   - `id_perturbation`: The perturbation's ID - no. 1 == unperturbed
#'   - `id_trans`: Foreign key to trans_meta_t
#'   - ... various other columns describing allocation parameters and goodness of fit
#' @export
as_alloc_params_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_perturbation = integer(0),
      id_trans = integer(0)
    )
  }
  new_evoland_table(
    x,
    "alloc_params_t",
    c("id_perturbation")
  )
}

#' @export
validate.alloc_params_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(x, "id_trans")
  # we don't know if there's an id_perturbation
  data.table::setcolorder(x, "id_perturbation", before = "id_trans", skip_absent = TRUE)

  stopifnot(is.integer(x[["id_trans"]]))

  return(x)
}

#' @export
#' @describeIn alloc_params_t Print a alloc_params_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.alloc_params_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_trans <- data.table::uniqueN(x[["id_trans"]])

    cat(glue::glue(
      "Allocation Parameters Table\n",
      "Rows: {nrow(x)}\n",
      "Transitions: {n_trans}\n\n"
    ))
  } else {
    cat("Allocation Parameters Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}


#' @desribeIn alloc_params_t Map patch elongation to Dinamica "isometry" patcher
#' parameter, c.f. fig. 3.10 in Mazy, 2022 https://theses.hal.science/tel-04382012v1
#' @param elongation Numeric vector of elongation values
#' @param curve Data frame with columns `isometry` and `elongation` defining the mapping curve
#' @param clamp Logical, whether to clamp values outside the curve range to min/max or to return NA
isometry_from_elongation <- function(
  elongation,
  curve = data.frame(
    # fmt: skip
    isometry =   c(0.0,  0.25,  0.5,   0.75,  1.0,   1.2,   1.5,   2.0),
    elongation = c(0.34, 0.334, 0.333, 0.328, 0.320, 0.172, 0.171, 0.17)
  ),
  clamp = TRUE
) {
  stopifnot(all(c("isometry", "elongation") %in% names(curve)))
  if (length(elongation) == 0L) {
    return(numeric())
  }
  if (all(is.na(elongation))) {
    return(rep(NA_real_, length(elongation)))
  }

  # Invert via interpolation isometry = f(elongation). For approx(), x must be increasing.
  # Since elongation is (approximately) decreasing with isometry, we sort by elongation.
  ord <- order(curve$elongation)
  e_x <- curve$elongation[ord]
  iso_y <- curve$isometry[ord]

  # Clamping behavior outside the observed e range
  rule <- if (clamp) 2 else 1 # 2 => clamp to endpoints, 1 => NA outside
  approx(
    x = e_x,
    y = iso_y,
    xout = elongation,
    method = "linear",
    rule = rule,
    ties = "ordered"
  )$y
}

#' @describeIn alloc_params_t
#' Compute allocation parameters for a single transition and period pair.
#' This is an internal function used by `create_alloc_params_t`.
#'
#' @param lulc_ant SpatRast with LULC data for the anterior (earlier) period
#' @param lulc_post SpatRast with LULC data for the posterior (later) period
#' @param id_lulc_ant Integer ID of the anterior LULC class
#' @param id_lulc_post Integer ID of the posterior LULC class
#'
#' @return A named list with allocation parameters:
#'   - mean_patch_size: Mean area of patches (hectares)
#'   - patch_size_variance: Standard deviation of patch area (hectares)
#'   - patch_isometry: Measure of patch shape regularity (0-1)
#'   - frac_expander: Fraction of transition cells adjacent to old patches in \[0, 1\]
#'   - frac_patcher: Fraction of transition cells forming new patches in \[0, 1\]
#'
#' @keywords internal
compute_alloc_params_single <- function(
  lulc_ant,
  lulc_post,
  id_lulc_ant,
  id_lulc_post
) {
  stopifnot(
    "cells must be square" = terra::res(lulc_ant)[1] == terra::res(lulc_ant)[2]
  )
  # Create binary raster of transition cells (anterior class -> posterior class)
  # 1 = cells that transitioned from id_lulc_ant to id_lulc_post
  # 0 or NA = all other cells
  trans_cells <- (lulc_ant == id_lulc_ant) & (lulc_post == id_lulc_post)
  trans_cells[trans_cells == 0] <- NA

  # Count total transition cells
  n_trans_cells <-
    terra::global(trans_cells, "sum", na.rm = TRUE)[1, 1] |>
    as.integer()

  if (is.na(n_trans_cells) || n_trans_cells == 0) {
    # No transitions occurred - return NULL or default values
    return(list(
      mean_patch_size = 0,
      patch_size_variance = 0,
      patch_isometry = 0,
      frac_expander = 0,
      frac_patcher = 0
    ))
  }

  # Identify patches in the anterior period for the posterior LULC class
  # Then identify transitioned cells that we want to know if they were expanded to as NA
  post_class_patches <- lulc_ant == id_lulc_post
  post_class_patches[post_class_patches == 0 & trans_cells == 1] <- NA

  # For each cell that was not yet id_lulc_post, count the neighbors that were already id_lulc_post
  neighbor_count <- terra::focal(
    post_class_patches,
    w = matrix(1, nrow = 3, ncol = 3), # 3x3 window (8-neighborhood)
    fun = "sum",
    na.rm = TRUE,
    na.policy = "only"
  )

  # Classify transition cells as expanders or patchers
  # Expander: has at least 1 neighbor that was already the posterior class
  # Patcher: has 0 neighbors that were already the posterior class
  is_expander <- (trans_cells == 1) & (neighbor_count >= 1)
  is_patcher <- (trans_cells == 1) & (neighbor_count == 0)
  is_patcher[is_patcher == 0] <- NA # ignore expanders in calculate_class_stats_cpp

  # Calculate percentages
  n_expanders <-
    terra::global(is_expander, "sum", na.rm = TRUE)[1, 1] |>
    as.numeric()
  n_patchers <-
    terra::global(is_patcher, "sum", na.rm = TRUE)[1, 1] |>
    as.numeric()

  frac_expander <- n_expanders / n_trans_cells
  frac_patcher <- n_patchers / n_trans_cells

  # Assuming square cells: get properties/shape of new patches
  trans_patch_stats <-
    is_patcher |>
    as.matrix(wide = TRUE) |>
    # cellsize = 1 because we want the patch characteristics in cell edge units
    calculate_class_stats_cpp(cellsize = 1)

  list(
    # Patch parameters for now intended for the Dinamica patcher
    # https://dinamicaego.com/dokuwiki/doku.php?id=patcher
    mean_patch_size = trans_patch_stats$patch_area_mean[1],
    patch_size_variance = trans_patch_stats$patch_area_variance[1],
    patch_isometry = isometry_from_elongation(trans_patch_stats$patch_elongation_mean[1]),
    frac_expander = frac_expander,
    frac_patcher = frac_patcher
  )
}

#' Initialize Allocation Parameters Table
#'
#' @description
#' Computes allocation parameters for all viable transitions, aggregated across
#' observed periods and then randomly perturbed N times. This is a method
#' on `evoland_db` that analyzes patch dynamics to determine expansion vs. patcher
#' behavior for the Dinamica allocation procedure.
#'
#' The method first computes parameters for all viable transitions across observed
#' periods (where `id_period > 1` and `is_extrapolated == FALSE`), then aggregates
#' (mean) these parameters by transition. Finally, it creates randomly perturbed
#' versions of these aggregated parameters and returns them together with the original
#' estimate, i.e. the result set size is (n viable transitions) * (m perturbations + 1)
#'
#' @details
#' The workflow is:
#' 1. For each transition and period pair:
#'    - Create rasters for the anterior and posterior periods
#'    - Identify transition cells (cells that changed from anterior to posterior class)
#'    - Use focal operations to determine if transition cells are adjacent to existing
#'      patches (expansion) or form new patches (patcher behavior)
#'    - Compute patch statistics using internal C++ implementation
#' 2. Aggregate parameters across periods (mean) for each transition
#' 3. For each transition, create N randomly perturbed versions:
#'    - Add random noise to frac_expander (normal distribution, mean=0, sd=sd)
#'    - Clamp frac_expander to \[0, 1\]
#'    - Recalculate frac_patcher as 1 - frac_expander
#' 4. Store all perturbed versions in the `alloc_params_t` table
#'
#' @param n_perturbations Integer number of perturbed parameter sets to generate
#'   per transition (default: 5)
#' @param sd Standard deviation for random perturbation of frac_expander as a
#'   fraction (default: 0.05)
#'
#' @section Requirements:
#' - `coords_t` must have `resolution` and `epsg` metadata
#' - `trans_meta_t` must have at least one viable transition
#' - `periods_t` must have at least one observed period with `id_period > 1`
#'
#' @return Invisibly returns the computed `alloc_params_t` table.
#'
#' @examples
#' \dontrun{
#' db <- evoland_db$new("path/to/db")
#' # ... populate with coords, periods, lulc_data, trans_meta ...
#' db$create_alloc_params_t()
#' }
#'
#' @name create_alloc_params_t
NULL

evoland_db$set(
  "public",
  "create_alloc_params_t",
  function(n_perturbations = 5L, sd = 0.05) {
    # Validate parameters
    stopifnot(
      "n_perturbations must be an int >= 0" = {
        (as.integer(n_perturbations) == n_perturbations) && n_perturbations >= 0
      },
      "sd must be a positive number" = sd > 0
    )

    n_perturbations <- as.integer(n_perturbations)

    # Get observed periods (not extrapolated, and > 1 since we need period - 1)
    periods <- self$periods_t[is_extrapolated == FALSE & id_period > 1]
    viable_trans <- self$trans_meta_t[is_viable == TRUE]
    resolution <- self$get_table_metadata("coords_t")[["resolution"]]

    # validate DB inputs
    stopifnot(
      "No viable transitions found in trans_meta_t" = nrow(viable_trans) > 0L,
      "No observed periods with id_period > 1 found in periods_t" = nrow(periods) > 0L,
      "coords_t must have resolution and epsg metadata" = !is.null(resolution)
    )

    raw_results <- list()

    message(glue::glue(
      "Computing allocation parameters for {nrow(viable_trans)} transitions ",
      "across {nrow(periods)} periods..."
    ))

    # Step 1: Compute parameters for all transition-period pairs
    for (i in seq_len(nrow(periods))) {
      period_post <- periods[i][["id_period"]]
      period_ant <- period_post - 1L

      message(glue::glue("  Processing period {period_ant} -> {period_post}"))

      # Get LULC data as rasters for both periods
      lulc_rast <- self$lulc_data_as_rast(
        resolution = resolution,
        id_period = c(period_ant, period_post)
      )

      # Loop over transitions
      for (j in seq_len(nrow(viable_trans))) {
        trans <- viable_trans[j]
        id_trans <- trans[["id_trans"]]

        # Compute allocation parameters
        alloc_params <- tryCatch(
          {
            stopifnot("Need data for at least 2 periods" = length(names(lulc_rast)) > 1L)
            compute_alloc_params_single(
              lulc_ant = lulc_rast[[1]],
              lulc_post = lulc_rast[[2]],
              id_lulc_ant = trans[["id_lulc_anterior"]],
              id_lulc_post = trans[["id_lulc_posterior"]]
            )
          },
          error = function(e) {
            warning(
              glue::glue(
                "Failed to compute allocation parameters for id_trans={id_trans}, ",
                "id_period={period_post}: {e$message}"
              ),
              call. = FALSE
            )
            NULL
          }
        )

        if (!is.null(alloc_params)) {
          raw_results[[length(raw_results) + 1]] <- data.table::data.table(
            id_trans = id_trans,
            id_period = period_post,
            mean_patch_size = alloc_params$mean_patch_size,
            patch_size_variance = alloc_params$patch_size_variance,
            patch_isometry = alloc_params$patch_isometry,
            frac_expander = alloc_params$frac_expander,
            frac_patcher = alloc_params$frac_patcher
          )
        }
      }
    }

    if (length(raw_results) == 0L) {
      stop("No allocation parameters could be computed")
    }

    # Convert to data.table
    raw_dt <- data.table::rbindlist(raw_results)

    # Step 2: Aggregate parameters across periods (mean) for each transition
    message("Aggregating parameters across periods...")

    # fmt: skip
    mean_na <- function(x) {m <- mean(x, na.rm = TRUE); ifelse(is.nan(m), NA_real_, m)}

    agg_dt <- raw_dt[,
      .(
        mean_patch_size = mean_na(mean_patch_size),
        patch_size_variance = mean_na(patch_size_variance),
        patch_isometry = mean_na(patch_isometry),
        frac_expander = mean_na(frac_expander),
        frac_patcher = mean_na(frac_patcher)
      ),
      by = id_trans
    ]

    # Step 3: Create N perturbed versions for each transition
    message(glue::glue("Creating {n_perturbations} randomly perturbed versions per transition..."))

    final_results <- list()
    final_results[[1]] <- agg_dt

    for (i in seq_len(n_perturbations)) {
      # Add random perturbation to frac_expander
      frac_exp_perturbed <- agg_dt[["frac_expander"]] + rnorm(nrow(agg_dt), mean = 0, sd = sd)

      # Clamp expanded / patched to [0, 1]
      frac_exp_perturbed <- pmax(0, pmin(1, frac_exp_perturbed))
      frac_patch_perturbed <- 1 - frac_exp_perturbed

      # add to list of perturbed params
      agg_dt_perturbed <- data.table::copy(agg_dt)
      data.table::set(agg_dt_perturbed, j = "frac_expander", value = frac_exp_perturbed)
      data.table::set(agg_dt_perturbed, j = "frac_patcher", value = frac_patch_perturbed)
      final_results[[i + 1L]] <- agg_dt_perturbed # offset bcoz [[1]] is unperturbed
    }

    # Step 4: Bind list items into data.table, add id_perturbation; cast as alloc params table
    results_dt <-
      final_results |>
      data.table::rbindlist(idcol = "id_perturbation") |>
      as_alloc_params_t()

    message(glue::glue(
      "Successfully computed {nrow(results_dt)} allocation parameter sets ",
      "({nrow(agg_dt)} transitions x ({n_perturbations} perturbations + best estimate))"
    ))

    results_dt
  }
)
