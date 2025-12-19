#' Fuzzy Similarity for Spatial Validation
#'
#' @description
#' Functions to compute fuzzy similarity between categorical maps using
#' a neighborhood-based approach with distance decay. This implements the
#' "similarity of differences" method from Dinamica EGO for validating
#' spatial allocation models with tolerance for spatial displacement.
#'
#' @name fuzzy_similarity
#' @include evoland_db.R
NULL

#' Compute fuzzy neighborhood similarity between two categorical rasters
#'
#' @description
#' Calculates fuzzy similarity indices between two categorical rasters using
#' a moving window approach with optional exponential decay. This allows for
#' spatial tolerance when comparing maps, recognizing that perfect pixel-by-pixel
#' match may be too strict for spatial models.
#'
#' @param map1 SpatRaster, first categorical map
#' @param map2 SpatRaster, second categorical map
#' @param window_size Integer, size of moving window (must be odd). Default 11.
#' @param use_exp_decay Logical, use exponential decay function? Default TRUE.
#' @param decay_divisor Numeric, attenuation factor for exponential decay. Default 2.
#' @param ignore_na Logical, ignore NA values in similarity calculation? Default TRUE.
#'
#' @return List with:
#'   - sim1: SpatRaster showing similarity from map1 to map2
#'   - sim2: SpatRaster showing similarity from map2 to map1
#'   - mean_sim1: Mean similarity from map1 to map2
#'   - mean_sim2: Mean similarity from map2 to map1
#'   - min_similarity: Minimum of the two mean similarities
#'
#' @details
#' The fuzzy similarity is based on Hagen (2003) and uses the concept of
#' fuzziness of location. For each cell, a fuzzy neighborhood vector is
#' computed that represents the membership of each category within a
#' specified window, weighted by distance. The similarity between two maps
#' is then computed as the intersection of their fuzzy vectors.
#'
#' The exponential decay function is: exp(-d / decay_divisor)
#' where d is the Euclidean distance from the center cell.
#'
#' @references
#' Hagen, A. (2003). Fuzzy set approach to assessing similarity of
#' categorical maps. International Journal of Geographical Information
#' Science, 17(3), 235-249.
#'
#' @export
calc_fuzzy_similarity <- function(
  map1,
  map2,
  window_size = 11L,
  use_exp_decay = TRUE,
  decay_divisor = 2.0,
  ignore_na = TRUE
) {
  stopifnot(
    "map1 must be a SpatRaster" = inherits(map1, "SpatRaster"),
    "map2 must be a SpatRaster" = inherits(map2, "SpatRaster"),
    "window_size must be odd" = (window_size %% 2L) == 1L,
    "window_size must be positive" = window_size > 0L,
    "decay_divisor must be positive" = decay_divisor > 0
  )

  # Check dimensions match
  if (!terra::compareGeom(map1, map2, stopOnError = FALSE)) {
    stop("map1 and map2 must have the same dimensions and CRS")
  }

  # Get unique categories (excluding NA)
  cats1 <- terra::unique(map1, na.rm = TRUE)[, 1]
  cats2 <- terra::unique(map2, na.rm = TRUE)[, 1]
  all_cats <- sort(unique(c(cats1, cats2)))

  if (length(all_cats) == 0L) {
    stop("No non-NA categories found in maps")
  }

  # Build distance weight matrix
  weight_matrix <- .build_weight_matrix(window_size, use_exp_decay, decay_divisor)

  # Compute fuzzy neighborhood vectors for each map
  fuzzy1 <- .compute_fuzzy_neighborhoods(map1, all_cats, weight_matrix)
  fuzzy2 <- .compute_fuzzy_neighborhoods(map2, all_cats, weight_matrix)

  # Compute two-way similarity
  sim1_to_2 <- .compute_similarity_map(fuzzy1, fuzzy2, map1, ignore_na)
  sim2_to_1 <- .compute_similarity_map(fuzzy2, fuzzy1, map2, ignore_na)

  # Calculate mean similarities
  mean_sim1 <- terra::global(sim1_to_2, "mean", na.rm = ignore_na)[1, 1]
  mean_sim2 <- terra::global(sim2_to_1, "mean", na.rm = ignore_na)[1, 1]

  list(
    sim1 = sim1_to_2,
    sim2 = sim2_to_1,
    mean_sim1 = mean_sim1,
    mean_sim2 = mean_sim2,
    min_similarity = min(mean_sim1, mean_sim2)
  )
}

#' Build distance weight matrix for fuzzy similarity
#'
#' @param window_size Integer, size of window (must be odd)
#' @param use_exp_decay Logical, use exponential decay?
#' @param decay_divisor Numeric, attenuation factor
#'
#' @return Matrix of weights
#' @keywords internal
.build_weight_matrix <- function(window_size, use_exp_decay, decay_divisor) {
  radius <- (window_size - 1L) / 2L
  center <- radius + 1L

  # Create coordinate matrices
  rows <- seq_len(window_size)
  cols <- seq_len(window_size)

  row_mat <- matrix(rows, nrow = window_size, ncol = window_size)
  col_mat <- matrix(cols, nrow = window_size, ncol = window_size, byrow = TRUE)

  # Calculate distances from center
  dist_mat <- sqrt((row_mat - center)^2 + (col_mat - center)^2)

  if (use_exp_decay) {
    # Exponential decay: exp(-d / A)
    weight_mat <- exp(-dist_mat / decay_divisor)
  } else {
    # Constant weight within window
    weight_mat <- matrix(1, nrow = window_size, ncol = window_size)
  }

  weight_mat
}

#' Compute fuzzy neighborhood vectors for all cells in a map
#'
#' @param rast SpatRaster, categorical map
#' @param categories Vector of category values
#' @param weight_matrix Matrix of distance weights
#'
#' @return List of SpatRasters, one per category, containing fuzzy memberships
#' @keywords internal
.compute_fuzzy_neighborhoods <- function(rast, categories, weight_matrix) {
  window_size <- nrow(weight_matrix)

  # For each category, create a binary raster and apply focal with weights
  fuzzy_layers <- list()

  for (cat in categories) {
    # Create binary layer: 1 where category matches, 0 elsewhere
    binary <- rast == cat

    # Apply focal weighted sum
    # Note: terra::focal normalizes by sum of weights by default
    # We want weighted sum normalized by total weight in neighborhood
    fuzzy_layer <- terra::focal(
      binary,
      w = weight_matrix,
      fun = "sum",
      na.rm = FALSE,
      expand = TRUE
    )

    # Normalize by sum of weights (accounting for edge effects)
    # At edges, fewer cells contribute, so we need to normalize properly
    # Create a mask of non-NA cells
    mask <- !is.na(rast)
    weight_sum <- terra::focal(
      mask,
      w = weight_matrix,
      fun = "sum",
      na.rm = FALSE,
      expand = TRUE
    )

    # Normalize
    fuzzy_layer <- fuzzy_layer / weight_sum

    fuzzy_layers[[as.character(cat)]] <- fuzzy_layer
  }

  fuzzy_layers
}

#' Compute similarity map from fuzzy neighborhoods
#'
#' @param fuzzy_a List of fuzzy neighborhood rasters for map A
#' @param fuzzy_b List of fuzzy neighborhood rasters for map B
#' @param crisp_a SpatRaster, original categorical map A
#' @param ignore_na Logical, ignore NA values?
#'
#' @return SpatRaster of similarity values (0 to 1)
#' @keywords internal
.compute_similarity_map <- function(fuzzy_a, fuzzy_b, crisp_a, ignore_na) {
  # Get categories
  categories <- names(fuzzy_a)

  # For each cell in crisp_a, compute similarity with fuzzy_b
  # S(VA, VB) = sum(min(mA,j, mB,j)) where j iterates over categories

  # Initialize similarity raster
  sim_rast <- crisp_a * 0 # Start with zeros, preserving NA structure

  # For each category, compute min(fuzzy_a, fuzzy_b) and sum
  for (cat in categories) {
    min_membership <- min(fuzzy_a[[cat]], fuzzy_b[[cat]])
    sim_rast <- sim_rast + min_membership
  }

  # Handle NA values
  if (ignore_na) {
    # Keep NA where original map had NA
    sim_rast <- terra::mask(sim_rast, crisp_a, maskvalues = NA)
  }

  sim_rast
}

#' Create change/difference map between two time periods
#'
#' @description
#' Creates a map showing only cells that changed between two time periods.
#' Cells that did not change are set to NA. This is used in the
#' "similarity of differences" approach.
#'
#' @param initial_map SpatRaster, initial time period
#' @param final_map SpatRaster, final time period
#' @param from_class Integer, optional filter for initial class (for specific transition)
#' @param to_class Integer, optional filter for final class (for specific transition)
#'
#' @return SpatRaster showing only changed cells. For general changes, values
#'   are the final class. For specific transitions, values indicate presence (1)
#'   or absence (NA) of that transition.
#'
#' @export
create_change_map <- function(
  initial_map,
  final_map,
  from_class = NULL,
  to_class = NULL
) {
  stopifnot(
    "initial_map must be a SpatRaster" = inherits(initial_map, "SpatRaster"),
    "final_map must be a SpatRaster" = inherits(final_map, "SpatRaster")
  )

  if (!terra::compareGeom(initial_map, final_map, stopOnError = FALSE)) {
    stop("initial_map and final_map must have the same dimensions and CRS")
  }

  if (!is.null(from_class) && !is.null(to_class)) {
    # Specific transition: mark cells that transitioned from -> to
    change_map <- terra::ifel(
      initial_map == from_class & final_map == to_class,
      1L, # Mark as 1 where transition occurred
      NA # NA elsewhere
    )
  } else if (is.null(from_class) && is.null(to_class)) {
    # General changes: show final class where changed, NA where unchanged
    change_map <- terra::ifel(
      initial_map != final_map,
      final_map,
      NA
    )
  } else {
    stop("Both from_class and to_class must be specified, or both must be NULL")
  }

  change_map
}

#' Compute fuzzy similarity of differences for transition validation
#'
#' @description
#' Implements the "similarity of differences" approach from Dinamica EGO.
#' Compares the spatial pattern of a specific transition in observed vs
#' simulated maps, allowing for spatial tolerance.
#'
#' @param initial_map SpatRaster, initial LULC state
#' @param observed_map SpatRaster, observed final LULC state
#' @param simulated_map SpatRaster, simulated final LULC state
#' @param from_class Integer, initial class of transition
#' @param to_class Integer, final class of transition
#' @param window_size Integer, size of moving window (must be odd). Default 11.
#' @param use_exp_decay Logical, use exponential decay? Default TRUE.
#' @param decay_divisor Numeric, attenuation factor. Default 2.
#'
#' @return List with:
#'   - observed_change: SpatRaster of observed changes
#'   - simulated_change: SpatRaster of simulated changes
#'   - similarity: Minimum fuzzy similarity between change maps
#'   - sim_obs_to_sim: Mean similarity from observed to simulated
#'   - sim_sim_to_obs: Mean similarity from simulated to observed
#'   - n_observed: Number of cells with observed transition
#'   - n_simulated: Number of cells with simulated transition
#'
#' @export
calc_transition_similarity <- function(
  initial_map,
  observed_map,
  simulated_map,
  from_class,
  to_class,
  window_size = 11L, # TODO insert warning if domain is too small for window
  use_exp_decay = TRUE,
  decay_divisor = 2.0
) {
  # Create change maps for the specific transition
  obs_change <- create_change_map(initial_map, observed_map, from_class, to_class)
  sim_change <- create_change_map(initial_map, simulated_map, from_class, to_class)

  # Count transitions
  n_obs <- terra::global(!is.na(obs_change), "sum", na.rm = FALSE)[1, 1]
  n_sim <- terra::global(!is.na(sim_change), "sum", na.rm = FALSE)[1, 1]

  # If no transitions in either map, return NA similarity
  if (n_obs == 0 && n_sim == 0) {
    return(list(
      observed_change = obs_change,
      simulated_change = sim_change,
      similarity = NA_real_,
      sim_obs_to_sim = NA_real_,
      sim_sim_to_obs = NA_real_,
      n_observed = 0,
      n_simulated = 0
    ))
  }

  # For similarity calculation, we need the maps to have the same categories
  # Use binary representation: 1 where transition occurred, 0 elsewhere
  # (replacing NA with 0 for similarity calculation)
  obs_binary <- terra::ifel(is.na(obs_change), 0L, 1L)
  sim_binary <- terra::ifel(is.na(sim_change), 0L, 1L)

  # Compute fuzzy similarity
  similarity <- calc_fuzzy_similarity(
    obs_binary,
    sim_binary,
    window_size = window_size,
    use_exp_decay = use_exp_decay,
    decay_divisor = decay_divisor,
    ignore_na = FALSE # We converted NA to 0
  )

  list(
    observed_change = obs_change,
    simulated_change = sim_change,
    similarity = similarity$min_similarity,
    sim_obs_to_sim = similarity$mean_sim1,
    sim_sim_to_obs = similarity$mean_sim2,
    similarity_map = similarity$sim1, # Can be used for visualization
    n_observed = n_obs,
    n_simulated = n_sim
  )
}
