#' Synthetic land use with a known change process
#'
#' @description
#' Simulates a small landscape and its land use over several periods, driven by a stated
#' change process. It is meant for tutorials, examples and tests: unlike real data or random
#' noise, the change has a signal a model can learn, and the process that generated it is
#' known.
#'
#' There are four classes: 1 forest, 2 arable land, 3 urban areas, 4 a lake that never
#' changes. Two latent drivers shape both the initial map and its change:
#'
#' - `accessibility`, high towards the west and south, plus a smooth random field;
#' - `site_quality`, high towards the north and east, plus a smooth random field.
#'
#' In the initial map, urban land clusters where accessibility is high, the lake sits in a
#' random patch, forest on the better sites and arable land takes the rest. Each later
#' period applies four transitions, whose per-period probabilities depend on the drivers and
#' on the share of each class in the 5 x 5 cell neighbourhood:
#'
#' | transition       | logit of the per-period probability                                    |
#' | ---------------- | ---------------------------------------------------------------------- |
#' | arable -> urban  | -5.5 + 7 * share_urban + 3 * accessibility                             |
#' | forest -> urban  | -7 + 6 * share_urban + 3 * accessibility                               |
#' | forest -> arable | -5 + 4 * (1 - site_quality) + 3 * share_arable                         |
#' | arable -> forest | -6 + 4 * site_quality * (1 - accessibility) + 3 * share_forest         |
#'
#' A cell undergoes at most one transition per period; the transitions are drawn in the
#' order of the table. A third, `random_nuisance` field has no influence on the change; it is
#' returned as a predictor a model should learn to ignore.
#'
#' The process is the one behind Fig. 2 of the evoland-plus model description paper. All
#' random draws go through R's random number generator, so [set.seed()] makes a landscape
#' replay.
#'
#' @param n_periods Integer, the number of maps: the initial map and `n_periods - 1` steps of
#'   the process.
#' @param n_cells Integer, cells per side of the square grid.
#' @param resolution Numeric, cell size in the units of `crs`.
#' @param xmin,ymin Numeric, the grid's lower left corner in the units of `crs`.
#' @param crs Character, the grid's coordinate reference system.
#'
#' @return A list of two [terra::SpatRaster]s on the same grid:
#'   - `lulc`: one layer per period, named `id_period=1`, `id_period=2`, ..., holding the
#'     class of each cell;
#'   - `drivers`: the layers `accessibility`, `site_quality` and `random_nuisance`, in
#'     \[0, 1\].
#'
#' @examples
#' set.seed(1)
#' landscape <- synthetic_landscape(n_periods = 3, n_cells = 30)
#' terra::plot(landscape$lulc, nc = 3)
#' terra::plot(landscape$drivers, nc = 3)
#'
#' @export
synthetic_landscape <- function(
  n_periods = 3L,
  n_cells = 30L,
  resolution = 100,
  xmin = 2697000,
  ymin = 1252000,
  crs = "EPSG:2056"
) {
  stopifnot(
    "n_periods must be a positive integer" = is_scalar_integerish(n_periods) && n_periods >= 1,
    "n_cells must be an integer of at least 5" = is_scalar_integerish(n_cells) && n_cells >= 5,
    "resolution must be a positive number" = is_scalar_numeric(resolution) && resolution > 0
  )

  template <- terra::rast(
    crs = crs,
    extent = terra::ext(
      xmin,
      xmin + n_cells * resolution,
      ymin,
      ymin + n_cells * resolution
    ),
    resolution = resolution
  )

  drivers <- synthetic_drivers(template)
  maps <- list(synthetic_initial_map(template, drivers))
  for (i in seq_len(n_periods - 1L)) {
    maps[[i + 1L]] <- synthetic_step(maps[[i]], drivers)
  }

  list(
    lulc = stats::setNames(terra::rast(maps), paste0("id_period=", seq_len(n_periods))),
    drivers = stats::setNames(
      terra::rast(drivers),
      c("accessibility", "site_quality", "random_nuisance")
    )
  )
}

scale01 <- function(x) {
  range <- terra::global(x, c("min", "max"), na.rm = TRUE)
  (x - range[1, 1]) / (range[1, 2] - range[1, 1])
}

smooth_field <- function(template, w) {
  terra::setValues(template, stats::rnorm(terra::ncell(template))) |>
    terra::focal(w = w, fun = mean, na.rm = TRUE) |>
    scale01()
}

synthetic_drivers <- function(template) {
  xy <- terra::crds(template, df = TRUE)
  x_grad <- terra::setValues(template, (xy$x - min(xy$x)) / (max(xy$x) - min(xy$x)))
  y_grad <- terra::setValues(template, (xy$y - min(xy$y)) / (max(xy$y) - min(xy$y)))

  list(
    accessibility = scale01(
      0.55 * (1 - x_grad) + 0.25 * (1 - y_grad) + 0.20 * smooth_field(template, w = 9)
    ),
    site_quality = scale01(
      0.50 * y_grad + 0.35 * smooth_field(template, w = 5) + 0.15 * x_grad
    ),
    random_nuisance = smooth_field(template, w = 3)
  )
}

synthetic_initial_map <- function(template, drivers) {
  quantile_of <- function(x, p) stats::quantile(terra::values(x), p, na.rm = TRUE)
  urban_score <- scale01(0.7 * drivers$accessibility + 0.3 * smooth_field(template, w = 3))
  lake_score <- smooth_field(template, w = 7)
  forest_score <- scale01(0.6 * smooth_field(template, w = 5) + 0.4 * drivers$site_quality)

  terra::ifel(
    urban_score > quantile_of(urban_score, 0.88),
    3,
    terra::ifel(
      lake_score > quantile_of(lake_score, 0.97),
      4,
      terra::ifel(forest_score > quantile_of(forest_score, 0.45), 1, 2)
    )
  )
}

# the per-period probability of each transition, in the order synthetic_step() draws them
synthetic_transitions <- function(map, drivers) {
  share <- function(class) terra::focal(map == class, w = 5, fun = mean, na.rm = TRUE)
  plogis_rast <- function(x) 1 / (1 + exp(-x))
  acc <- drivers$accessibility
  sq <- drivers$site_quality

  list(
    list(from = 2, to = 3, p = plogis_rast(-5.5 + 7 * share(3) + 3 * acc)),
    list(from = 1, to = 3, p = plogis_rast(-7 + 6 * share(3) + 3 * acc)),
    list(from = 1, to = 2, p = plogis_rast(-5 + 4 * (1 - sq) + 3 * share(2))),
    list(from = 2, to = 1, p = plogis_rast(-6 + 4 * sq * (1 - acc) + 3 * share(1)))
  )
}

# One period of the process: a single uniform draw per cell is compared against the
# cumulative probabilities of its class's transitions, so a cell changes at most once
synthetic_step <- function(map, drivers) {
  anterior <- terra::values(map, mat = FALSE)
  posterior <- anterior
  draw <- stats::runif(length(anterior))
  cumulative <- numeric(length(anterior))

  for (transition in synthetic_transitions(map, drivers)) {
    p <- terra::values(transition$p, mat = FALSE)
    at_risk <- anterior == transition$from
    flips <- at_risk & draw >= cumulative & draw < cumulative + p & posterior == anterior
    posterior[flips] <- transition$to
    cumulative[at_risk] <- cumulative[at_risk] + p[at_risk]
  }

  terra::setValues(map, posterior)
}
