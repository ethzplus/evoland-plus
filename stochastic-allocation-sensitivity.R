## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: cleanup
#| include: false
unlink("stochastic-sensitivity.evolanddb", recursive = TRUE)
set.seed(666)
knitr::knit_hooks$set(seed = function(before, options, envir) {
  if (before && !is.null(options$seed)) {
    set.seed(options$seed)
  }
})
local({
  chunk_messages <- character()
  knitr::knit_hooks$set(message = function(x, options) {
    chunk_messages <<- c(chunk_messages, x)
    return(character(0))
  })
  default_chunk_hook <- knitr::knit_hooks$get("chunk")
  knitr::knit_hooks$set(chunk = function(x, options) {
    out <- default_chunk_hook(x, options)
    if (length(chunk_messages) > 0) {
      msg_text <- paste(chunk_messages, collapse = "")
      callout <- paste0(
        "\n\n::: {.callout-note collapse='true'}\n## Messages\n",
        msg_text,
        "\n:::\n"
      )
      chunk_messages <<- character()
      out <- paste0(out, callout)
    }
    return(out)
  })
})


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: setup
#| output: false
library(evoland)
library(data.table)
library(terra)
# library(lubridate)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: create-db
db <- evoland_db$new(path = "stochastic-sensitivity.evolanddb")
db


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: define-lulc-meta
db$lulc_meta_t <- create_lulc_meta_t(
  list(
    forest = list(
      pretty_name = "Forest",
      description = "Areas with lots of trees",
      src_classes = 1:3
    ),
    arable = list(
      pretty_name = "Arable Land",
      src_classes = c(4, 8)
    ),
    urban = list(
      pretty_name = "Urban Areas",
      description = "Where nature goes to die",
      src_classes = 5:7
    ),
    static = list(
      pretty_name = "Immutable",
      description = "Areas where we cannot conceptualize change",
      src_classes = 9:10
    )
  )
)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: define-spatial-domain
template_rast <- terra::rast(
  crs = "EPSG:2056",
  extent = terra::ext(c(
    xmin = 2697000,
    xmax = 2700000,
    ymin = 1252000,
    ymax = 1255000
  )),
  resolution = 100
)

db$coords_t <- create_coords_t_square(
  epsg = terra::crs(template_rast, describe = TRUE)$code |> as.integer(),
  extent = terra::ext(template_rast),
  resolution = terra::res(template_rast)[1]
)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: define-periods
db$periods_t <- create_periods_t(
  period_length_str = "P10Y",
  start_observed = "1995-01-01",
  end_observed = "2020-01-01",
  end_extrapolated = "2030-01-01"
)

db$periods_t


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| seed: 123
#| label: synthesize-lulc
#| fig-asp: 0.3
n_cells <- dim(template_rast)[1] * dim(template_rast)[2]
noise1 <- runif(n_cells, min = 0, max = 10)
noise2 <- noise1 + stats::rpois(n_cells, 0.2) - stats::rpois(n_cells, 0.2)
noise3 <- noise2 + stats::rpois(n_cells, 0.2) - stats::rpois(n_cells, 0.2)

synthetic_lulc <- rast(template_rast, nlyrs = 3, vals = c(noise1, noise2, noise3)) |>
  focal(w = 3, fun = mean, na.rm = TRUE) |>
  clamp(lower = 0, upper = 10) |>
  classify(
    rcl = data.frame(
      from = 0:9,
      to = 1:10,
      becomes = c(3, 7, 1, 10, 5, 8, 2, 9, 4, 6)
    )
  )

plot(synthetic_lulc, nc = 3)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: ingest-lulc
synthetic_at_coords <- extract_using_coords_t(synthetic_lulc, db$coords_t)

synthetic_joint_meta <- synthetic_at_coords[,
  .(
    id_coord,
    id_period = substr(layer, 4, 4) |> as.integer(),
    src_class = value
  )
][
  db$lulc_meta_long_v,
  on = .(src_class),
  nomatch = NULL
]

db$lulc_data_t <- as_lulc_data_t(
  synthetic_joint_meta[, .(
    id_run = 0L,
    id_period,
    id_lulc,
    id_coord
  )]
)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: predictors
db$pred_meta_t <- evoland:::test_pred_meta_t
db$pred_data_t <- evoland:::test_pred_data_t

db$set_neighbors(
  max_distance = 1000,
  distance_breaks = c(0, 100, 500, 1000),
  quiet = TRUE
)
db$generate_neighbor_predictors()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: transitions-and-predictors
#| seed: 666
db$trans_meta_t <- create_trans_meta_t(db$trans_v, min_cardinality_abs = 20)

db$set_full_trans_preds()

trans_pred_scored <- db$get_pred_filter_score(
  filter = mlr3filters::FilterImportance$new(
    learner = mlr3::lrn("classif.rpart")
  )
)

db$trans_preds_t <- trans_pred_scored[
  importance > 5 | is.na(importance)
]


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: fit-models
#| seed: 666
db$trans_models_t <- db$fit_partial_models(
  learner = mlr3::lrn("classif.featureless"),
  measures = c("classif.auc", "classif.acc"),
  sample_frac = 0.7,
  seed = 666
)

db$trans_models_t <- db$fit_partial_models(
  learner = mlr3::lrn("classif.rpart"),
  measures = c("classif.auc", "classif.acc"),
  sample_frac = 0.7,
  seed = 666
)

db$trans_models_t <- db$fit_full_models(
  select_score = "classif.auc",
  select_maximize = TRUE
)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: transition-rates
#| seed: 666
db$trans_rates_t <- db$get_obs_trans_rates() |>
  extrapolate_trans_rates(
    periods = db$periods_t,
    coord_count = n_cells
  )

alloc_base <- db$create_alloc_params_t(n_perturbations = 0)
alloc_base[, id_run := 0L]


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: register-runs

n_realizations <- 30L
seed_start <- 1000L
runs_new <- data.table(
  id_run = seq_len(n_realizations + 1) - 1L,
  parent_id_run = NA_integer_,
  description = "base",
  kind = "synthetic 'historical' case",
  seed = 666L
)
runs_new[id_run > 0, id_run := seq_len(n_realizations)]
runs_new[id_run > 0, parent_id_run := 0L]
runs_new[id_run > 0, description := paste("stochastic allocation", id_run)]
runs_new[id_run > 0, kind := "allocation_sensitivity"]
runs_new[id_run > 0, seed := id_run + seed_start]

db$commit(as_runs_t(runs_new), "runs_t", method = "overwrite")
runs_out <- db$runs_t[id_run > 0]

run_ids <- runs_out$id_run
run_seeds <- runs_out$seed

db$runs_t[id_run %in% run_ids]


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: copy-alloc-params-to-runs
alloc_params_runs <- rbindlist(lapply(run_ids, function(i) {
  copy(alloc_base)[, id_run := i]
}))

db$alloc_params_t <- alloc_params_runs


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: identify-periods
last_observed_id <- db$periods_t[
  id_period != 0 & is_extrapolated == FALSE,
  max(id_period)
]

first_extrapolated_id <- db$periods_t[
  is_extrapolated == TRUE,
  min(id_period)
]

last_observed_id
first_extrapolated_id


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: allocation-wrapper
alloc_one_run <- function(db, id_run, id_period, seed) {
  alloc_formals <- names(formals(db$alloc_clumpy))
  alloc_args <- list(
    id_period = id_period,
    select_score = "classif.auc",
    select_maximize = TRUE,
    seed = seed
  )
  if ("id_run" %in% alloc_formals) {
    alloc_args$id_run <- id_run
  }
  do.call(db$alloc_clumpy, alloc_args)
}


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: run-30-realizations
#| results: false
invisible(lapply(seq_along(run_ids), function(i) {
  alloc_one_run(
    db = db,
    id_run = run_ids[i],
    id_period = first_extrapolated_id,
    seed = run_seeds[i]
  )
}))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: summarize-change-frequency
observed_last <- db$lulc_data_t[
  id_run == 0L & id_period == last_observed_id,
  .(id_coord, id_lulc_observed = id_lulc)
]

simulated_extrap <- db$lulc_data_t[
  id_run %in% run_ids & id_period == first_extrapolated_id,
  .(id_run, id_coord, id_lulc_simulated = id_lulc)
]

change_frequency <- simulated_extrap[
  observed_last,
  on = .(id_coord)
][,
  .(
    n_changed = sum(id_lulc_simulated != id_lulc_observed),
    p_changed = mean(id_lulc_simulated != id_lulc_observed)
  ),
  by = .(id_coord)
]

change_frequency


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: helper-rasterize
as_coord_raster <- function(values_dt, value_col, template = template_rast) {
  raster_values <- db$coords_t[
    values_dt,
    on = .(id_coord)
  ]
  vect_obj <- terra::vect(
    raster_values[, .(lon, lat, value = get(value_col))],
    geom = c("lon", "lat"),
    crs = terra::crs(template)
  )
  terra::rasterize(vect_obj, template, field = "value")
}

change_frequency_rast <- as_coord_raster(change_frequency, "p_changed")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: plot-change-frequency
#| fig-width: 7
#| fig-height: 6
plot(
  change_frequency_rast,
  main = "Share of runs in which a cell changes class",
  col = hcl.colors(20, palette = "YlOrRd", rev = FALSE),
  range = c(0, 1)
)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: helper-sample-realizations
sample_runs <- run_ids[c(1, 2, 3, 4)]

make_lulc_raster_for_run <- function(id_run_value, id_period_value) {
  values_dt <- db$lulc_data_t[
    id_run == id_run_value & id_period == id_period_value,
    .(id_coord, value = id_lulc)
  ]
  values_joined <- db$coords_t[values_dt, on = .(id_coord)]
  vect_obj <- terra::vect(
    values_joined[, .(lon, lat, value)],
    geom = c("lon", "lat"),
    crs = terra::crs(template_rast)
  )
  terra::rasterize(vect_obj, template_rast, field = "value")
}

sample_rasters <- rast(lapply(sample_runs, function(i) {
  make_lulc_raster_for_run(i, first_extrapolated_id)
}))

sample_rasters <- categories(
  sample_rasters,
  layer = 0,
  value = data.frame(
    id = db$lulc_meta_t$id_lulc,
    name = db$lulc_meta_t$pretty_name
  )
)

names(sample_rasters) <- sprintf("run %02d", sample_runs)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: plot-sample-realizations
#| fig-asp: 0.8
plot(sample_rasters)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#| label: urban-frequency
urban_id <- db$lulc_meta_t[pretty_name == "Urban Areas", id_lulc]

urban_frequency <- simulated_extrap[,
  .(
    p_urban = mean(id_lulc_simulated == urban_id)
  ),
  by = .(id_coord)
]

urban_frequency_rast <- as_coord_raster(urban_frequency, "p_urban")

plot(
  urban_frequency_rast,
  main = "Share of runs in which a cell ends up urban",
  col = hcl.colors(20, palette = "Reds", rev = FALSE),
  range = c(0, 1)
)
