#' Dinamica EGO allocator
#'
#' @description
#' Allocation with [Dinamica EGO](https://dinamicaego.com/)'s expander and patcher
#' (`AllocateTransitions`), run as an external program, see the `install-dinamica` vignette.
#' Each period's inputs are written to a working directory: the anterior map, one probability
#' map per viable transition (the adjusted potentials), the transition rates, and the
#' expansion and patcher tables from `alloc_params_t` (`frac_expander`, `mean_patch_size`,
#' `patch_size_variance`, `patch_isometry`).
#'
#' [check_dinamica()] runs before anything is predicted or written. Dinamica draws its own
#' random seed, so its realisations cannot be replayed (see ethzplus/evoland-plus#104).
#'
#' @param work_dir Character or `NULL`; directory under which each period gets its own working
#'   directory, `run_<id_run>_period_<id_period_post>`. `NULL` (default) uses a temporary
#'   directory.
#' @param keep_intermediate Logical; keep the working directories after each period (default
#'   `FALSE`, remove them).
#'
#' @return An allocator specification for [allocate()].
#'
#' @include allocate.R util_dinamica.R
#' @export
alloc_dinamica_spec <- function(work_dir = NULL, keep_intermediate = FALSE) {
  stopifnot(
    "work_dir must be NULL or a single string" = is.null(work_dir) || is_scalar_character(work_dir),
    "keep_intermediate must be TRUE or FALSE" = is_scalar_logical(keep_intermediate)
  )
  args <- list(work_dir = work_dir, keep_intermediate = keep_intermediate)

  new_allocator_spec(
    name = "Dinamica EGO",
    args = args,
    one_period = function(ctx) alloc_dinamica_one_period(ctx, args),
    check = function() require_dinamica()
  )
}

alloc_dinamica_one_period <- function(ctx, args) {
  work_dir <- ensure_dir(
    if (is.null(args[["work_dir"]])) {
      tempfile("dinamica_")
    } else {
      file.path(args[["work_dir"]], glue::glue("run_{ctx$id_run}_period_{ctx$id_period_post}"))
    }
  )
  if (args[["keep_intermediate"]]) {
    message(glue::glue("  Dinamica working directory kept: {work_dir}"))
  } else {
    on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  }

  grid <- ctx$grid()
  write_dinamica_inputs(ctx, grid, work_dir)

  run_alloc_dinamica(work_dir = work_dir, echo = FALSE, write_logfile = TRUE)

  posterior <- rast_to_grid(grid, terra::rast(file.path(work_dir, "posterior.tif")))
  posterior[ctx$anterior[, .(id_coord)], on = "id_coord"][
    !is.na(value),
    .(id_coord, id_lulc = as.integer(value))
  ]
}

# Write the input files for one Dinamica allocation into work_dir: rates, expansion and
# patcher tables, anterior map and one probability map per viable transition. The tables
# and the probability maps follow the same order of transitions, which is how Dinamica
# matches them up.
write_dinamica_inputs <- function(ctx, grid, work_dir) {
  # one row per viable transition, with its rate and patch parameters
  trans <-
    ctx$alloc_params[ctx$viable_trans[ctx$quota, on = "id_trans"], on = "id_trans"] |>
    data.table::setorder(id_lulc_anterior, id_lulc_posterior)

  trans[, .(`From*` = id_lulc_anterior, `To*` = id_lulc_posterior, Rate = rate)] |>
    data.table::fwrite(file.path(work_dir, "trans_rates.csv"))

  trans[, .(
    `From*` = id_lulc_anterior,
    `To*` = id_lulc_posterior,
    # Dinamica cannot do full expansion or full patching, hence constrained to (0, 1)
    Frac_expander = pmax(1e-6, pmin(1 - 1e-6, frac_expander))
  )] |>
    data.table::fwrite(file.path(work_dir, "expansion_table.csv"))

  trans[, .(
    `From*` = id_lulc_anterior,
    `To*` = id_lulc_posterior,
    Mean_Patch_Size = data.table::fcoalesce(mean_patch_size, 1),
    Patch_Size_Variance = data.table::fcoalesce(patch_size_variance, 0),
    Patch_Isometry = data.table::fcoalesce(patch_isometry, 1)
  )] |>
    data.table::fwrite(file.path(work_dir, "patcher_table.csv"))

  grid_to_rast(grid, ctx$anterior[["id_coord"]], ctx$anterior[["id_lulc"]]) |>
    terra::writeRaster(
      file.path(work_dir, "anterior.tif"),
      overwrite = TRUE,
      datatype = "INT1U",
      NAflag = 255 # Dinamica cannot handle NaN
    )

  prob_map_dir <- ensure_dir(file.path(work_dir, "probability_map_dir"))
  for (i in seq_len(nrow(trans))) {
    pots <- ctx$potentials[id_trans == trans[["id_trans"]][i]]
    grid_to_rast(grid, pots[["id_coord"]], pots[["value"]]) |>
      terra::writeRaster(
        # prefixed with 001, 002, ... so that the files sort like the tables on every
        # filesystem
        file.path(
          prob_map_dir,
          glue::glue("{sprintf('%03d', i)}_id_trans_{trans[['id_trans']][i]}.tif")
        ),
        overwrite = TRUE,
        NAflag = -999 # Dinamica cannot handle NaN
      )
  }

  invisible(work_dir)
}

#' Evaluate allocation parameters by simulating the observed periods
#'
#' @description
#' For every run in `alloc_params_t`, allocate the observed periods from the first observed map
#' with [allocate()], and score the final simulated map against the final observed one with
#' [calc_transition_similarity()] (fuzzy similarity per transition, 11-cell window, exponential
#' decay). The allocated maps are written under each run.
#'
#' @param self An [evoland_db] instance.
#' @param select_score,select_maximize Model selection, see [allocate()].
#' @param allocator An allocator specification, see [allocate()]; Dinamica by default.
#'
#' @return The `alloc_params_t` rows of all runs, with `similarity` filled in.
eval_alloc_params_t <- function(
  self,
  select_score,
  select_maximize,
  allocator = alloc_dinamica_spec()
) {
  historical_periods <- self$periods_t[is_extrapolated == FALSE & id_period > 0]
  # the first period has no anterior period
  posterior_historical_periods <- historical_periods[["id_period"]][-1]
  # the rows of every run, not resolved through a lineage
  all_alloc_params <- as_alloc_params_t(self$get_query("from dl_db.alloc_params_t"))
  runs_required <- unique(all_alloc_params[["id_run"]])

  stopifnot(
    "select_score must be a single string" = is_scalar_character(select_score),
    "select_maximize must be TRUE or FALSE" = is_scalar_logical(select_maximize),
    "need at least 2 historical periods for evaluation" = {
      length(posterior_historical_periods) >= 1L
    },
    "all runs in alloc_params_t must be defined in runs_t" = {
      all(runs_required %in% self$runs_t[["id_run"]])
    }
  )
  allocator$check()

  id_period_initial <- min(historical_periods[["id_period"]])
  id_period_final <- max(historical_periods[["id_period"]])

  message(glue::glue(
    "Evaluating allocation parameters with fuzzy similarity\n",
    "  Runs: {toString(runs_required)}\n",
    "  Periods: {id_period_initial} -> {id_period_final}"
  ))

  lulc_rast <- function(id_run, id_period) {
    self$get_query(
      "select id_coord, id_lulc
       from {self$get_read_expr('lulc_data_t', id_run = id_run)}
       where id_period = {id_period}"
    ) |>
      tabular_to_raster(coords = self$coords_minimal, value_col = "id_lulc")
  }
  rast_initial <- lulc_rast(self$id_run, id_period_initial)
  rast_obs_final <- lulc_rast(self$id_run, id_period_final)
  viable_trans <- self$trans_meta_t[is_viable == TRUE]

  similarity <- lapply(runs_required, function(id_run) {
    message(glue::glue("\n=== Evaluating run {id_run} ==="))
    tryCatch(
      {
        allocate(
          self,
          id_periods = posterior_historical_periods,
          allocator = allocator,
          select_score = select_score,
          select_maximize = select_maximize,
          id_run = id_run
        )
        rast_sim_final <- lulc_rast(id_run, id_period_final)

        message("  Computing per-transition fuzzy similarity...")
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

        data.table::data.table(
          id_run = id_run,
          id_trans = viable_trans[["id_trans"]],
          similarity = unlist(pluck_wildcard(similarities, NA, "similarity"))
        )
      },
      error = function(e) {
        warning(glue::glue("Failed to evaluate run {id_run}: {conditionMessage(e)}"))
        data.table::data.table(
          id_run = id_run,
          id_trans = viable_trans[["id_trans"]],
          similarity = NA_real_
        )
      }
    )
  })

  merge(
    all_alloc_params[, -"similarity"],
    data.table::rbindlist(similarity),
    by = c("id_run", "id_trans"),
    all.x = TRUE
  )
}
