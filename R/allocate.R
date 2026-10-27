#' Allocate land use change
#'
#' @description
#' *Allocation* places the projected quantity of change on the map: for each period, it turns
#' the land use at the start of the period (the *anterior* map) into the land use at its end
#' (the *posterior* map), given the transition potentials and the demanded transition rates.
#'
#' `allocate()` runs a contiguous sequence of periods. For each period it
#'
#' 1. predicts the transition potentials if `trans_pot_t` does not hold them yet, see
#'    [predict_trans_pot()];
#' 2. builds the allocation context: the anterior map, the allocation-ready potentials
#'    ([adjusted_trans_pot_v()]), the viable transitions, the quantity of change per
#'    transition and the allocation parameters;
#' 3. hands the context to the allocator, which returns the posterior map;
#' 4. commits the map to `lulc_data_t`, together with the neighbor predictors the next period
#'    is predicted from, in one transaction.
#'
#' `allocate_one_period()` runs steps 1-3 for one period and returns the map without
#' committing it. Use it to write your own loop: edit `trans_pot_t` between prediction and
#' allocation (e.g. interventions), edit the allocated map afterwards, or draw an ensemble of
#' single-period realisations. Commit the map with `db$commit(x, "lulc_data_t", method =
#' "upsert")` and, if a later period follows, run `db$upsert_new_neighbors(id_period_post)`.
#'
#' The allocator is one of [alloc_clumpy_spec()] (stochastic, unbiased; the default),
#' [alloc_greedy_spec()] (deterministic rank-and-fill) or [alloc_dinamica_spec()] (Dinamica
#' EGO's expander and patcher).
#'
#' The quantity of change is the same for every allocator: each viable transition changes
#' `round(rate * n_anterior)` cells, where `rate` is the transition's rate in `trans_rates_t`
#' for the posterior period and `n_anterior` the number of cells of its anterior class in the
#' anterior map. Transitions without a rate change no cells.
#'
#' @param db An [evoland_db] instance.
#' @param id_periods Integer vector of contiguous posterior period IDs to simulate.
#' @param id_period_post Integer posterior period ID.
#' @param allocator An allocator specification, see Description.
#' @param select_score Character; mlr3 measure ID (e.g. `"classif.auc"`) used to select the
#'   model for prediction, see [predict_trans_pot()].
#' @param select_maximize Logical; maximize (`TRUE`) or minimize (`FALSE`) `select_score`.
#' @param id_run Integer; the run to allocate. It reads its anterior map, demand and
#'   allocation parameters through its lineage, and the allocated maps are written under it.
#' @param potentials_from Integer; the run whose transition potentials are used, predicted
#'   under that run if missing. Pass the parent run to let sibling runs (e.g. a Monte Carlo
#'   ensemble) share one set of potentials. It can differ from `id_run` for a single period
#'   only: from the second period on, the anterior map is the run's own allocation, which
#'   another run cannot see.
#' @param force_predict Logical; if `TRUE`, predict the transition potentials even if
#'   `trans_pot_t` already holds them for `potentials_from` and the period.
#' @param update_neighbors Logical; whether to recompute the neighbor predictors after the last
#'   period (default `TRUE`). Intermediate periods are always updated, since the next period's
#'   prediction reads them. Set `FALSE` when nothing is allocated onwards from the last period,
#'   e.g. for single-period ensembles.
#'
#' @return `allocate()`: called for its side effects on `lulc_data_t` and `pred_data_t`;
#'   returns `NULL` invisibly. `allocate_one_period()`: an [lulc_data_t] with the posterior
#'   map.
#'
#' @name allocate
#' @include evoland_db.R
NULL

#' @rdname allocate
#' @export
allocate <- function(
  db,
  id_periods,
  allocator = alloc_clumpy_spec(),
  select_score,
  select_maximize,
  id_run = db$id_run,
  potentials_from = id_run,
  force_predict = FALSE,
  update_neighbors = TRUE
) {
  stopifnot(
    "id_periods must be integers" = is_integerish(id_periods),
    "id_periods must be contiguous" = all(diff(id_periods) == 1L),
    "id_periods must be in periods_t" = all(id_periods %in% db$periods_t[["id_period"]]),
    "potentials_from can differ from id_run for a single period only" = {
      length(id_periods) == 1L || identical(as.integer(potentials_from), as.integer(id_run))
    }
  )
  check_allocation_args(allocator, id_run, potentials_from)

  message(glue::glue(
    "Allocating with {allocator$name}: run {id_run}, ",
    "periods {paste(id_periods, collapse = ' -> ')}"
  ))

  for (id_period_post in id_periods) {
    posterior <- allocate_one_period(
      db = db,
      id_period_post = id_period_post,
      allocator = allocator,
      select_score = select_score,
      select_maximize = select_maximize,
      id_run = id_run,
      potentials_from = potentials_from,
      force_predict = force_predict
    )

    # the map and the neighbor counts derived from it land together or not at all
    db$transaction({
      db$commit(posterior, "lulc_data_t", method = "upsert")
      if (update_neighbors || id_period_post != max(id_periods)) {
        db$upsert_new_neighbors(id_period_post, id_run = id_run)
      }
    })
  }

  invisible(NULL)
}

#' @rdname allocate
#' @export
allocate_one_period <- function(
  db,
  id_period_post,
  allocator = alloc_clumpy_spec(),
  select_score,
  select_maximize,
  id_run = db$id_run,
  potentials_from = id_run,
  force_predict = FALSE
) {
  stopifnot(
    "id_period_post must be a single integer" = is_scalar_integerish(id_period_post)
  )
  check_allocation_args(allocator, id_run, potentials_from)

  db$predict_trans_pot(
    id_period_post = id_period_post,
    select_score = select_score,
    select_maximize = select_maximize,
    force = force_predict,
    id_run = potentials_from
  )

  ctx <- allocation_context(db, id_period_post, id_run, potentials_from)

  message(glue::glue(
    "  {allocator$name}: period {id_period_post - 1L} -> {id_period_post}"
  ))
  posterior <- allocator$one_period(ctx)

  n_changed <- ctx$anterior[posterior, on = "id_coord", sum(id_lulc != i.id_lulc)]
  message(glue::glue("  Changed {n_changed} of {nrow(posterior)} cells"))

  posterior[, .(id_run = id_run, id_coord, id_lulc, id_period = id_period_post)] |>
    as_lulc_data_t()
}

check_allocation_args <- function(allocator, id_run, potentials_from) {
  stopifnot(
    "allocator must be an allocator specification, e.g. alloc_clumpy_spec()" = {
      inherits(allocator, "allocator_spec")
    },
    "id_run must be a single integer" = is_scalar_integerish(id_run),
    "potentials_from must be a single integer" = is_scalar_integerish(potentials_from)
  )
  # e.g. Dinamica's setup, before anything is predicted or written
  allocator$check()
  invisible(NULL)
}

# Everything an allocator reads, gathered in one place. Allocators get this list and never
# touch the database themselves. `grid()` is the only part that is computed on demand, since
# only the raster-based allocators (CLUMPY, Dinamica) need it.
allocation_context <- function(db, id_period_post, id_run, potentials_from) {
  id_period_anterior <- id_period_post - 1L

  anterior <- db$get_query(
    "select id_coord, id_lulc
     from {db$get_read_expr('lulc_data_t', id_run = id_run)}
     where id_period = {id_period_anterior}
     order by id_coord"
  )
  if (nrow(anterior) == 0L) {
    stop(
      glue::glue("No LULC data for run {id_run} in the anterior period {id_period_anterior}"),
      call. = FALSE
    )
  }

  viable_trans <-
    db$trans_meta_t[is_viable == TRUE, .(id_trans, id_lulc_anterior, id_lulc_posterior)] |>
    data.table::setorder(id_trans)
  stopifnot("No viable transitions found" = nrow(viable_trans) > 0L)

  rates <- db$get_query(
    "select id_trans, rate
     from {db$get_read_expr('trans_rates_t', id_run = id_run)}
     where id_period = {id_period_post}"
  )

  # one quantity of change for every allocator: the rate's share of the anterior class
  quota <-
    rates[viable_trans, on = "id_trans"][
      anterior[, .(n_anterior = .N), by = .(id_lulc_anterior = id_lulc)],
      on = "id_lulc_anterior"
    ][
      viable_trans[, .(id_trans)],
      on = "id_trans"
    ][, `:=`(
      rate = data.table::fcoalesce(rate, 0),
      n_anterior = data.table::fcoalesce(n_anterior, 0L)
    )][, .(id_trans, rate, n_anterior, quota = as.integer(round(rate * n_anterior)))]

  potentials <- db$adjusted_trans_pot_v(
    id_period_post,
    id_run = id_run,
    id_run_trans_pot = potentials_from
  )

  alloc_params <- db$get_query(
    "from {db$get_read_expr('alloc_params_t', id_run = id_run)}"
  )

  grid <- local({
    cached <- NULL
    function() {
      if (is.null(cached)) {
        cached <<- coords_grid(db$coords_minimal)
      }
      cached
    }
  })

  list(
    id_run = id_run,
    id_period_post = id_period_post,
    anterior = anterior,
    viable_trans = viable_trans,
    quota = quota,
    potentials = potentials,
    alloc_params = alloc_params,
    grid = grid
  )
}

# A raster template covering coords and the raster cell of every coordinate point, for the
# allocators that work on rasters
coords_grid <- function(coords) {
  template <- coords_rast_template(coords)
  list(
    template = template,
    cells = data.table::data.table(
      id_coord = coords[["id_coord"]],
      cell = terra::cellFromXY(template, as.matrix(coords[, .(lon, lat)]))
    )
  )
}

# Write values given per coordinate point into a raster on the grid; cells without a
# coordinate point, or without a value, are NA
grid_to_rast <- function(grid, id_coord, value) {
  values <- rep(NA_real_, terra::ncell(grid[["template"]]))
  values[grid[["cells"]][["cell"]][match(id_coord, grid[["cells"]][["id_coord"]])]] <- value
  out <- terra::rast(grid[["template"]])
  terra::values(out) <- values
  out
}

# Read a raster on the grid back into one value per coordinate point
rast_to_grid <- function(grid, x) {
  grid[["cells"]][, .(id_coord, value = terra::values(x, mat = FALSE)[cell])]
}

#' Allocator specifications
#'
#' @description
#' An allocator specification names an allocation algorithm and carries its arguments; pass it
#' to [allocate()]. The arguments are checked when the specification is created, so that a
#' mistake surfaces before anything is predicted or written.
#'
#' @details
#' A specification is a list of class `allocator_spec` with
#' - `name`: a label for messages;
#' - `args`: the arguments it was created with;
#' - `check()`: called by [allocate()] before anything else, e.g. to check that an external
#'   program is installed;
#' - `one_period(ctx)`: takes the allocation context [allocate()] builds and returns the
#'   posterior map as a data.table with `id_coord` and `id_lulc`, one row per row of
#'   `ctx$anterior`.
#'
#' @param name Character, label for messages.
#' @param args Named list, the specification's arguments.
#' @param one_period Function of the allocation context, see Details.
#' @param check Function without arguments, see Details.
#' @param x An allocator specification.
#' @param ... Ignored.
#'
#' @name allocator_spec
#' @keywords internal
new_allocator_spec <- function(name, args, one_period, check = function() invisible(NULL)) {
  stopifnot(
    "name must be a single string" = is_scalar_character(name),
    "args must be a list" = is.list(args),
    "one_period must be a function" = is.function(one_period),
    "check must be a function" = is.function(check)
  )
  structure(
    list(name = name, args = args, one_period = one_period, check = check),
    class = "allocator_spec"
  )
}

#' @rdname allocator_spec
#' @export
print.allocator_spec <- function(x, ...) {
  cat("Allocator:", x[["name"]], "\n")
  for (arg in names(x[["args"]])) {
    cat("  ", arg, ": ", deparse1(x[["args"]][[arg]]), "\n", sep = "")
  }
  invisible(x)
}
