#' Calculate Transition Potential
#'
#' Estimate transition potential at `id_period_post`. Based on the LULC at `id_period_anterior`
#'
#' @name trans_pot_t
#'
#' @param x A list or data.frame coercible to a data.table
#' @param id_period_post Integer, period to estimate transition potential to
#'
#' @return A data.table of class "trans_pot_t" with columns:
#'   - `id_trans`: Foreign key to [trans_meta_t()]
#'   - `id_period_post`: Foreign key to [periods_t()]
#'   - `id_coord`: Foreign key to [coords_t()]
#'   - `value`: Map of model (hyper) parameters
#' @export
as_trans_pot_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_run = integer(0),
      id_trans = integer(0),
      id_period_post = integer(0),
      id_coord = integer(0),
      value = numeric(0)
    )
  }

  data.table::setDT(x) |>
    cast_dt_col("id_run", "int") |>
    cast_dt_col("id_trans", "int") |>
    cast_dt_col("id_period_post", "int") |>
    cast_dt_col("id_coord", "int")

  as_parquet_db_t(
    x,
    class_name = "trans_pot_t",
    key_cols = c("id_trans", "id_period_post", "id_coord"),
    partition_cols = "id_run"
  )
}

#' @export
validate.trans_pot_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_run",
      "id_trans",
      "id_period_post",
      "id_coord",
      "value"
    )
  )

  # Skip soft checks if empty
  if (nrow(x) == 0L) {
    return(x)
  }

  stopifnot(
    is.integer(x[["id_run"]]),
    is.integer(x[["id_trans"]]),
    is.integer(x[["id_period_post"]]),
    is.integer(x[["id_coord"]]),
    all(x[["value"]] >= 0),
    all(x[["value"]] <= 1)
  )

  return(x)
}

#' @describeIn trans_pot_t Print a trans_pot_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
#' @export
print.trans_pot_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_trans <- data.table::uniqueN(x[["id_trans"]])
    n_periods <- data.table::uniqueN(x[["id_period_post"]])
    n_runs <- data.table::uniqueN(x[["id_run"]])

    cat(glue::glue(
      "Transition Potential Table\n",
      "Rows: {nrow(x)}\n",
      "Transitions: {n_trans}, Periods: {n_periods}, Runs: {n_runs}\n\n"
    ))
  } else {
    cat("Transition Potential Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}


#' @describeIn trans_pot_t For each viable transition in current `id_run`, predict the raw
#' transition potential for a given period and store it in `trans_pot_t` in the database. Raw
#' potentials are per-transition MLR3 model probabilities; they are **not** yet allocation-ready
#' (not column-scaled to target rates, not row-closed to max probability of 1). Use
#' [adjusted_trans_pot_v()] to obtain allocation-ready values.
#' @param self an [evoland_db] instance
#' @param id_period_post scalar integerish, passed to [pred_data_wide_v()]
#' @param select_score character scalar, name of score/measure to identify best fitting model
#' @param select_maximize logical scalar, whether to maximize or minimize `select_score`
#' @param force logical scalar, recompute transitions that already have potentials
#' stored for this run and period instead of skipping them
#' @param cluster An optional cluster object created by [parallel::makeCluster()] or
#' [mirai::make_cluster()]. Each worker predicts one transition at a time and
#' upserts the result itself; the upserts serialise on the `trans_pot_t` lock, so
#' they stay idempotent and no worker can drop another's rows. See
#' [parquet_db_lock].
#' @return `predict_trans_pot()`: called for side effect; commit `trans_pot_t` to
#' database. Invisibly returns a data.table with one row per transition reporting
#' how many rows were written.
predict_trans_pot <- function(
  self,
  id_period_post,
  select_score,
  select_maximize,
  force = FALSE,
  cluster = NULL
) {
  .check_viable_trans_models(self, select_score) # error on missing models

  viable_trans <- self$trans_meta_t[is_viable == TRUE, id_trans]
  message(glue::glue("Predicting transition potential for {length(viable_trans)} transitions"))

  # one item per transition; i/n only carry the progress message
  items <- data.table::data.table(
    id_run = self$id_run,
    id_trans = viable_trans,
    id_period_post = as.integer(id_period_post),
    i = seq_along(viable_trans),
    n = length(viable_trans)
  )

  items |>
    split(by = "id_trans") |>
    run_parallel_evoland(
      items = _,
      worker_fun = predict_trans_pot_worker,
      parent_db = self,
      cluster = cluster,
      worker_writable = TRUE,
      select_score = select_score,
      select_maximize = select_maximize,
      force = force
    ) |>
    data.table::rbindlist() |>
    invisible()
}

# Worker function for transition potential prediction
# Not exported; used internally by predict_trans_pot
predict_trans_pot_worker <- function(
  item,
  db,
  select_score,
  select_maximize,
  force = FALSE
) {
  id_run_orig <- db$id_run
  on.exit(db$id_run <- id_run_orig, add = TRUE)
  db$id_run <- item[["id_run"]]

  id_trans <- item[["id_trans"]]
  id_period_post <- item[["id_period_post"]]

  # a skipped transition still reports a row, so callers can tell the two apart
  skipped <- data.table::data.table(
    id_trans = id_trans,
    id_period_post = id_period_post,
    n_rows = 0L,
    written = FALSE
  )

  if (!force && .has_predictions(db, id_trans, id_period_post)) {
    message(glue::glue(
      "Found trans_pot_t for ",
      "id_run={db$id_run}/id_trans={id_trans}/id_period={id_period_post}",
      "; set force=TRUE to recompute"
    ))
    return(skipped)
  }

  message(glue::glue(
    "Predicting transition {item[['i']]}/{item[['n']]} (id_trans={id_trans})"
  ))

  # Get model for this transition
  model_blob <- db$get_query(glue::glue(
    r"[
    select learner_full
    from {db$get_read_expr("trans_models_t")}
    where id_trans = {id_trans}
      and learner_full is not null
    order by crossval_score['{select_score}'] {ifelse(select_maximize, "desc", "asc")}
    limit 1
    ]"
  ))[[1]]

  if (length(model_blob) == 0L) {
    stop(glue::glue("No model found for id_trans={id_trans}"))
  }

  learner_obj <- qs2::qs_deserialize(model_blob[[1]])

  # Get predictor data for id_period_post at coords with id_lulc_ant at id_period_post - 1
  pred_data_post <- db$pred_data_wide_v(
    id_trans = id_trans,
    id_period_anterior = id_period_post - 1
  )

  if (nrow(pred_data_post) == 0L) {
    warning(glue::glue(
      "No predictor data for id_trans={id_trans}, id_period={id_period_post}"
    ))
    return(skipped)
  }

  # Predict probabilities; probs keeps the pred_data_post ordering
  probs <- learner_obj$predict_newdata(pred_data_post)$prob[, "TRUE"]

  db$trans_pot_t <- as_trans_pot_t(
    data.table::data.table(
      id_run = db$id_run,
      id_trans = id_trans,
      id_period_post = id_period_post,
      id_coord = pred_data_post[["id_coord"]],
      value = probs
    )
  )

  # Only report what was written: the potentials are in the database already, and
  # shipping them back through a cluster socket would be needlessly expensive.
  data.table::data.table(
    id_trans = id_trans,
    id_period_post = id_period_post,
    n_rows = nrow(pred_data_post),
    written = TRUE
  )
}

# called for side effect: error if a viable transition does either not have a full model available
# OR it does not have the required crossvalidation score
.check_viable_trans_models <- function(self, select_score) {
  viable_trans <- self$trans_meta_t[is_viable == TRUE]

  modeled_ids <- self$get_query(glue::glue(
    r"[
    select distinct id_trans
    from {self$get_read_expr("trans_models_t")}
    where 
      learner_full is not null
      and crossval_score['{select_score}'] is not null
    ]"
  ))[[1]]

  missing_models <- sort(setdiff(viable_trans$id_trans, modeled_ids))

  if (length(missing_models) == 0L) {
    return()
  }

  # If a transition has no valid model but _has_ an error row, print that
  err_messages <-
    self$get_query(glue::glue(
      r"[
        select id_trans, learner_id, learner_params.error_message
        from {self$get_read_expr("trans_models_t")}
        where id_trans in ({toString(missing_models)})
          and learner_params.error_message is not null
        ]"
    )) |>
    split(by = c("id_trans", "learner_id"), keep.by = TRUE) |>
    sapply(function(df) {
      glue::glue(
        "id_trans: {df[,id_trans]}, learner_id: {df[,learner_id]}",
        df[, error_message],
        "\n"
      )
    }) |>
    gsub(pattern = "\\x1b\\[[0-9;]*m", replacement = "") # drop color codes

  err_messages <- if (length(err_messages) > 0) {
    c("\nFound following failed models:", err_messages)
  } else {
    character()
  }

  stop(glue::glue_collapse(
    sep = "\n",
    c(
      "No fitted model for viable transition(s): {toString(missing_models)}.",
      "  Check that trans_models_t has a learner_full for each viable trans",
      err_messages
    )
  ))
}

# check that if we already have predictions for given id_run/id_trans/id_period_post
.has_predictions <- function(self, id_trans, id_period_post) {
  # TODO DB internals leaking - maybe refactor? add method to check that any data are present for a
  # given slice?

  # Take the table lock for the read as well: a concurrent worker upserting into
  # trans_pot_t deletes and rewrites whole partition folders, which a reader
  # landing mid-write would see as missing or half-written files.
  self$with_table_lock("trans_pot_t", {
    if (!file.exists(self$get_table_path("trans_pot_t"))) {
      FALSE
    } else {
      self$get_query(glue::glue(
        r"[
          select exists (
            select 1
            from {self$get_read_expr("trans_pot_t")}
            where id_trans = {id_trans} and id_period_post = {id_period_post}
          )
        ]"
      ))[[1]] # returns scalar boolean
    }
  })
}
