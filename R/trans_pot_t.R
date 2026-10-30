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
#'   - `id_run`: Foreign key to [runs_t()]
#'   - `id_trans`: Foreign key to [trans_meta_t()]
#'   - `id_period_post`: Foreign key to [periods_t()], the posterior period
#'   - `id_coord`: Foreign key to [coords_t()]
#'   - `value`: Raw transition potential in \[0, 1\], as predicted by the transition's model
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

  as_ducklake_db_t(
    x,
    class_name = "trans_pot_t",
    key_cols = c("id_run", "id_trans", "id_period_post", "id_coord"),
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


#' @describeIn trans_pot_t For each viable transition in `id_run`, predict the raw
#' transition potential for a given period and store it in `trans_pot_t` in the database. Raw
#' potentials are per-transition MLR3 model probabilities; they are **not** yet allocation-ready
#' (not column-scaled to target rates, not row-closed to max probability of 1). Use
#' [adjusted_trans_pot_v()] to obtain allocation-ready values. Set
#' `options(evoland.use_prefetch_predict=TRUE)` to prefetch all predictors; this causes higher
#' memory pressure but only needs to go to disk once. The learners have
#' `parallel_predict` enabled, see [mlr3::Learner]: the prediction task is automatically
#' chunked out to any [future](https://future.futureverse.org/) workers available.
#' @param self an [evoland_db] instance
#' @param id_period_post scalar integerish, passed to [pred_wide_predict_v()]
#' @param select_score character scalar, name of score/measure to identify best fitting model
#' @param select_maximize logical scalar, whether to maximize or minimize `select_score`
#' @param force logical, predict even if `trans_pot_t` already holds potentials for `id_run`
#' and the period (potentials inherited from an ancestor run do not count)
#' @param id_run integer, the run to predict for: its models, predictors and anterior map are
#' read through its lineage, and the potentials are written under it
#' @return `predict_trans_pot()`: called for side effect; commit `trans_pot_t` to database
predict_trans_pot <- function(
  self,
  id_period_post,
  select_score,
  select_maximize,
  force = FALSE,
  id_run = self$id_run
) {
  stopifnot("id_run must be a single integer" = is_scalar_integerish(id_run))
  viable_trans <- self$trans_meta_t[is_viable == TRUE]

  # potentials may come from outside evoland (written to trans_pot_t directly); models are only
  # needed for the transitions left to predict
  to_predict <- viable_trans[["id_trans"]]
  if (!force) {
    # with verbose logging, report the transitions skipped here (pass force = TRUE to recompute)
    to_predict <- setdiff(to_predict, .predicted_trans(self, id_period_post, id_run))
  }
  if (length(to_predict) == 0L) {
    return(invisible(NULL))
  }
  .check_viable_trans_models(self, select_score, id_run) # error on missing models
  message(glue::glue("Predicting transition potential for {length(to_predict)} transitions"))

  use_prefetch <- getOption("evoland.use_prefetch_predict", default = FALSE)

  pred_data_all <- NULL

  for (id_trans in to_predict) {
    message(glue::glue(
      "Predicting transition {which(to_predict == id_trans)}/",
      "{length(to_predict)} (id_trans={id_trans})"
    ))

    if (use_prefetch) {
      if (is.null(pred_data_all)) {
        pred_data_all <- self$pred_wide_predict_v(
          id_trans = NA,
          id_period_anterior = id_period_post - 1,
          id_run = id_run
        )
      }
      id_lulc_anterior <- viable_trans[
        id_trans == id_sel,
        id_lulc_anterior,
        env = list(id_sel = id_trans)
      ]
      pred_data_post <- pred_data_all[id_lulc == id_lulc_anterior, !"id_lulc"]
    } else {
      # Get predictor data for id_period_post at coords with id_lulc_ant at id_period_post - 1
      pred_data_post <- self$pred_wide_predict_v(
        id_trans = id_trans,
        id_period_anterior = id_period_post - 1,
        id_run = id_run
      )
    }

    # Get model for this transition
    model_blob <- self$get_query(
      r"[
      select learner_full
      from {self$get_read_expr("trans_models_t", id_run = id_run)}
      where id_trans = {id_trans}
        and learner_full is not null
      order by crossval_score[{select_score}] {sort_direction}
      limit 1
      ]",
      sort_direction = DBI::SQL(if (select_maximize) "desc" else "asc"),
      as_atomic = TRUE
    )

    if (length(model_blob) == 0L) {
      warning(glue::glue("No model found for id_trans={id_trans}"))
      next
    }

    learner_obj <- qs2::qs_deserialize(model_blob[[1]])

    if (nrow(pred_data_post) == 0L) {
      warning(glue::glue(
        "No predictor data for id_trans={id_trans}, id_period={id_period_post}"
      ))
      next
    }

    # if future workers are available, chunk out prediction to them
    learner_obj$parallel_predict <- TRUE

    # Predict probabilities; probs keeps the pred_data_post ordering
    probs <- learner_obj$predict_newdata(pred_data_post)$prob[, "TRUE"]

    self$trans_pot_t <- as_trans_pot_t(
      data.table::data.table(
        id_run = id_run,
        id_trans = id_trans,
        id_period_post = id_period_post,
        id_coord = pred_data_post$id_coord,
        value = probs
      )
    )
  }
}

# called for side effect: error if a viable transition does either not have a full model available
# OR it does not have the required crossvalidation score
.check_viable_trans_models <- function(self, select_score, id_run = self$id_run) {
  viable_trans <- self$trans_meta_t[is_viable == TRUE]

  modeled_ids <- self$get_query(
    r"[
    select distinct id_trans
    from {self$get_read_expr("trans_models_t", id_run = id_run)}
    where
      learner_full is not null
      and crossval_score[{select_score}] is not null
    ]",
    as_atomic = TRUE
  )

  missing_models <- sort(setdiff(viable_trans$id_trans, modeled_ids))

  if (length(missing_models) == 0L) {
    return()
  }

  # If a transition has no valid model but _has_ an error row, print that
  err_messages <-
    self$get_query(
      r"[
        select id_trans, learner_id, learner_params.error_message
        from {self$get_read_expr("trans_models_t", id_run = id_run)}
        where id_trans in ({missing_models*})
          and learner_params.error_message is not null
        ]"
    ) |>
    split(by = c("id_trans", "learner_id"), keep.by = TRUE) |>
    sapply(function(df) {
      glue::glue(
        "id_trans: {df[,id_trans]}, learner_id: {df[,learner_id]}\n",
        df[, error_message],
        "\n",
        .trim = FALSE
      )
    }) |>
    gsub(pattern = "\\x1b\\[[0-9;]*m", replacement = "") # drop color codes

  err_messages <- if (length(err_messages) > 0) {
    c("\nFound following failed models:\n", err_messages)
  } else {
    character()
  }

  stop(
    glue::glue(
      "No fitted model for viable transition(s): {toString(missing_models)}.\n",
      "  Check that trans_models_t has a learner_full with a crossval_score ",
      "'{select_score}' for each viable transition\n",
      .trim = FALSE
    ),
    err_messages
  )
}

# The transitions that already have potentials for the period under id_run itself. Not
# through the lineage: a run with its own models must not take its ancestors' potentials for
# its own. Sharing potentials is explicit, by predicting under the run that shares them (see
# `potentials_ancestor` in allocate()).
.predicted_trans <- function(self, id_period_post, id_run) {
  if (!"trans_pot_t" %in% self$list_tables()) {
    return(integer(0))
  }

  self$get_query(
    "select distinct id_trans
     from dl_db.trans_pot_t
     where id_run = {id_run} and id_period_post = {id_period_post}",
    as_atomic = TRUE
  )
}
