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
#' @return `predict_trans_pot()`: called for side effect; commit `trans_pot_t` to database
predict_trans_pot <- function(
  self,
  id_period_post,
  select_score,
  select_maximize,
  force = FALSE
) {
  # TODO parallelize
  .check_viable_trans_models(self, select_score) # error on missing models

  viable_trans <- self$trans_meta_t[is_viable == TRUE, id_trans]
  message(glue::glue("Predicting transition potential for {length(viable_trans)} transitions"))

  for (id_trans in viable_trans) {
    has_predictions <- .has_predictions(self, id_trans, id_period_post)

    if (has_predictions && !force) {
      message(glue::glue(
        "Found trans_pot_t for ",
        "id_run={self$id_run}/id_trans={id_trans}/id_period={id_period_post}",
        "; set force=TRUE to recompute"
      ))
      next
    } else {
      message(glue::glue(
        "Predicting transition {which(viable_trans == id_trans)}/",
        "{length(viable_trans)} (id_trans={id_trans})"
      ))
    }

    # Get model for this transition
    model_blob <- self$get_query(glue::glue(
      r"[
      select learner_full
      from {self$get_read_expr("trans_models_t")}
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
    pred_data_post <- self$pred_data_wide_v(
      id_trans = id_trans,
      id_period_anterior = id_period_post - 1
    )

    if (nrow(pred_data_post) == 0L) {
      warning(glue::glue(
        "No predictor data for id_trans={id_trans}, id_period={id_period_post}"
      ))
      next
    }

    # Predict probabilities; probs keeps the pred_data_post ordering
    probs <- learner_obj$predict_newdata(pred_data_post)$prob[, "TRUE"]

    self$trans_pot_t <- as_trans_pot_t(
      data.table::data.table(
        id_run = self$id_run,
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
  if (!"trans_pot_t" %in% self$list_tables()) {
    return(FALSE)
  }

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
