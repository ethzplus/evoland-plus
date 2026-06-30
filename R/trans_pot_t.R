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
      id_trans = integer(0),
      id_period_post = integer(0),
      id_coord = integer(0),
      value = numeric(0)
    )
  }

  data.table::setDT(x) |>
    cast_dt_col("id_trans", "int") |>
    cast_dt_col("id_period_post", "int") |>
    cast_dt_col("id_coord", "int")

  as_parquet_db_t(
    x,
    "trans_pot_t",
    c("id_trans", "id_period_post", "id_coord")
  )
}

#' @export
validate.trans_pot_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
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

    cat(glue::glue(
      "Transition Potential Table\n",
      "Rows: {nrow(x)}\n",
      "Transitions: {n_trans}, Periods: {n_periods}\n\n"
    ))
  } else {
    cat("Transition Potential Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}


#' @describeIn trans_pot_t For each viable transition, predict the raw transition
#' potential for a given period and store it in `trans_pot_t` in the database.
#' Raw potentials are per-transition MLR3 model probabilities; they are **not**
#' yet allocation-ready (not column-scaled to target rates, not row-closed).
#' Use [adjusted_trans_pot_v()] to obtain allocation-ready values.
#' @param self an [evoland_db] instance
#' @param id_period_post scalar integerish, passed to `self$pred_data_wide_v()`
#' @param select_score character scalar, name of score/measure to identify best fitting model
#' @param select_maximize logical scalar, whether to maximize or minimize `select_score`
#' @return A `trans_pot_t` object (invisibly); the same data are committed to the DB.
predict_trans_pot <- function(
  self,
  id_period_post,
  select_score,
  select_maximize
) {
  # TODO parallelize
  # TODO: this assumes every viable transition has a fitted model, but
  # fit_partial_models()/fit_full_models() inner-join viable transitions against
  # trans_preds_t and silently skip any viable transition that retained no
  # predictors (e.g. a split-less rpart importance tree). That leaves a viable
  # transition without a model and makes the "No model found" stop() below fire
  # for an otherwise valid pipeline. The viable set and the modelled set should be
  # reconciled in one place (either fitting should warn+demote, or prediction
  # should skip unmodelled viable transitions) rather than relying on callers.
  viable_trans <- self$trans_meta_t[is_viable == TRUE]

  gather <- list()
  message(glue::glue("Predicting transition potential for {nrow(viable_trans)} transitions"))

  for (id_trans in viable_trans$id_trans) {
    message(glue::glue(
      "Predicting trans {which(viable_trans$id_trans == id_trans)}/",
      "{nrow(viable_trans)} (id_trans {id_trans})"
    ))

    # Get model for this transition
    model_row <- self$get_query(glue::glue(
      r"[
      select learner_full
      from {self$get_read_expr("trans_models_t")}
      where id_trans = {id_trans}
        and learner_full is not null
      order by crossval_score['{select_score}'] {ifelse(select_maximize, "desc", "asc")}
      limit 1
      ]"
    ))

    if (nrow(model_row) > 1L) {
      stop(glue::glue("Several models found for id_trans={id_trans}"))
    } else if (nrow(model_row) == 0L) {
      stop(glue::glue("No model found for id_trans={id_trans}"))
    }

    # Deserialize full learner
    learner_obj <- qs2::qs_deserialize(model_row$learner_full[[1]])

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

    # Predict probabilities using mlr3 predict_newdata; id_coord is dropped automatically
    probs <- learner_obj$predict_newdata(pred_data_post)$prob[, "TRUE"]
    # Ensure probabilities are in [0, 1]
    probs <- pmax(0, pmin(1, probs))

    gather[[id_trans]] <- data.table::data.table(
      id_trans = id_trans,
      id_coord = pred_data_post$id_coord,
      value = probs
    )
  }

  result <- data.table::rbindlist(gather)[, id_period_post := id_period_post]

  trans_pot <- as_trans_pot_t(result)

  # Store raw potentials in the DB so that adjusted_trans_pot_v() and allocation
  # backends can retrieve them without re-running the models.
  self$commit(trans_pot, "trans_pot_t", method = "upsert")

  invisible(trans_pot)
}
