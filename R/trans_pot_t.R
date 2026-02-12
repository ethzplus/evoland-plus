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
  cast_dt_col(x, "id_trans", "int")
  cast_dt_col(x, "id_period_post", "int")
  cast_dt_col(x, "id_coord", "int")
  new_evoland_table(
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
    !anyDuplicated(x, by = c("id_trans", "id_period_post", "id_coord")),
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


#' @describeIn trans_pot_t For each viable transition, predict the transition potential
#' for a given period, with cumulative probabilities for a single id_coord capped to 1;
#' returns a `trans_pot_t` object
#' @param self an [evoland_db] instance
#' @param id_period_post scalar integerish, passed to `self$pred_data_wide_v()`
predict_trans_pot <- function(
  self,
  id_period_post
) {
  # TODO parallelize
  # TODO move into integration test
  viable_trans <- self$trans_meta_t[is_viable == TRUE]

  gather <- list()
  message(glue::glue("Predicting transition potential for {nrow(viable_trans)} transitions"))

  for (id_trans in viable_trans$id_trans) {
    message(glue::glue(
      "Predicting trans {which(viable_trans$id_trans == id_trans)}/",
      "{nrow(viable_trans)} (id_trans {id_trans})"
    ))

    # Get model for this transition. Only expect one non-null full model.
    # TODO should use a GOF criterion to select one if multiple models exist, but for
    # now just error out
    model_row <- self$fetch(
      "trans_models_t",
      where = glue::glue("id_trans = {id_trans} and model_obj_full is not null")
    )

    if (nrow(model_row) == 0L) {
      stop(glue::glue("No model found for id_trans={id_trans}"))
    } else if (nrow(model_row) > 1) {
      stop(glue::glue(
        "Multiple models found for id_trans={id_trans}, ",
        "edit trans_models_t to have only one per transition"
      ))
    }

    # Deserialize full model
    model_obj <- qs2::qs_deserialize(model_row$model_obj_full[[1]])

    # Get predictor data for id_period_post at coords with id_lulc_ant at id_period_post - 1
    pred_data_post <- self$pred_data_wide_v(
      id_trans = id_trans,
      id_period = id_period_post,
      na_value = 0 # Replace NAs with 0 for prediction
    )

    if (nrow(pred_data_post) == 0L) {
      warning(glue::glue(
        "No predictor data for id_trans={id_trans}, id_period={id_period_post}"
      ))
      next
    }

    # Predict probabilities
    # Drop id_coord for prediction
    pred_cols <- grep("^id_pred_", names(pred_data_post), value = TRUE)

    # Predict - assuming model has predict() method that returns probabilities
    # FIXME apparently not all models respect the standard function signature, pull in
    # mlr3
    if (inherits(model_obj, "ranger")) {
      probs <-
        predict(
          model_obj,
          data = pred_data_post[, -"id_coord"],
          type = "response"
        )[[
          "predictions" # assume model has been run with probability = TRUE
        ]][,
          "TRUE" # matrix column for transition _does_ occur
        ]
    } else {
      probs <-
        predict(
          model_obj,
          newdata = pred_data_post[, -"id_coord"],
          type = "response"
        ) |>
        setNames(NULL)
    }
    # Ensure probabilities are in [0, 1]
    probs <- pmax(0, pmin(1, probs))

    # Create a data.table with id_coord and probability
    gather[[i]] <- data.table::data.table(
      id_trans = id_trans,
      id_coord = pred_data_post$id_coord,
      value = probs
    )
  }

  # normalize probabilities if they exceed 1 per id_coord
  normalized <-
    data.table::rbindlist(gather)[,
      tot_pot := sum(value),
      by = id_coord
    ][
      tot_pot > 1,
      value := value / tot_pot
    ][,
      `:=`(
        tot_pot = NULL,
        id_period_post = id_period_post
      )
    ]

  as_trans_pot_t(normalized)
}
