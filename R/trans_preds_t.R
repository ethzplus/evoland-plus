#' Create Transition-Predictor Relationship Table
#'
#' Creates a trans_preds_t table based on the relationships between transitions
#' and predictors. This function establishes which predictors are useful for
#' modelling each transition type.
#'
#' @name trans_preds_t
#' @include evoland_db.R
#'
#' @param db An [evoland_db] instance with populated trans_meta_t and pred_meta_t tables
#'
#' @return A data.table of class "trans_preds_t" with columns:
#'   - `id_pred`: Foreign key to pred_meta_t
#'   - `id_trans`: Foreign key to trans_meta_t
#' @export
as_trans_preds_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_pred = integer(0),
      id_trans = integer(0)
    )
  }
  cast_dt_col(x, "id_pred", "int")
  cast_dt_col(x, "id_trans", "int")
  new_evoland_table(
    x,
    "trans_preds_t",
    c("id_pred", "id_trans")
  )
}

# set an initial full set of transition / predictor relations
evoland_db$set(
  "public",
  "set_full_trans_preds",
  function(overwrite = FALSE) {
    if (self$row_count("trans_preds_t") > 0 && !overwrite) {
      stop("Set overwrite to TRUE to overwrite existing trans_preds_t")
    }
    p <- self$pred_meta_t
    t <- self$trans_meta_t[is_viable == TRUE]

    full <- expand.grid(
      id_pred = p[["id_pred"]],
      id_trans = t[["id_trans"]],
      KEEP.OUT.ATTRS = FALSE
    )

    self$commit(as_trans_preds_t(full), "trans_preds_t", method = "overwrite")
  }
)

#' describeIn trans_preds_t Create a transition-predictor relation, i.e. records the
#' result of a predictor selection step. Runs covariance filtering for each viable
#' transition and stores the selected predictors.
#' param corcut Numeric threshold (0-1) for correlation filtering passed to [covariance_filter()]
#' param filter_fun Defaults to [covariance_filter()], but can be any function that returns
#' param na_value Passed to db$trans_pred_data_v - if not NA, replace all NA predictor values with this value
#' param ... Additional arguments passed to rank_fun via [covariance_filter()]
evoland_db$set(
  "public",
  "get_pruned_trans_preds_t",
  function(
    filter_fun = covariance_filter,
    na_value = NA,
    ...
  ) {
    if (self$row_count("trans_preds_t") == 0) {
      self$set_full_trans_preds()
    }
    trans_preds_pre <- self$trans_preds_t
    unique_trans <- unique(trans_preds_pre$id_trans)

    results_list <- list()

    i <- 1L
    # Iterate over transitions (anterior/posterior pairs)
    for (id_trans in unique_trans) {
      id_preds <- trans_preds_pre$id_pred[
        trans_preds_pre$id_trans == id_trans
      ]

      message(glue::glue(
        "Processing transition {i}/{length(unique_trans)}: id_trans={id_trans}"
      ))
      i <- i + 1L

      if (length(id_preds) == 0L) {
        next
      }

      # Get wide transition-predictor data
      tryCatch(
        {
          trans_pred_data <- self$trans_pred_data_v(id_trans, id_preds, na_value)

          # Check if we have any data
          if (nrow(trans_pred_data) == 0L) {
            warning(glue::glue(
              "No data for transition {id_trans}, skipping"
            ))
            next
          }

          # Check if we have any predictor columns
          pred_cols <- grep("^id_pred_", names(trans_pred_data), value = TRUE)
          if (length(pred_cols) == 0L) {
            warning(glue::glue(
              "No predictor columns for transition {id_trans}, skipping"
            ))
            next
          }

          # Return ranked + filtered predictor names as id_pred_{n}
          filtered_preds <- filter_fun(
            # drop vars that are irrelevant to the filtering
            data = trans_pred_data[, .SD, .SDcols = !c("id_coord", "id_period")],
            result_col = "result",
            ...
          )

          if (length(filtered_preds) > 0L) {
            # Parse id_pred values from column names (e.g., "id_pred_1" -> 1)
            selected_ids <- as.integer(sub("^id_pred_", "", filtered_preds))

            # Create result rows
            results_list[[id_trans]] <- data.table::data.table(
              id_pred = selected_ids,
              id_trans = id_trans
            )

            message(glue::glue(
              "  Selected {length(selected_ids)} predictor(s) for transition {id_trans}"
            ))
          } else {
            message(glue::glue(
              "  No predictors selected for transition {id_trans}"
            ))
          }
        },
        error = function(e) {
          # do not prune on error
          results_list[[id_trans]] <- data.table::data.table(
            id_pred = id_preds,
            id_trans = id_trans
          )
          warning(glue::glue(
            "Error processing transition {id_trans}: {e$message}"
          ))
        }
      )
    }

    # Combine all results
    if (length(results_list) == 0L) {
      warning("No predictors selected for any transition")
      return(invisible(NULL))
    }

    result <- data.table::rbindlist(results_list)

    # FIXME can we do this safely? something might go wrong in between
    # self$commit(as_trans_preds_t(result), "trans_preds_t", method = "overwrite")
    as_trans_preds_t(result)
  }
)

#' @export
validate.trans_preds_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "id_pred",
      "id_trans"
    )
  )

  # Skip soft checks if empty
  if (nrow(x) == 0L) {
    return(x)
  }

  stopifnot(
    is.integer(x[["id_pred"]]),
    is.integer(x[["id_trans"]]),
    !anyDuplicated(x, by = c("id_pred", "id_trans")),
    all(x[["id_pred"]] > 0),
    all(x[["id_trans"]] > 0)
  )

  return(x)
}

#' @export
#' @describeIn trans_preds_t Print a trans_preds_t object, passing params to data.table print
#' @param nrow see [data.table::print.data.table]
#' @param ... passed to [data.table::print.data.table]
print.trans_preds_t <- function(x, nrow = 10, ...) {
  if (nrow(x) > 0) {
    n_preds <- data.table::uniqueN(x[["id_pred"]])
    n_trans <- data.table::uniqueN(x[["id_trans"]])
    avg_preds_per_trans <- nrow(x) / n_trans
    avg_trans_per_pred <- nrow(x) / n_preds

    cat(glue::glue(
      "Transition-Predictor Relationships Table\n",
      "Total relationships: {nrow(x)}\n",
      "Predictors: {n_preds}, Transitions: {n_trans}\n",
      "Avg predictors per transition: {round(avg_preds_per_trans, 1)}\n",
      "Avg transitions per predictor: {round(avg_trans_per_pred, 1)}\n\n"
    ))
  } else {
    cat("Transition-Predictor Relationships Table (empty)\n")
  }
  NextMethod(nrow = nrow, ...)
  invisible(x)
}
