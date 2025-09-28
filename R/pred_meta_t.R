#' Create Predictor Metadata Table from Configuration
#'
#' Creates a pred_meta_t table based on the predictor specification in an
#' evoland_config object. This function uses the pred_data field to create
#' metadata entries for each predictor variable.
#'
#' @name pred_meta_t
#'
#' @param config An [evoland_config] instance
#'
#' @return A data.table of class "pred_meta_t" with columns:
#'   - `id_pred`: Unique ID for each predictor
#'   - `name`: Name for use in code and queries
#'   - `pretty_name`: Name for plots/output
#'   - `description`: Long description / operationalisation
#'   - `orig_format`: Original format description
#'   - `sources`: Sources, a data.frame with cols `url` and `md5sum`
#'   - `unit`: SI-compatible unit (nullable for categorical)
#'   - `factor_levels`: Map of factor levels (nullable)
#' @export
create_pred_meta_t <- function(config) {
  pred_data_spec <- config[["pred_data"]]

  if (!is.list(pred_data_spec) || length(pred_data_spec) == 0) {
    stop("pred_data must be a non-empty list")
  }

  # Extract predictor names
  pred_names <- names(pred_data_spec)
  if (is.null(pred_names) || any(pred_names == "")) {
    stop("All pred_data entries must have names")
  }

  x <- data.table::data.table(
    id_pred = seq_along(pred_names),
    name = pred_names,
    # path is config > pred_data > pred_name > leaf_name
    pretty_name = unlist(
      pluck_wildcard(pred_data_spec, NA, "pretty_name") %||% pred_names
    ),
    description = unlist(
      pluck_wildcard(pred_data_spec, NA, "description") %||% NA_character_
    ),
    orig_format = unlist(
      pluck_wildcard(pred_data_spec, NA, "orig_format") %||% NA_character_
    ),
    sources = lapply(
      pred_data_spec,
      # path is config > pred_data > pred_name > sources > listelement > url/md5sum
      function(pred) {
        data.frame(
          url = unlist(pluck_wildcard(pred, "sources", NA, "url")),
          md5sum = unlist(pluck_wildcard(pred, "sources", NA, "md5sum"))
        )
      }
    ),
    unit = unlist(
      pluck_wildcard(pred_data_spec, NA, "unit") %||% NA_character_
    ),
    factor_levels = pluck_wildcard(pred_data_spec, NA, "factor_levels")
  )

  data.table::setkey(x, "id_pred")

  new_evoland_table(x, "pred_meta_t")
}

#' @export
validate.pred_meta_t <- function(x, ...) {
  # Check that it's a data.table
  if (!inherits(x, "data.table")) {
    stop("pred_meta_t must inherit from data.table")
  }

  # Check required columns
  check_missing_names(
    x,
    c(
      "id_pred",
      "name",
      "pretty_name",
      "description",
      "orig_format",
      "sources",
      "unit",
      "factor_levels"
    )
  )

  # Check column types
  if (!is.integer(x[["id_pred"]])) {
    id_ints <- as.integer(x[["id_pred"]])
    if (anyNA(id_ints)) {
      stop("id_pred must be int or coercible to it")
    }
    data.table::set(x, j = "id_pred", value = id_ints)
  }

  if (!is.character(x[["name"]])) {
    stop("name must be character")
  }

  if (!is.character(x[["pretty_name"]])) {
    stop("pretty_name must be character")
  }

  if (!is.character(x[["description"]])) {
    stop("description must be character")
  }

  if (!is.character(x[["orig_format"]])) {
    stop("orig_format must be character")
  }

  if (!is.list(x[["sources"]])) {
    stop("sources must be list")
  }

  if (!is.character(x[["unit"]])) {
    stop("unit must be character")
  }

  if (!is.list(x[["factor_levels"]])) {
    stop("factor_levels must be a list")
  }

  # if empty, don't run soft checks
  if (nrow(x) == 0L) {
    return(x)
  }

  # Check for unique id_pred values
  if (anyDuplicated(x[["id_pred"]])) {
    stop("id_pred values must be unique")
  }

  # Check for unique names
  if (anyDuplicated(x[["name"]])) {
    stop("name values must be unique")
  }

  # Check for non-empty names
  if (any(x[["name"]] == "")) {
    stop("name values cannot be empty strings")
  }

  # Check for positive IDs
  if (any(x[["id_pred"]] <= 0)) {
    stop("id_pred must be positive integers")
  }

  # check that we're not trying to ingest conflicting versions of raw files
  sources_dt <- unique(data.table::rbindlist(x[["sources"]]))
  if (anyDuplicated(sources_dt[["url"]])) {
    stop("One or more sources is declared with conflicting md5sums")
  }

  return(x)
}

#' @export
#' @describeIn pred_meta_t Print a pred_meta_t object, passing params to data.table print
#' @param ... passed to [yaml::as.yaml()]
print.pred_meta_t <- function(x, ...) {
  if (nrow(x) > 0) {
    cat(glue::glue(
      "Predictor Metadata Table\n",
      "Number of predictors: {nrow(x)}\n\n"
    ))
  } else {
    cat("Predictor Metadata Table (empty)\n")
    return(invisible(x))
  }

  # try to serialize as close as possible to config file
  # but print info about id_pred
  as_list <- list()
  as_list[["pred_data"]] <-
    x |>
    split(by = "name") |>
    lapply(as.list) |>
    lapply(function(y) {
      y[["name"]] <- NULL
      y[["sources"]] <- y[["sources"]][[1]]
      y[["factor_levels"]] <- y[["factor_levels"]][[1]]
      return(y)
    })

  cat(yaml::as.yaml(as_list, column.major = FALSE))

  invisible(x)
}
