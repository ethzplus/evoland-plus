#' Create Intervention Metadata Table from Configuration
#'
#' Creates an intrv_meta_t table based on the intervention specification in an
#' evoland_config object. This function uses the interventions field to create
#' metadata entries for each intervention type.
#'
#' @name intrv_meta_t
#'
#' @param config An [evoland_config] instance
#'
#' @return A data.table of class "intrv_meta_t" with columns:
#'   - `id_intrv`: Unique ID for each intervention
#'   - `id_period_list`: Array of associated period IDs
#'   - `id_trans_list`: Array of associated transition IDs
#'   - `pre_allocation`: Boolean indicating if intervention is pre-allocation
#'   - `name`: Name for use in code and queries
#'   - `pretty_name`: Name for plots/output
#'   - `description`: Long description / operationalisation
#'   - `sources`: Array of structs with url and md5sum
#'   - `params`: Map of parameters
#' @export
as_intrv_meta_t <- function(x) {
  new_evoland_table(
    x,
    "intrv_meta_t",
    "id_intrv"
  )
}

#' @export
create_intrv_meta_t <- function(config) {
  intrv_spec <- config[["interventions"]]

  if (is.null(intrv_spec)) {
    stop("No interventions specified in config")
  }
  # Extract intervention names
  intrv_names <- names(intrv_spec)
  if (is.null(intrv_names) || any(intrv_names == "")) {
    stop("All intervention entries must have names")
  }

  # Create periods_t to do date lookups
  periods_t <- create_periods_t(config)
  find_matching_periods <- function(intrv, periods_t) {
    start_date <- intrv[["start_intervention"]]
    end_date <- intrv[["end_intervention"]]

    if (is.null(start_date) || is.null(end_date)) {
      return(integer())
    }

    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)

    # Find periods that overlap with intervention timeframe
    overlap_mask <- start_date <= periods_t$end_date & end_date >= periods_t$start_date
    matching_periods <- periods_t$id_period[overlap_mask]

    return(matching_periods)
  }

  x <- data.table::data.table(
    id_intrv = seq_along(intrv_names),
    id_period_list = lapply(intrv_spec, find_matching_periods, periods_t = periods_t),
    id_trans_list = pluck_wildcard(intrv_spec, NA, "transitions") %||% integer(),
    pre_allocation = unlist(
      pluck_wildcard(intrv_spec, NA, "pre_allocation") %||% NA
    ),
    name = intrv_names,
    pretty_name = unlist(
      pluck_wildcard(intrv_spec, NA, "pretty_name") %||% intrv_names
    ),
    description = unlist(
      pluck_wildcard(intrv_spec, NA, "description") %||% NA_character_
    ),
    sources = lapply(
      intrv_spec,
      function(intrv) {
        data.frame(
          url = unlist(pluck_wildcard(intrv, "sources", NA, "url") %||% character()),
          md5sum = unlist(pluck_wildcard(intrv, "sources", NA, "md5sum") %||% character())
        )
      }
    ),
    params = pluck_wildcard(intrv_spec, NA, "params")
  )

  as_intrv_meta_t(x)
}

#' @export
validate.intrv_meta_t <- function(x, ...) {
  NextMethod()
  # TODO probably still needs a "has_mask" field, because some interventions apply to
  # the full domain
  data.table:::setcolorder(
    x,
    c(
      "id_intrv",
      "id_period_list",
      "id_trans_list",
      "pre_allocation",
      "name",
      "pretty_name",
      "description",
      "sources",
      "params"
    )
  )

  sources_dt <- unique(data.table::rbindlist(x[["sources"]]))

  stopifnot(
    is.integer(x[["id_intrv"]]),
    is.list(x[["id_period_list"]]),
    is.list(x[["id_trans_list"]]),
    is.list(x[["sources"]]),
    is.list(x[["params"]]),
    is.character(x[["name"]]),
    is.character(x[["pretty_name"]]),
    is.character(x[["description"]]),
    is.logical(x[["pre_allocation"]]),
    !anyDuplicated(x[["id_intrv"]]),
    !anyDuplicated(x[["name"]]),
    !any(x[["name"]] == ""),
    !anyDuplicated(sources_dt[["url"]])
  )

  return(x)
}

#' @export
#' @describeIn intrv_meta_t Print an intrv_meta_t object, passing params to data.table print
#' @param ... passed to [yaml::as.yaml()]
print.intrv_meta_t <- function(x, ...) {
  if (nrow(x) > 0) {
    cat(glue::glue(
      "Intervention Metadata Table\n",
      "Number of interventions: {nrow(x)}\n\n"
    ))
  } else {
    cat("Intervention Metadata Table (empty)\n")
    return(invisible(x))
  }

  # try to serialize as close as possible to config file
  # but print info about id_intrv
  as_list <- list()
  as_list[["interventions"]] <-
    x |>
    split(by = "name") |>
    lapply(as.list) |>
    lapply(function(y) {
      y[["name"]] <- NULL
      y[["sources"]] <- y[["sources"]][[1]]
      y[["params"]] <- y[["params"]][[1]]
      y[["id_period_list"]] <- y[["id_period_list"]][[1]]
      y[["id_trans_list"]] <- y[["id_trans_list"]][[1]]
      return(y)
    })

  cat(yaml::as.yaml(as_list, column.major = FALSE))

  invisible(x)
}
