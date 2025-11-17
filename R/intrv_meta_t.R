#' Create Intervention Metadata Table / Entries
#'
#' Creates an `intrv_meta_t` table or rows therein based on intervention specifications.
#'
#' @name intrv_meta_t
#'
#' @param x An object that can be passed to [data.table::setDT()]
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
  if (missing(x)) {
    x <- data.table::data.table(
      id_intrv = integer(0),
      id_period_list = list(),
      id_trans_list = list(),
      pre_allocation = logical(0),
      name = character(0),
      pretty_name = character(0),
      description = character(0),
      sources = list(),
      params = list()
    )
  }
  new_evoland_table(
    x,
    "intrv_meta_t",
    "id_intrv"
  )
}

#' @describeIn intrv_meta_t Creates an intrv_meta_t table from intervention specifications
#' @param intrv_spec A list of intervention specifications, schema: see examples
#' @examples create_intrv_meta_t(list(
#'   protected_areas = list(
#'     pre_allocation = TRUE,
#'     pretty_name = "Nature protection areas",
#'     description = "introduces additional protected areas (PAs",
#'     periods = c(7, 8),
#'     transitions = c(1, 2),
#'     sources = list(
#'       list(
#'         url = "file:///somedir/protected_areas.gpkg",
#'         md5sum = "something"
#'       )
#'     )
#'   ),
#'   hydro_predictors = list(
#'     pre_allocation = TRUE,
#'     pretty_name = "Hydrological predictor variables",
#'     description = "Provide dynamic predictor vars",
#'     params = list(
#'       tmpdir = "/mnt/ramdisk"
#'     )
#'   )
#' ))
#' @export
create_intrv_meta_t <- function(intrv_spec) {
  # Extract intervention names
  intrv_names <- names(intrv_spec)
  if (is.null(intrv_names) || any(intrv_names == "")) {
    stop("All intervention entries must have names")
  }

  x <- data.table::data.table(
    id_intrv = seq_along(intrv_names),
    id_period_list = pluck_wildcard(intrv_spec, NA, "periods"),
    id_trans_list = pluck_wildcard(intrv_spec, NA, "transitions"),
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

#' @describeIn intrv_meta_t Creates an metadata entry / row
#' @param name Name for use in code and queries
#' @param pretty_name Name for plots/output
#' @param description Long description / operationalisation
#' @param id_period_list Array of associated period IDs
#' @param id_trans_list Array of associated transition IDs
#' @param pre_allocation Boolean indicating if intervention is pre-allocation
#' @param sources Data frame of sources with columns `url` and `md5sum
#' @param params A list of parameters, depth 1; children can only have length 1
#' @export
create_intrv_meta_t_row <- function(
  name = character(),
  pretty_name = character(),
  description = NA_character_,
  id_period_list = integer(),
  id_trans_list = integer(),
  pre_allocation = logical(),
  sources = data.frame(url = character(), md5sum = character()),
  params
) {
  stopifnot(
    rlang::is_scalar_character(name),
    rlang::is_scalar_character(pretty_name),
    rlang::is_scalar_character(description),
    is.integer(id_period_list),
    is.integer(id_trans_list),
    rlang::is_scalar_logical(pre_allocation),
    inherits(sources, "data.frame"),
  )
  check_missing_names(sources, c("url", "md5sum"))

  x <- data.table::data.table(
    id_intrv = NA_integer_,
    id_period_list = list(id_period_list),
    id_trans_list = list(id_trans_list),
    pre_allocation = pre_allocation,
    name = name,
    pretty_name = pretty_name,
    description = description,
    sources = list(sources),
    params = list(params)
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
#' @param ... passed to [data.table::print.data.table()]
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

  NextMethod(trunc.cols = TRUE, ...)

  invisible(x)
}
