#' Create Predictor Metadata Table
#'
#' Construct and validate `pred_meta_t` objects, which are used to store predictor
#' metadata.
#'
#' @name pred_meta_t
#'
#' @param x An object that is accepted by [data.table::setDT()]
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
as_pred_meta_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      id_pred = integer(0),
      name = character(0),
      pretty_name = character(0),
      description = character(0),
      orig_format = character(0),
      sources = list(),
      unit = character(0),
      factor_levels = list()
    )
  }
  new_evoland_table(
    x,
    "pred_meta_t",
    "id_pred"
  )
}


#' @describeIn pred_meta_t Creates a `pred_meta_t` table from intervention specifications
#' @param pred_spec A list of predictor specifications, schema: see examples
# nolint start
#' @examples create_pred_meta_t(list(
#'    noise = list(
#'      unit = "dBa",
#'      pretty_name = "Maximum noise exposure",
#'      orig_format = "10m*10m raster",
#'      description = "daytime & nighttime road & rail noise exposure",
#'      sources = list(
#'        list(
#'          url = "https://data.geo.admin.ch/ch.bafu.laerm-strassenlaerm_tag/laerm-strassenlaerm_tag/laerm-strassenlaerm_tag_2056.tif",
#'          md5sum = "a4b9f1c04ee63824f18852bfd1eecbdd"
#'        ),
#'        list(
#'          url = "https://data.geo.admin.ch/ch.bafu.laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht_2056.tif",
#'          md5sum = "4b782128495b5af8467e2259bd57def2"
#'        )
#'      )
#'    ),
#'    distance_to_lake = list(
#'      unit = "m",
#'      pretty_name = "Distance to closest lake",
#'      orig_format = "vector",
#'      description = "Derived from swissTLM3D",
#'      sources = list(list(
#'        url = "https://data.geo.admin.ch/ch.swisstopo.swisstlm3d/swisstlm3d_2025-03/swisstlm3d_2025-03_2056_5728.gpkg.zip",
#'        md5sum = "ecb3bcfbf6316c6e7542e20de24f61b7"
#'      ))
#'    )
#'  ))
# nolint end
#' @export
create_pred_meta_t <- function(pred_spec) {
  # Extract predictor names
  pred_names <- names(pred_spec)
  if (is.null(pred_names) || any(pred_names == "")) {
    stop("All pred_data_spec entries must have names")
  }

  x <- data.table::data.table(
    name = pred_names,
    # path is pred_spec > pred_name > leaf_name
    # we pluck each element, then replace potential null using %||%
    pretty_name = unlist(
      mapply(
        function(x, y) x %||% y,
        pluck_wildcard(pred_spec, NA, "pretty_name"),
        pred_names,
        SIMPLIFY = FALSE
      )
    ),
    description = unlist(
      lapply(pluck_wildcard(pred_spec, NA, "description"), function(x) x %||% NA_character_)
    ),
    orig_format = unlist(
      lapply(pluck_wildcard(pred_spec, NA, "orig_format"), function(x) x %||% NA_character_)
    ),
    sources = lapply(
      pred_spec,
      # path is pred_spec > pred_name > sources > listelement > url/md5sum
      function(pred) {
        data.frame(
          url = unlist(pluck_wildcard(pred, "sources", NA, "url")),
          md5sum = unlist(pluck_wildcard(pred, "sources", NA, "md5sum"))
        )
      }
    ),
    unit = unlist(
      lapply(pluck_wildcard(pred_spec, NA, "unit"), function(x) x %||% NA_character_)
    ),
    factor_levels = pluck_wildcard(pred_spec, NA, "factor_levels")
  )

  as_pred_meta_t(x)
}

#' @export
validate.pred_meta_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "name",
      "pretty_name",
      "description",
      "orig_format",
      "sources",
      "unit",
      "factor_levels"
    )
  )
  # we don't know if there's an id_pred
  data.table::setcolorder(x, "id_pred", before = "name", skip_absent = TRUE)

  sources_dt <- unique(data.table::rbindlist(x[["sources"]]))

  stopifnot(
    is.character(x[["name"]]),
    is.character(x[["pretty_name"]]),
    is.character(x[["description"]]),
    is.character(x[["orig_format"]]),
    is.list(x[["sources"]]),
    is.character(x[["unit"]]),
    is.list(x[["factor_levels"]]),
    !anyDuplicated(x[["name"]]),
    !any(x[["name"]] == ""),
    !anyDuplicated(sources_dt[["url"]])
  )

  return(x)
}

#' @export
#' @describeIn pred_meta_t Print an `pred_meta_t` object, passing params to data.table print
#' @param ... passed to [data.table::print.data.table()]
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

  NextMethod(trunc.cols = TRUE, ...)

  invisible(x)
}
