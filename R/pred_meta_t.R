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
#'   - `sources`: Sources, list column of data.frames with cols `url` and `md5sum`
#'   - `unit`: SI units for physical properties, or more complex descriptors
#'     like "bed nights/year" as a proxy for touristic activity
#'   - `data_type`: Factor with levels "int", "float", "bool", "factor".
#'     Used for coercion.
#'   - `fill_value`: Value to use for missing data for [coords_t] coordinate points that
#'     are not explicitly stored.
#'   - `factor_levels`: list of character vectors; order matters!
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
      data_type = factor(
        character(0),
        # leaving out POSIXct / Date for now; can be operationalized as int/float
        levels = c("int", "float", "bool", "factor", "ordered")
      ),
      fill_value = NA_character_,
      factor_levels = list(character(0))
    )
  }

  data.table::setDT(x) |>
    cast_dt_col("id_pred", "int") |>
    cast_dt_col("name", "char") |>
    cast_dt_col("pretty_name", "char") |>
    cast_dt_col("description", "char") |>
    cast_dt_col("orig_format", "char") |>
    cast_dt_col("unit", "char") |>
    cast_dt_col("data_type", "factor", levels = c("int", "float", "bool", "factor", "ordered")) |>
    cast_dt_col("fill_value", "char")

  x[,
    sources := lapply(
      sources,
      function(src) {
        # coerce to data.table with exactly url & md5sum
        if (
          inherits(src, "data.frame") &&
            all(hasName(src, c("url", "md5sum")))
        ) {
          return(src[, c("url", "md5sum")])
        }
        src_dt <- data.table::rbindlist(src, use.names = TRUE)
        if (length(src_dt) == 0L) {
          # length 0 is a null data frame, e.g. if src is NULL or list()
          src_dt <- data.table::data.table(url = character(), md5sum = character())
        }
        src_dt[, .(url, md5sum)]
      }
    )
  ]

  as_parquet_db_t(
    x,
    class_name = "pred_meta_t",
    key_cols = "name",
    alternate_key_cols = "id_pred"
  )
}


#' @describeIn pred_meta_t Creates a `pred_meta_t` table from intervention
#' specifications. Attributes incremental IDs to predictors, starting from
#' `starting_id`. See examples for schema of `pred_spec`.
#' @param pred_spec A list of predictor specifications, schema: see examples
#' @param starting_id Integer, starting ID for predictors; default 1L
#' @examples create_pred_meta_t(list(
#'    noise = list(
#'      unit = "dBa",
#'      pretty_name = "Maximum noise exposure",
#'      orig_format = "10m*10m raster",
#'      description = "daytime & nighttime road & rail noise exposure",
#'      sources = list(
#'        list(
#'          url = "https://example.com/noisedata1.tif",
#'          md5sum = "a4b9f1c04ee63824f18852bfd1eecbdd"
#'        ),
#'        list(
#'          url = "https://example.com/noisedata2.tif",
#'          md5sum = "4b782128495b5af8467e2259bd57def2"
#'        )
#'      ),
#'      data_type = "float"
#'    ),
#'    distance_to_lake = list(
#'      unit = "m",
#'      pretty_name = "Distance to closest lake",
#'      orig_format = "vector",
#'      description = "Derived from swissTLM3D",
#'      sources = list(list(
#'        url = "https://example.com/dist_to_lake.tif",
#'        md5sum = "ecb3bcfbf6316c6e7542e20de24f61b7"
#'      )),
#'      data_type = "float"
#'    )
#'  ))
#' @export
create_pred_meta_t <- function(pred_spec, starting_id = 1L) {
  # Extract predictor names
  pred_names <- names(pred_spec)
  if (is.null(pred_names) || any(pred_names == "")) {
    stop("All pred_data_spec entries must have names")
  }

  x <- data.table::data.table(
    id_pred = seq(from = starting_id, length.out = length(pred_spec)),
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
      pluck_wildcard(pred_spec, NA, "sources"),
      function(src) {
        src_dt <- data.table::rbindlist(src, use.names = TRUE)
        if (length(src_dt) == 0L) {
          src_dt <- data.table::data.table(url = character(), md5sum = character())
        }
        src_dt[, .(url, md5sum)]
      }
    ),
    unit = unlist(
      lapply(pluck_wildcard(pred_spec, NA, "unit"), function(x) x %||% NA_character_)
    ),
    data_type = {
      lapply(pluck_wildcard(pred_spec, NA, "data_type"), function(x) {
        x %||% NA_character_
      }) |>
        unlist() |>
        factor(
          levels = c("int", "float", "bool", "factor", "ordered")
        )
    },
    fill_value = unlist(
      lapply(pluck_wildcard(pred_spec, NA, "fill_value"), function(x) x %||% NA_character_)
    ),
    factor_levels = lapply(
      pluck_wildcard(pred_spec, NA, "factor_levels"),
      \(y) if (is.null(y)) character(0) else as.character(y)
    )
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
    is.factor(x[["data_type"]]),
    "data_type must be set" = !any(is.na(x[["data_type"]])),
    "data_type can only be one of 'int', 'float', 'bool', 'factor', 'ordered'" = setequal(
      levels(x[["data_type"]]),
      c("int", "float", "bool", "factor", "ordered")
    ),
    is.list(x[["factor_levels"]]),
    "name cannot be empty" = !any(x[["name"]] == ""),
    "single URL with multiple checksums present" = !anyDuplicated(sources_dt[["url"]]),
    "sources must hold exactly a url and an md5sum field" = setequal(
      names(sources_dt),
      c("url", "md5sum")
    )
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
