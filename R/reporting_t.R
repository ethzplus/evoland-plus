#' Create Reporting Table
#'
#' The reporting table holds information handy for writing out reports (tables,
#' graphs...)
#'
#' @name reporting_t
#'
#' @return A data.table of class "reporting_t" with columns:
#'   - `key`: Unique character key
#'   - `value`: Value as character
#' @export
as_reporting_t <- function(x) {
  if (missing(x)) {
    x <- data.table::data.table(
      key = character(),
      value = character()
    )
  }

  data.table::setDT(x) |>
    cast_dt_col("key", "char") |>
    cast_dt_col("value", "char")

  as_parquet_db_t(
    x,
    class_name = "reporting_t",
    key_cols = "key"
  )
}

#' @describeIn reporting_t Set or update reporting metadata in the database.
#' @param ... Each named argument is entered into the table with the argument name as its key.
#' @keywords internal
db_set_report <- function(self, ...) {
  params <- list(...)
  if (self$row_count("reporting_t") == 0L) {
    # only upsert if these values are missing upon DB init
    params[["report_name"]] <-
      params[["report_name"]] %||% "evoland_scenario"
    params[["report_name_pretty"]] <-
      params[["report_name_pretty"]] %||% "Default Evoland Scenario"
    params[["report_include_date"]] <-
      params[["report_include_date"]] %||% "TRUE"
    params[["creator_username"]] <-
      params[["creator_username"]] %||% Sys.getenv("USER", unset = "unknown")
  }
  params[["last_opened"]] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  params[["last_opened_username"]] <- Sys.getenv("USER", unset = "unknown")

  self$reporting_t <- as_reporting_t(list(
    key = names(params), # cannot name a column "key" in data.table()
    value = unlist(params)
  ))
}


#' @export
validate.reporting_t <- function(x, ...) {
  NextMethod()

  data.table::setcolorder(
    x,
    c(
      "key",
      "value"
    )
  )

  return(x)
}
