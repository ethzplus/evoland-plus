# Run upon loading (!= attachment) and unloading of the package

.onLoad <- function(libname, pkgname) {
  cache_path <-
    Sys.getenv(
      "EVOLAND_CACHEDIR",
      unset = "~/evoland_cache"
    ) |>
    path.expand()

  options("evoland.cachedir" = cache_path)
}

.onUnload <- function(libpath) {
  .Options$evoland.cachedir <- NULL
}

#' @importFrom data.table :=
#' @importFrom data.table %chin%
.datatable.aware <- TRUE

# rcpp imports
#' @importFrom Rcpp sourceCpp
#' @useDynLib evoland, .registration = TRUE
NULL

# silence R CMD check complaints - it doesn't see :: namespace imports inside/around R6 constructors
#' @importFrom DBI dbConnect
#' @importFrom duckdb duckdb
#' @importFrom paradox ps
#' @importFrom R6 R6Class
NULL

# Column names and data.table specials used in non-standard evaluation (`j`/`by`/`on`
# expressions, `:=`). R CMD check's codetools pass cannot see through data.table NSE and
# reports each of them as "no visible binding for global variable" in a NOTE; declaring
# them here silences the false positive
# Regenerate after adding NSE expressions with:
#   tools:::.check_code_usage_in_package("evoland")
utils::globalVariables(c(
  ".",
  ".I",
  ".N",
  ".SD",
  "From*",
  "To*",
  "V1",
  "area",
  "cardinality",
  "cells",
  "did_transition",
  "distance",
  "distance_class",
  "end_date",
  "error_message",
  "found_files",
  "frac_expander",
  "frac_patcher",
  "frequency_rel",
  "i.area",
  "i.gain",
  "i.loss",
  "id_coord",
  "id_lulc",
  "id_lulc_anterior",
  "id_lulc_posterior",
  "id_period",
  "id_pred",
  "id_run",
  "id_trans",
  "is_extrapolated",
  "is_persistence",
  "is_viable",
  "lat",
  "local_filename",
  "local_path",
  "lon",
  "md5sum",
  "md5sum_actual",
  "mean_date",
  "mean_patch_size",
  "no_found_files",
  "parent_id_run",
  "patch_elongation",
  "patch_isometry",
  "patch_size_variance",
  "period_length_d",
  "period_length_y",
  "rate",
  "rate_annual",
  "shape",
  "sources",
  "start_date",
  "to_download",
  "value"
))
