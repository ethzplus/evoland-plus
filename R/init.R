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
