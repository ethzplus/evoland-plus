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
