#!/usr/bin/env Rscript

# Reusing logic from remotes::local_package_deps
# MIT licensed, source at https://github.com/r-lib/remotes/

desc <- remotes:::read_dcf("DESCRIPTION")

dependencies <- c("Depends", "Imports", "LinkingTo", "Suggests")
dependencies <- intersect(dependencies, names(desc))
pkg_deps <-
  lapply(desc[dependencies], remotes:::parse_deps) |>
  lapply(`[[`, "name") |>
  unlist(use.names = FALSE)

pkgs_to_install <- setdiff(pkg_deps, installed.packages()[, 1])
install.packages(
  pkgs_to_install,
  Ncpus = max(1L, parallel::detectCores())
)
