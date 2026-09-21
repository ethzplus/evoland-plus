#!/usr/bin/env Rscript
# After a run: is the data actually correct? The id_pred allocation in
# add_predictor() is a read-then-write (max(id_pred) + 1) across processes, so
# two workers can pick the same id. Whether that shows up depends entirely on
# whether the write path detects the conflict.
# Resolve this harness's own directory, so it runs from anywhere.
here <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[[1L]]))) else getwd()
}
runs_dir <- function() file.path(here(), "runs")


suppressPackageStartupMessages({library(data.table); library(R6)})
source(file.path(here(), "mini.R"))
db <- mini_db$new(Sys.getenv("LAKELAB_CATALOG"), Sys.getenv("LAKELAB_DATA_PATH"),
                  attach_opts = Sys.getenv("LAKELAB_ATTACH_OPTS", ""))
meta <- db$get_query(sprintf("select id_pred, name from %s.pred_meta_t order by id_pred, name", CATALOG_ALIAS))
cat("pred_meta_t rows:", nrow(meta), "\n")
cat("distinct id_pred:", uniqueN(meta$id_pred), "  distinct name:", uniqueN(meta$name), "\n")
dup_id <- meta[, .N, by = id_pred][N > 1]
dup_nm <- meta[, .N, by = name][N > 1]
if (nrow(dup_id)) { cat("!! id_pred collisions:\n"); print(dup_id); print(meta[id_pred %in% dup_id$id_pred]) }
if (nrow(dup_nm)) { cat("!! name collisions:\n"); print(dup_nm) }
if (!nrow(dup_id) && !nrow(dup_nm)) cat("pred_meta_t: OK, unique on both keys\n")

dat <- db$get_query(sprintf(
  "select id_pred, count(*) n, count(distinct (id_run, id_pred, id_coord, id_period)) nd
   from %s.pred_data_t group by id_pred order by id_pred", CATALOG_ALIAS))
cat("\npred_data_t: ", nrow(dat), "distinct id_pred,", sum(dat$n), "rows\n")
bad <- dat[n != nd]
if (nrow(bad)) {
  cat("!! pred_data_t DUPLICATE primary keys under id_pred:", toString(bad$id_pred), "\n")
  cat("!! ", sum(bad$n - bad$nd), "surplus rows\n")
} else {
  cat("pred_data_t: OK, no duplicate primary keys\n")
}
# every registered predictor must have exactly its rows present
expect <- as.integer(Sys.getenv("LAKELAB_NCOORD", "0"))
if (expect > 0) {
  joined <- merge(meta[, .(id_pred)], dat, by = "id_pred", all.x = TRUE)
  joined[is.na(n), n := 0]
  missing <- joined[n != expect]
  if (nrow(missing)) {
    cat("!! ", nrow(missing), "of", nrow(joined),
        "predictors do not have exactly", expect, "rows:\n")
    print(head(missing, 12))
    cat("!!  rows expected:", nrow(joined) * expect, " present:", sum(dat$n), "\n")
  } else {
    cat("completeness: OK, all", nrow(joined), "predictors have", expect, "rows\n")
  }
}
db$close()
