# Create the catalog (and pred_data_t/pred_meta_t seed, if asked) before the
# workers start, so a run begins from the same state every time and the
# journal-mode switch has a file to act on.
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
seed <- as.integer(Sys.getenv("LAKELAB_SEED_ROWS", "0"))
if (seed > 0L) {
  db$upsert(data.table(id_pred = 0L, name = "seed", unit = "x", descr = "y"),
            "pred_meta_t", key_cols = "id_pred")
  db$upsert(data.table(id_run = 1L, id_pred = 0L, id_coord = seq_len(seed),
                       id_period = 0L, value = runif(seed)),
            "pred_data_t", key_cols = c("id_run","id_pred","id_coord","id_period"))
}
if (grepl("^counter", Sys.getenv("LAKELAB_ALLOC", "max")) && Sys.getenv("LAKELAB_ALLOC") != "counter-lazy") {
  db$upsert(data.table(table_name = "pred_meta_t", next_id = 1L),
            "id_alloc_t", key_cols = "table_name")
}
db$close()
cat("prepped\n")
