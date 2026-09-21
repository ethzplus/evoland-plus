# One "020-ingest-preds-*.qmd" stand-in: register K predictors, each a small
# metadata upsert inside a transaction plus a large data upsert outside it,
# exactly as add_predictor() does.
# Resolve this harness's own directory, so it runs from anywhere.
here <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[[1L]]))) else getwd()
}
runs_dir <- function() file.path(here(), "runs")


suppressPackageStartupMessages({library(data.table); library(R6)})
source(file.path(here(), "mini.R"))

cfg <- list(
  catalog   = Sys.getenv("LAKELAB_CATALOG"),
  data_path = Sys.getenv("LAKELAB_DATA_PATH"),
  worker    = Sys.getenv("LAKELAB_WORKER"),
  n_pred    = as.integer(Sys.getenv("LAKELAB_NPRED", "3")),
  n_coord   = as.integer(Sys.getenv("LAKELAB_NCOORD", "300000")),
  big_in_txn = Sys.getenv("LAKELAB_BIG_IN_TXN", "0") == "1",
  mode      = Sys.getenv("LAKELAB_MODE", "upsert"),   # upsert | append
  backoff   = Sys.getenv("LAKELAB_BACKOFF", "current"),
  retry_max = as.integer(Sys.getenv("LAKELAB_RETRY_MAX", "20")),
  retry_wait = as.numeric(Sys.getenv("LAKELAB_RETRY_WAIT", "0.1")),
  threads   = {v <- Sys.getenv("LAKELAB_THREADS", ""); if (nzchar(v)) as.integer(v) else NULL},
  attach_opts = Sys.getenv("LAKELAB_ATTACH_OPTS", ""),
  retry_timeout = as.numeric(Sys.getenv("LAKELAB_RETRY_TIMEOUT", "1e9"))
)

jlog("worker_start", catalog = cfg$catalog, mode = cfg$mode,
     big_in_txn = as.integer(cfg$big_in_txn), n_pred = cfg$n_pred,
     n_coord = cfg$n_coord, backoff = cfg$backoff)

db <- mini_db$new(cfg$catalog, cfg$data_path, retry_max = cfg$retry_max,
                  retry_wait = cfg$retry_wait, backoff = cfg$backoff,
                  threads = cfg$threads, attach_opts = cfg$attach_opts,
                  retry_timeout = cfg$retry_timeout)

# deterministic but worker-specific predictor names
pred_names <- sprintf("%s_pred_%02d", cfg$worker, seq_len(cfg$n_pred))

# Two ways to hand out the next id_pred. `max` is what add_predictor() does
# today; `counter` puts the allocation through a row every allocator must
# update, so two concurrent allocations conflict and one is made to retry.
alloc_mode <- Sys.getenv("LAKELAB_ALLOC", "max")

# `disjoint` sidesteps allocation entirely: each worker owns a private block of
# ids, so any missing or duplicated row is the write path's doing, not a race
# over the id.
worker_index <- as.integer(sub("^w", "", Sys.getenv("LAKELAB_WORKER", "w1")))
disjoint_seq <- 0L

next_id_pred <- function(db) {
  if (alloc_mode == "disjoint") {
    disjoint_seq <<- disjoint_seq + 1L
    return(worker_index * 1000L + disjoint_seq)
  }
  if (alloc_mode == "max") return(db$column_max("pred_meta_t", "id_pred") + 1L)
  if (!"id_alloc_t" %in% db$list_tables()) {
    # created by prep.R; a worker finding it absent means prep did not run
    stop("id_alloc_t missing")
  }
  cur <- db$get_query(sprintf(
    "select next_id from %s.id_alloc_t where table_name = 'pred_meta_t'", CATALOG_ALIAS
  ), label = "alloc_read")$next_id[1L]
  db$execute(sprintf(
    "update %s.id_alloc_t set next_id = %d where table_name = 'pred_meta_t'",
    CATALOG_ALIAS, as.integer(cur) + 1L
  ), label = "alloc_write")
  as.integer(cur)
}

add_predictor <- function(db, pred_data_raw, name) {
  id_pred <- NULL
  db$transaction({
    id_pred <- next_id_pred(db)
    if (id_pred > 1L && "pred_meta_t" %in% db$list_tables()) {
      existing <- db$get_query(sprintf(
        "select id_pred from %s.pred_meta_t where name = '%s'", CATALOG_ALIAS, name
      ), label = "fetch_meta")
      if (nrow(existing) > 0L) id_pred <- existing$id_pred[1L]
    }
    meta <- data.table(id_pred = as.integer(id_pred), name = name,
                       unit = "x", descr = "y")
    if (Sys.getenv("LAKELAB_META_MODE", "upsert") == "append") {
      db$append(meta, "pred_meta_t")
    } else {
      db$upsert(meta, "pred_meta_t", key_cols = "id_pred")
    }
    if (cfg$big_in_txn) {
      d <- copy(pred_data_raw)[, `:=`(id_pred = as.integer(id_pred), id_run = 1L)]
      if (cfg$mode == "append") db$append(d, "pred_data_t")
      else db$upsert(d, "pred_data_t", key_cols = c("id_run", "id_pred", "id_coord", "id_period"))
    }
  })
  if (!cfg$big_in_txn) {
    d <- copy(pred_data_raw)[, `:=`(id_pred = as.integer(id_pred), id_run = 1L)]
    if (cfg$mode == "append") db$append(d, "pred_data_t")
    else db$upsert(d, "pred_data_t", key_cols = c("id_run", "id_pred", "id_coord", "id_period"))
  }
  invisible(id_pred)
}

status <- 0L
for (nm in pred_names) {
  pred_data <- data.table(
    id_coord = seq_len(cfg$n_coord),
    id_period = 0L,
    value = runif(cfg$n_coord)
  )
  jlog("predictor_start", name = nm)
  r <- try(add_predictor(db, pred_data, nm), silent = TRUE)
  if (inherits(r, "try-error")) {
    jlog("predictor_failed", name = nm,
         msg = substr(conditionMessage(attr(r, "condition")), 1, 300))
    status <- 1L
    break
  }
  jlog("predictor_done", name = nm)
}

db$close()
jlog("worker_end", status = status)
quit(status = status)
