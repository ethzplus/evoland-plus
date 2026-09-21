#!/usr/bin/env Rscript
# Resolve this harness's own directory, so it runs from anywhere.
here <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[[1L]]))) else getwd()
}
runs_dir <- function() file.path(here(), "runs")


suppressPackageStartupMessages(library(data.table))
options(width = 210)
parse1 <- function(l) {
  kv <- regmatches(l, gregexpr('"[^"]+":(("[^"]*")|(-?[0-9.]+))', l))[[1]]
  k <- sub('^"([^"]+)":.*$', "\\1", kv)
  v <- sub('^"[^"]+":"?([^"]*)"?$', "\\1", kv)
  setNames(as.list(v), k)
}
load_run <- function(v) {
  f <- file.path(runs_dir(), v, "all.jsonl")
  if (!file.exists(f)) return(NULL)
  d <- rbindlist(lapply(readLines(f, warn = FALSE), parse1), fill = TRUE)
  for (col in c("t","attempt","attempt_s","total_s","wait_s","rows","status")) {
    if (col %in% names(d)) d[, (col) := as.numeric(get(col))] else d[, (col) := NA_real_]
  }
  if (!"event" %in% names(d)) d[, event := NA_character_]
  lf <- file.path(runs_dir(), v, "locks.jsonl")
  locks <- if (file.exists(lf) && length(readLines(lf, warn = FALSE))) {
    ln <- readLines(lf, warn = FALSE)
    durs <- as.numeric(sub('^.*"dur":\\s*([0-9.eE+-]+).*$', "\\1", ln))
    durs <- durs[!is.na(durs)]
    if (length(durs)) data.table(held_s = sum(durs), max_hold = max(durs))
    else data.table(held_s = NA_real_, max_hold = NA_real_)
  } else data.table(held_s = NA_real_, max_hold = NA_real_)
  W <- d[worker != "prep"]
  data.table(
    variant       = v,
    workers       = uniqueN(W[event == "worker_start", worker]),
    wall_s        = round(max(W$t, na.rm = TRUE), 1),
    ok_workers    = sum(W[event == "worker_end", status] == 0),
    failed_workers= sum(W[event == "worker_end", status] != 0),
    gave_up       = sum(W$event == "gave_up"),
    transients    = sum(W$event == "transient"),
    lost_s        = round(sum(W[event == "transient", attempt_s], na.rm = TRUE) +
                          sum(W[event == "backoff", wait_s], na.rm = TRUE), 1),
    mean_fail_s   = round(mean(W[event == "transient", attempt_s]), 2),
    max_attempt   = suppressWarnings(max(W[event == "transient", attempt], na.rm = TRUE)),
    catalog_held_s= round(locks$held_s, 1),
    max_hold_s    = round(locks$max_hold, 2)
  )
}
vs <- commandArgs(TRUE)
if (!length(vs)) vs <- list.files(runs_dir())
out <- rbindlist(lapply(vs, load_run), fill = TRUE)
print(out, row.names = FALSE)
