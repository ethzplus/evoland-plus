#!/usr/bin/env Rscript
# Summarise a run's JSONL into the numbers that matter: who got through, who
# starved, how much wall time went to blind SQLITE_BUSY waits vs. our backoff.
# Resolve this harness's own directory, so it runs from anywhere.
here <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[[1L]]))) else getwd()
}
runs_dir <- function() file.path(here(), "runs")


suppressPackageStartupMessages(library(data.table))
args <- commandArgs(TRUE)
f <- file.path(runs_dir(), args[1], "all.jsonl")
lines <- readLines(f, warn = FALSE)
parse1 <- function(l) {
  kv <- regmatches(l, gregexpr('"[^"]+":(("[^"]*")|(-?[0-9.]+))', l))[[1]]
  k <- sub('^"([^"]+)":.*$', "\\1", kv)
  v <- sub('^"[^"]+":"?([^"]*)"?$', "\\1", kv)
  setNames(as.list(v), k)
}
d <- rbindlist(lapply(lines, parse1), fill = TRUE)
for (col in intersect(names(d), c("t","wall","attempt","attempt_s","total_s","wait_s","rows","status","n_pred","n_coord")))
  d[, (col) := as.numeric(get(col))]
setorder(d, t)

cat("\n=== events ===\n"); print(d[, .N, by = event][order(-N)])

cat("\n=== per worker ===\n")
print(d[, .(
  end_status = { s <- .SD[event == "worker_end", status]; if (length(s)) as.numeric(s[1]) else NA_real_ },
  preds_done = sum(event == "predictor_done"),
  failed     = sum(event == "predictor_failed"),
  transients = sum(event == "transient"),
  busy_wait_s = round(sum(attempt_s[event == "transient"], na.rm = TRUE), 1),
  backoff_s   = round(sum(wait_s[event == "backoff"], na.rm = TRUE), 1),
  gave_up     = sum(event == "gave_up"),
  wall_s      = round(max(t, na.rm = TRUE), 1)
), by = worker][order(worker)])

cat("\n=== transient errors by label ===\n")
if (nrow(d[event == "transient"]))
  print(d[event == "transient", .(n = .N, mean_attempt_s = round(mean(attempt_s), 2),
                                  total_s = round(sum(attempt_s), 1)),
          by = .(label)][order(-n)])

cat("\n=== distinct transient messages ===\n")
if (nrow(d[event == "transient"]))
  print(d[event == "transient", .N, by = .(msg = substr(msg, 1, 110))][order(-N)])

cat("\n=== merge / commit durations ===\n")
mm <- d[event %in% c("merge_begin","merge_end","append_begin","append_end","txn_commit_begin","txn_commit_end")]
if (nrow(mm)) {
  mm[, phase := sub("_(begin|end)$", "", event)]
  mm[, side  := sub("^.*_", "", event)]
  setorder(mm, worker, t)
  mm[, pair := cumsum(side == "begin"), by = .(worker, phase)]
  print(mm[, .(dur = if (uniqueN(side) == 2) diff(range(t)) else NA_real_),
           by = .(worker, phase, pair)][!is.na(dur),
           .(n = .N, mean_s = round(mean(dur), 2), max_s = round(max(dur), 2)),
           by = .(worker, phase)][order(worker, phase)])
}

cat("\n=== gave_up detail ===\n")
if (nrow(d[event == "gave_up"])) print(d[event == "gave_up", .(worker, label, total_s, msg = substr(msg,1,120))])

cat("\n=== hard errors ===\n")
if (nrow(d[event == "hard_error"])) print(d[event == "hard_error", .(worker, label, msg = substr(msg,1,200))])
