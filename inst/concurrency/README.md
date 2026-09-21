# DuckLake concurrency harness

Multi-process tests for the `ducklake_db` write path. Deliberately not part of
`inst/tinytest`: each variant spawns several OS processes and writes millions of
rows, which takes minutes, and what it is measuring — starvation, lock
convoying, silent write loss — only appears with real concurrency.

It reimplements the parts of `ducklake_db` that concurrency touches
(`mini.R`: the retry wrapper, `transaction()`, the upsert and append commit
paths) rather than loading the package, so a variant can change one of them
without a rebuild, and so the harness still runs against a version of the
package that predates a fix.

`worker.R` stands in for one `020-ingest-preds-*.qmd` step: it registers
`LAKELAB_NPRED` predictors, each a small `pred_meta_t` upsert inside a
transaction followed by a large `pred_data_t` write, which is what
`add_predictor()` does.

## Running

    ./run.sh <variant> <n_workers>        # one run, printing a correctness check
    REPS=3 W=8 ./matrix.sh <variant>...   # repeat, one line per run
    ./summary.R <variant>...              # compare finished runs
    ./analyse.R <variant>                 # per-worker detail for one run
    ./keeplogs.sh                         # drop the parquet, keep the logs

A run leaves `runs/<variant>/`: `all.jsonl` (every catalog attempt, with
timings), `locks.jsonl` (each interval the SQLite catalog was write-locked,
sampled at 20 ms by `watch_lock.py` — this is what shows convoying), and
`verify.out`.

Variants whose name starts with `postgres` or `correctness-postgres` need a
reachable PostgreSQL; point `LAKELAB_PG` at it, and note that `run.sh` drops
and recreates its `public` schema.

## What a variant can change

`env/_base.sh` lists every knob and its default. The ones that matter:

| variable | values | what it is for |
| --- | --- | --- |
| `LAKELAB_CATALOG` | `sqlite:...`, `postgres:...` | which catalog backs the lake |
| `LAKELAB_ATTACH_OPTS` | e.g. `METADATA_PARAMETERS MAP{'journal_mode':'wal'}` | extra DuckLake attach options |
| `LAKELAB_MODE`, `LAKELAB_META_MODE` | `upsert`, `append` | write path per table |
| `LAKELAB_ALLOC` | `max`, `counter`, `disjoint` | how `id_pred` is handed out: `max(id_pred) + 1` as `add_predictor()` does it, through a row every allocator must update, or not at all (each worker owns a block) |
| `LAKELAB_CREATE_MODE` | `replace`, `if_not_exists` | what the first write to a missing table does |
| `LAKELAB_BIG_IN_TXN` | `0`, `1` | whether the `pred_data_t` write shares the metadata transaction |
| `LAKELAB_BACKOFF` | `current`, `capped`, `flat`, `decorrelated` | retry curve |
| `LAKELAB_RETRY_MAX`, `LAKELAB_RETRY_TIMEOUT` | | attempt cap vs. wall-clock budget |

## What it found

Measured on four cores, 16 concurrent writers, 2M rows per predictor over an
8M-row table, unless noted.

| variant | wall | workers lost | catalog write-locked |
| --- | --- | --- | --- |
| `sqlite-journal` | 116s | 1 of 16, after 20 retries | 82s of 116s |
| `sqlite-wal` | 59s | none | 1.7s of 59s |
| `postgres` | 17s | none | n/a |

A SQLite catalog in its default rollback-journal mode is the starvation. A
writer that cannot take the exclusive lock at once waits in SQLite's busy
handler holding the PENDING lock, and PENDING stops other processes *reading*
the catalog — so contention surfaces in a process that was only reading, as
`Failed to query most recent snapshot for DuckLake: ... database is locked`.
Each collision costs about a second, which causes the next. `busy_timeout` is
accepted as an attach option and has no effect.

Correctness, at 8 workers with `LAKELAB_NCOORD=200000`, reproducible every run:

| variant | `pred_meta_t` keys | `pred_data_t` |
| --- | --- | --- |
| `correctness-postgres` | 24 predictors share 16 `id_pred` | 800k duplicate rows |
| `correctness-postgres-append` | 24 share 10 | 2.8M duplicate rows |
| `correctness-postgres-no-inlining` | 24 share 14 | duplicates |
| `correctness-postgres-counter` | unique | clean |
| `correctness-postgres-disjoint` | unique | clean |
| `correctness-postgres-fixed` | unique | clean |

None of the corrupting runs reported an error. Two separate causes:

- `add_predictor()` takes `id_pred` from `max(id_pred) + 1`, a read-then-write
  across processes. A SQLite catalog serialises the transactions and so hides
  it; PostgreSQL gives each its own snapshot, and DuckLake does not count
  "something was inserted since I read the maximum" as a conflict. Allocating
  through a row every allocator updates (`LAKELAB_ALLOC=counter`) makes the two
  conflict, so one retries and re-reads.
- the first write to a table that does not exist yet was `create or replace`,
  so two writers that both found it missing destroyed each other's rows
  (`LAKELAB_CREATE_MODE=replace` against `if_not_exists`). Fixed in
  `ducklake_db`; `correctness-*-disjoint` is the variant that isolates it,
  since it removes the `id_pred` race.
