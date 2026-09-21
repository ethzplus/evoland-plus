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
| `LAKELAB_ALLOC` | `max`, `counter`, `counter-lazy`, `disjoint` | how `id_pred` is handed out: `max(id_pred) + 1`, through a row every allocator must update (seeded by `prep.R`, or on first use for `counter-lazy`), or not at all (each worker owns a block) |
| `LAKELAB_DUCKLAKE_RETRY` | e.g. `0` | `ducklake_max_retry_count`; set to 0 to tell DuckLake's own commit replay apart from `transaction()`'s |
| `LAKELAB_INLINE_LIMIT` | e.g. `0` | `ducklake_default_data_inlining_row_limit` |
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

- `add_predictor()` took `id_pred` from `max(id_pred) + 1`, a read-then-write
  across processes. A SQLite catalog serialises the transactions and so hides
  it; PostgreSQL gives each its own snapshot, and DuckLake does not count
  "something was inserted since I read the maximum" as a conflict. Allocating
  through a row every allocator updates (`LAKELAB_ALLOC=counter`) makes the two
  conflict, so one retries and re-reads. Fixed in `ducklake_db$next_id()`.
- the first write to a table that does not exist yet was `create or replace`,
  so two writers that both found it missing destroyed each other's rows
  (`LAKELAB_CREATE_MODE=replace` against `if_not_exists`). Fixed in
  `ducklake_db`; `correctness-*-disjoint` is the variant that isolates it,
  since it removes the `id_pred` race.

## Allocating ids: why a row and not a sequence

The `alloc-stress-*` variants isolate the allocation: 16 writers registering 6
predictors each, 2000 rows apiece, so the ids are what collides rather than the
data. 96 predictors expected.

| variant | distinct `id_pred` | dense | `pred_data_t` | wall |
| --- | --- | --- | --- | --- |
| `alloc-stress-postgres-max` | **52** of 96 | no | duplicate rows | 19s |
| `alloc-stress-postgres-counter` | 96 | 1..96 | clean | 26s |
| `alloc-stress-postgres-counter-lazy` | 96 | 1..96 | clean | 27s |
| `alloc-stress-postgres-counter-nodlretry` | 96 | 1..96 | clean | 35s |
| `alloc-stress-sqlite-max` | 96 | 1..96 | clean | 26s |
| `alloc-stress-sqlite-counter` | 96 | 1..96 | clean | 25s |

`sqlite-max` looks fine only because SQLite's single writer serialises the
transactions. That is the property WAL mode and a server-backed catalog exist to
give up, so it is not one to build on.

`-nodlretry` sets `ducklake_max_retry_count=0`. It still comes out clean, which
matters: it means the correctness comes from `transaction()` replaying the block
and *re-reading* the counter, not from DuckLake's internal commit replay, which
would not re-run the read.

The counter costs 32% more wall time here on PostgreSQL and nothing measurable
on SQLite — on a workload that is nothing but allocation. A real ingest step
writes millions of rows per predictor, so it disappears.

A catalog-native sequence would be the obvious alternative. It is not reachable:

- `create sequence` is refused by DuckDB on a SQLite attachment ("SQLite
  databases do not support creating sequences") and on a PostgreSQL one. A
  server-side sequence can be created and read through `postgres_execute` /
  `postgres_query`, but only there — SQLite has no sequences at all, and a
  DuckDB-file catalog cannot even be attached twice in one process ("Unique
  file handle conflict").
- decisively, one DuckDB transaction cannot write to both an attached catalog
  and the lake: "Attempting to write to database ... in a transaction that has
  already modified database ...". So a sequence could not roll back with the
  write it numbers — every retry would burn an id, and the retries are the
  normal case under contention.

Putting a table of our own in the catalog is otherwise tolerated: it survives
`checkpoint` with `expire_older_than`/`delete_older_than` set to zero, and does
not appear in DuckLake's own table listing. The blockers above are what rule
the approach out, not the catalog objecting.
