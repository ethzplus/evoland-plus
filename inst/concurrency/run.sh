#!/usr/bin/env bash
# Usage: ./run.sh <variant-name> <n_workers>
set -uo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
variant="$1"; workers="${2:-5}"
lab="$here/runs/$variant"
rm -rf "$lab"; mkdir -p "$lab/data" "$lab/logs"

export LAKELAB_DATA_PATH="$lab/data/"
export LAKELAB_JOURNAL_MODE=""
export LAKELAB_SEED_ROWS=0
export LAKELAB_PGSETUP=""
# shellcheck disable=SC1090
source "$here/env/$variant.env"

cd "$here"

if [[ -n "$LAKELAB_PGSETUP" ]]; then
  psql "$LAKELAB_PGSETUP" -c "drop schema if exists public cascade; create schema public;" >/dev/null
fi

LAKELAB_LOG="$lab/logs/prep.jsonl" LAKELAB_WORKER=prep \
  Rscript "$here"/prep.R >"$lab/logs/prep.out" 2>&1 \
  || { echo "prep failed"; tail -20 "$lab/logs/prep.out"; exit 1; }

if [[ -n "$LAKELAB_JOURNAL_MODE" ]]; then
  cat="${LAKELAB_CATALOG#sqlite:}"
  python3 - "$cat" "$LAKELAB_JOURNAL_MODE" <<'PY'
import sqlite3, sys
con = sqlite3.connect(sys.argv[1])
print("journal_mode ->", con.execute("pragma journal_mode=%s" % sys.argv[2]).fetchone()[0])
con.close()
PY
fi

echo "== variant $variant, $workers workers, catalog=$LAKELAB_CATALOG"
watcher=""
if [[ "$LAKELAB_CATALOG" == sqlite:* ]]; then
  python3 $here/watch_lock.py "${LAKELAB_CATALOG#sqlite:}" "$lab/locks.jsonl" 0.02 &
  watcher=$!
fi
start=$(date +%s.%N)
pids=()
for i in $(seq 1 "$workers"); do
  w="w$i"
  ( LAKELAB_WORKER="$w" LAKELAB_LOG="$lab/logs/$w.jsonl" \
    Rscript "$here"/worker.R >"$lab/logs/$w.out" 2>&1 ) &
  pids+=($!)
done
fail=0
for p in "${pids[@]}"; do wait "$p" || fail=$((fail+1)); done
end=$(date +%s.%N)
if [[ -n "$watcher" ]]; then kill "$watcher" 2>/dev/null; fi
cat "$lab"/logs/*.jsonl > "$lab/all.jsonl"
if [[ "${LAKELAB_VERIFY:-1}" == "1" ]]; then
  LAKELAB_LOG="" LAKELAB_WORKER=verify Rscript "$here"/verify.R \
    >"$lab/verify.out" 2>&1 || echo "verify failed"
  grep -vE "^i |^\* |temporary directory" "$lab/verify.out" | sed 's/^/   /'
fi
printf '== done in %.1fs, failures: %d/%d\n' "$(echo "$end - $start" | bc)" "$fail" "$workers"
echo "$variant $(echo "$end - $start" | bc) $fail $workers" >> $here/results.txt
