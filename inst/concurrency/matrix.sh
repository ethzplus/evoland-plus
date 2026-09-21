#!/usr/bin/env bash
# Repeat each variant N times, one line per run: does the data come out right?
set -uo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
reps="${REPS:-3}"
for v in "$@"; do
  for i in $(seq 1 "$reps"); do
    out=$(timeout 1800 "$here"/run.sh "$v" "${W:-8}" 2>&1)
    wall=$(grep -oE "done in [0-9.]+s" <<<"$out" | grep -oE "[0-9.]+")
    fails=$(grep -oE "failures: [0-9]+/[0-9]+" <<<"$out" | cut -d' ' -f2)
    keys=$(grep -qE "DUPLICATE|collisions" <<<"$out" && echo DUP || echo ok)
    comp=$(grep -qE "do not have exactly" <<<"$out" && echo LOST || echo ok)
    lost=$(grep -oE "present: [0-9e+.]+" <<<"$out" | tail -1)
    printf "%-30s rep%-2s wall=%-7s workers_failed=%-6s keys=%-4s complete=%-5s %s\n" \
      "$v" "$i" "$wall" "$fails" "$keys" "$comp" "$lost"
  done
done
