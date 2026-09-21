#!/usr/bin/env bash
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Keep the logs of finished runs, drop their parquet, so the matrix fits on disk.
for d in "$here"/runs/*/; do
  if [[ -f "$d/all.jsonl" ]]; then rm -rf "$d/data" "$d/catalog.sqlite"*; fi
done
du -sh "$here"/runs
