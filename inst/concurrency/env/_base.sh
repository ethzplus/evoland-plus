# Defaults every variant starts from. A variant's own file is sourced after
# this one and overrides whatever it names.
#
# NCOORD x NPRED x workers rows get written per run, so the defaults are sized
# to contend visibly on four cores in about a minute. Raise NCOORD towards the
# 8.3M of a 100 m grid over Switzerland to reproduce what a real ingest stage
# does to the catalog.
export LAKELAB_NPRED=3
export LAKELAB_NCOORD=2000000
export LAKELAB_SEED_ROWS=8000000
export LAKELAB_MODE=upsert          # upsert | append
export LAKELAB_META_MODE=upsert     # upsert | append
export LAKELAB_ALLOC=max            # max | counter | disjoint
export LAKELAB_CREATE_MODE=replace  # replace | if_not_exists
export LAKELAB_BIG_IN_TXN=0
export LAKELAB_BACKOFF=current      # current | capped | flat | decorrelated
export LAKELAB_RETRY_MAX=20
export LAKELAB_RETRY_WAIT=0.1
export LAKELAB_RETRY_TIMEOUT=1e9
export LAKELAB_THREADS=1
