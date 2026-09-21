#!/usr/bin/env python3
"""Poll the SQLite catalog for write-lock availability and log every interval
in which it was held. Shows directly how long a DuckLake commit keeps the
catalog exclusive, which is what starves everyone else."""
import sqlite3, sys, time, json

path, out, interval = sys.argv[1], sys.argv[2], float(sys.argv[3])
t0 = time.time()
held_since = None
f = open(out, "w")
while True:
    try:
        con = sqlite3.connect(path, timeout=0, isolation_level=None)
        con.execute("PRAGMA busy_timeout=0")
        try:
            con.execute("BEGIN IMMEDIATE")
            con.execute("ROLLBACK")
            busy = False
        finally:
            con.close()
    except sqlite3.OperationalError:
        busy = True
    except Exception:
        busy = True
    now = time.time()
    if busy and held_since is None:
        held_since = now
    elif not busy and held_since is not None:
        f.write(json.dumps({"start": held_since - t0, "dur": now - held_since}) + "\n")
        f.flush()
        held_since = None
    time.sleep(interval)
