# Test default constructor: the base run, with every standard column
x <- as_runs_t()
expect_inherits(x, "runs_t")
expect_equal(
  names(x),
  c(
    "id_run",
    "parent_id_run",
    "description",
    "kind",
    "member",
    "seed",
    "created_by",
    "created_at",
    "evoland_version",
    "attributes"
  )
)
expect_equal(x$id_run, 0L)
expect_equal(x$kind, "base")
expect_true(is.na(x$seed) && is.integer(x$seed))
expect_inherits(x$created_at, "POSIXct")
expect_equal(attr(x, "key_cols"), "id_run")
expect_equal(attr(x, "map_cols"), "attributes")

# Missing standard columns are added; ad hoc columns are kept, after the standard ones
legacy <- as_runs_t(data.frame(
  id_run = c(0, 1),
  parent_id_run = c(NA, 0),
  description = c("Base", "Scenario 1"),
  ssp = c(NA, "SSP1")
))
expect_equal(names(legacy)[1:10], names(x))
expect_equal(legacy$ssp, c(NA, "SSP1"))
expect_true(all(is.na(legacy$member)))

# member must be a positive index
expect_error(
  as_runs_t(list(
    id_run = 0:1,
    parent_id_run = c(NA, 0L),
    description = c("a", "b"),
    member = c(NA, 0L)
  )),
  "member must be >= 1 or NA"
)

# Test constructor with custom data, which should be coerced and validated
runs_t <-
  data.frame(
    id_run = c(0, 1),
    parent_id_run = c(NA, 0),
    description = c("Base", "Scenario 1")
  ) |>
  as_runs_t()

# Test Validation Logic
runs_t$id_run <- as.double(runs_t$id_run)
expect_error(validate(runs_t), pattern = "id_run is not integer")

# Missing id_run 0 (Base)
expect_error(
  as_runs_t(data.frame(
    id_run = 1L,
    parent_id_run = 0L,
    description = "No Base"
  )),
  "all parent_id_run must be in id_run or NA"
)

# Duplicate id_run
expect_error(
  as_runs_t(
    bad_dupe <- data.frame(
      id_run = c(0L, 0L),
      parent_id_run = c(NA_integer_, NA_integer_),
      description = c("Base", "Duplicate")
    )
  ),
  "Duplicates found"
)

# Test Lineage Logic (get_lineage)

# Setup a hierarchy table
# genealogy 0 -> 1 -> 2
hier_runs <- as_runs_t(list(
  id_run = c(0L, 1L, 2L),
  parent_id_run = c(NA_integer_, 0L, 1L),
  description = c("Base", "Child", "Grandchild")
))

# Lineage for Base, Child, and Grandchild
expect_equal(evoland:::get_lineage(hier_runs, 0L), 0L)
expect_equal(evoland:::get_lineage(hier_runs, 1L), 1:0)
expect_equal(evoland:::get_lineage(hier_runs, 2L), 2:0)

# error for non-existent id_run
expect_error(
  evoland:::get_lineage(hier_runs, 999L),
  pattern = "Requested run \\(id_run = 999\\) not found in runs_t"
)

# Broken chain (parent doesn't exist)
expect_error(
  as_runs_t(list(
    id_run = c(0L, 2L),
    parent_id_run = c(NA_integer_, 1L), # Parent 1 is missing
    description = c("Base", "Orphan")
  )),
  "all parent_id_run must be in id_run or NA"
)


# --- add_runs, run_attributes_v and run seeds, on a database -----------------------------
db_path <- tempfile("runs_t_test_")
db <- evoland_db$new(path = db_path)
expect_equal(db$runs_t$kind, "base")

options(evoland.created_by = "test_runs_t.R")
scenario <- db$add_runs(
  parent_id_run = 0L,
  description = "Scenario A",
  kind = "scenario",
  attributes = list(ssp = "SSP1", climate = "gwl")
)
expect_equal(scenario$id_run, 1L) # allocated after the base run
expect_equal(scenario$created_by, "test_runs_t.R")
expect_equal(scenario$evoland_version, as.character(utils::packageVersion("evoland")))
expect_false(is.na(scenario$created_at))

members <- db$add_runs(
  parent_id_run = scenario$id_run,
  description = paste("realisation", 1:3),
  kind = "realisation",
  member = 1:3,
  seed = 100L + 1:3,
  # the last member overrides one inherited attribute
  attributes = list(NULL, NULL, list(climate = "current"))
)
options(evoland.created_by = NULL)
expect_equal(members$id_run, 2:4)
expect_equal(members$member, 1:3)
expect_equal(members$seed, 101:103)
expect_equal(nrow(db$runs_t), 5L)
expect_equal(db$runs_t[id_run == 3L, seed], 102L) # round trip through the database
expect_equal(db$runs_t[id_run == 1L, attributes][[1]], list(ssp = "SSP1", climate = "gwl"))
expect_error(db$add_runs(parent_id_run = 99L, description = "orphan"), "must exist in runs_t")

# attributes resolve along the lineage, nearest run first
attrs <- db$run_attributes_v()
expect_equal(attrs[id_run == 0L, ssp], NA_character_)
expect_equal(attrs[id_run == 2L, ssp], "SSP1")
expect_equal(attrs[id_run == 2L, climate], "gwl")
expect_equal(attrs[id_run == 4L, climate], "current")
long <- db$run_attributes_v(wide = FALSE)
expect_equal(long[id_run == 4L & key == "climate", from_id_run], 4L)
expect_equal(long[id_run == 4L & key == "ssp", from_id_run], 1L)

# reopening the database keeps the base run as it is, instead of resetting it
base_run <- db$runs_t[id_run == 0L]
# one row, one named list: the outer list() is the column
data.table::set(base_run, j = "attributes", value = list(list(list(note = "kept"))))
db$commit(as_runs_t(base_run), "runs_t", method = "upsert")
db2 <- evoland_db$new(path = db_path)
expect_equal(db2$runs_t[id_run == 0L, attributes][[1]], list(note = "kept"))

# per-period seeds: distinct per period, and runs with consecutive seeds do not share streams
expect_equal(evoland:::run_period_seed(101L, 4L), evoland:::run_period_seed(101L, 4L))
expect_false(evoland:::run_period_seed(101L, 4L) == evoland:::run_period_seed(101L, 5L))
expect_false(evoland:::run_period_seed(101L, 5L) == evoland:::run_period_seed(102L, 4L))

# seeding from the run restores the caller's RNG state afterwards
db$id_run <- 2L
set.seed(1)
before <- .Random.seed
restore <- evoland:::seed_rng_from_active_run(db, 4L)
expect_false(identical(.Random.seed, before))
restore()
expect_identical(.Random.seed, before)
db$id_run <- 1L # no seed: nothing to restore
expect_null(evoland:::seed_rng_from_active_run(db, 4L))

# --- migration of a runs_t written before the standard columns ----------------------------
old_path <- tempfile("runs_t_legacy_")
old <- ducklake_db$new(path = old_path)
old$commit(
  as_ducklake_db_t(
    data.table::data.table(
      id_run = 0:1,
      parent_id_run = c(NA, 0L),
      description = c("Base", "legacy child"),
      ssp = c(NA, "SSP3")
    ),
    key_cols = "id_run"
  ),
  "runs_t",
  method = "overwrite"
)
rm(old)
migrated <- evoland_db$new(path = old_path)
expect_equal(migrated$runs_t$description, c("Base", "legacy child"))
expect_equal(migrated$runs_t$ssp, c(NA, "SSP3"))
# and upserts with the standard columns work afterwards
expect_silent(
  migrated$add_runs(parent_id_run = 1L, description = "new child", kind = "realisation")
)
expect_equal(nrow(migrated$runs_t), 3L)
