# Test default constructor
x <- data.table::data.table(
  id_run = 0L,
  parent_id_run = NA_integer_,
  description = "Base"
)

data.table::setattr(x, "class", c("runs_t", "parquet_db_t", "data.table", "data.frame"))
data.table::setattr(x, "key_cols", "id_run")
data.table::setkeyv(x, "id_run")
expect_equal(as_runs_t(), x)


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
  "no base.*0.*id_run"
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
  "duplicated id_run"
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
broken_runs <- as_runs_t(list(
  id_run = c(0L, 2L),
  parent_id_run = c(NA_integer_, 1L), # Parent 1 is missing
  description = c("Base", "Orphan")
))

expect_error(
  evoland:::get_lineage(broken_runs, 2L),
  pattern = "Parent for id_run = 1 not found; base runs should have parent_id_run = NA"
)
