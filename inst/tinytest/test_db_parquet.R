# Test generic parquet_db functionality
library(tinytest)

# Create temporary directory for testing
test_dir <- tempfile("parquet_db_test_")

# Test 1: Initialization
expect_silent(
  db <- parquet_db$new(
    path = test_dir
  )
)
expect_inherits(db, "parquet_db")
expect_true(dir.exists(test_dir))
expect_true(!is.null(db$connection))
expect_inherits(db$connection, "duckdb_connection")

# Test 2: Initial state - no tables
expect_identical(db$list_tables(), character(0))

# Test 3: Fetch for nonexistent errors out
expect_error(db$fetch("nonexistent_table"), "does not exist")

# Test 4: Row count for non-existent table
expect_equal(db$row_count("nonexistent_table"), 0L)

# Test 5: commit overwrite creates new table
test_data_1 <- data.table::data.table(
  id = 1:5,
  name = letters[1:5],
  value = c(10.1, 20.2, 30.3, 40.4, 50.5)
)
expect_silent(
  db$commit(
    method = "overwrite",
    test_data_1,
    "test_table_1"
  )
)
expect_true("test_table_1" %in% db$list_tables())
expect_equal(db$row_count("test_table_1"), 5L)

# Test 6: Fetch retrieves committed data
retrieved <- db$fetch("test_table_1")
expect_equal(retrieved, test_data_1)

# Test 7: commit overwrite replaces existing data
test_data_1b <- data.table::data.table(
  id = 10:12,
  name = letters[24:26],
  value = c(100.1, 200.2, 300.3)
)
expect_silent(
  db$commit(
    method = "overwrite",
    test_data_1b,
    "test_table_1"
  )
)
expect_equal(db$row_count("test_table_1"), 3L)
retrieved <- db$fetch("test_table_1")
expect_equal(retrieved, test_data_1b)

# Test 8: commit append adds to existing data
test_data_1c <- data.table::data.table(
  id = 13:15,
  name = letters[1:3],
  value = c(111.1, 222.2, 333.3)
)
expect_silent(
  db$commit(test_data_1c, "test_table_1", method = "append")
)
expect_equal(db$row_count("test_table_1"), 6L)
retrieved <- db$fetch("test_table_1")
expect_equal(nrow(retrieved), 6L)
expect_true(all(c(10:15) %in% retrieved$id))

# Test 9: commit append on non-existent table creates it
expect_silent(
  db$commit(test_data_1, "test_table_2", method = "append")
)
expect_true("test_table_2" %in% db$list_tables())
expect_equal(db$row_count("test_table_2"), 5L)

# Test 10: commit w/ upsert on non-existent table creates it
test_data_3 <- data.table::data.table(
  id_key = 1:3,
  name = c("a", "b", "c"),
  value = c(1.1, 2.2, 3.3)
)
data.table::setattr(test_data_3, "key_cols", "id_key")
expect_silent({
  db$commit(test_data_3, "test_table_3", method = "upsert")
  db$commit(test_data_3, "test_table_3", method = "upsert")
})
expect_true("test_table_3" %in% db$list_tables())
expect_equal(db$row_count("test_table_3"), 3L)

# Test 11: commit w/ upsert updates existing rows and inserts new ones
test_data_3b <- data.table::data.table(
  id_key = c(2L, 3L, 4L),
  name = c("b_updated", "c_updated", "d"),
  value = c(22.2, 33.3, 44.4)
)
expect_silent(
  db$commit(test_data_3b, "test_table_3", method = "upsert")
)
expect_equal(db$row_count("test_table_3"), 4L)
retrieved <- db$fetch("test_table_3")
expect_equal(nrow(retrieved), 4L)
expect_equal(retrieved[id_key == 2]$name, "b_updated")
expect_equal(retrieved[id_key == 3]$value, 33.3)
expect_equal(retrieved[id_key == 4]$name, "d")
expect_equal(retrieved[id_key == 1]$name, "a") # unchanged

# Test 12: Fetch with WHERE clause
result <- db$fetch("test_table_3", where = "id_key >= 3")
expect_equal(nrow(result), 2L)
expect_true(all(result$id_key >= 3))

# Test 13: Fetch with LIMIT
result <- db$fetch("test_table_3", limit = 2)
expect_equal(nrow(result), 2L)

# Test 14: Fetch with WHERE and LIMIT
result <- db$fetch("test_table_3", where = "value > 20", limit = 1)
expect_equal(nrow(result), 1L)
expect_true(result$value > 20)

# Test 15: delete_from with WHERE clause
deleted_count <- db$delete_from("test_table_3", where = "id_key = 1")
expect_equal(deleted_count, 1L)
expect_equal(db$row_count("test_table_3"), 3L)
retrieved <- db$fetch("test_table_3")
expect_false(1L %in% retrieved$id_key)

# Test 16: delete_from with complex WHERE
db$commit(
  method = "overwrite",
  test_data_1,
  "test_table_4"
)
deleted_count <- db$delete_from("test_table_4", where = "id < 3")
expect_equal(deleted_count, 2L)
retrieved <- db$fetch("test_table_4")
expect_true(all(retrieved$id >= 3))

# Test 17: delete_from with NULL (delete all)
count_before <- db$row_count("test_table_4")
deleted_count <- db$delete_from("test_table_4", where = NULL)
expect_equal(deleted_count, count_before)
expect_equal(db$row_count("test_table_4"), 0L)

# Test 18: delete_from on non-existent table
deleted_count <- db$delete_from("nonexistent", where = "id = 1")
expect_equal(deleted_count, 0L)

# Test 19: delete_from with WHERE that matches nothing
db$commit(
  method = "overwrite",
  test_data_1,
  "test_table_5"
)
initial_count <- db$row_count("test_table_5")
deleted_count <- db$delete_from("test_table_5", where = "id = 999")
expect_equal(deleted_count, 0L)
expect_equal(db$row_count("test_table_5"), initial_count)

# Test 31: Extension loading
# Gate: skip during R CMD check; run with build_install_test()
if (at_home()) {
  test_dir_ext <- tempfile("parquet_db_ext_")
  db_ext <- parquet_db$new(
    path = test_dir_ext,
    extensions = "spatial"
  )
  # Verify spatial extension is loaded by using a spatial function
  expect_match(
    db_ext$get_query("SELECT ST_AsText(ST_Point(0, 0)) as geom")[[1]],
    "POINT (0 0)",
    fixed = TRUE
  )
}

# Test 32: Persistence across connections
db$commit(
  method = "overwrite",
  test_data_1,
  "persist_test"
)
rm(db)
gc()

# Reconnect to same path
db <- parquet_db$new(path = test_dir)
expect_true("persist_test" %in% db$list_tables())
retrieved <- db$fetch("persist_test")
expect_equal(retrieved, test_data_1)

# Test 34: commit with no key_cols defaults to append
test_no_keys <- data.table::data.table(
  name = c("x", "y"),
  value = c(1, 2)
)
db$commit(
  method = "overwrite",
  test_no_keys,
  "no_keys_test"
)
expect_equal(db$row_count("no_keys_test"), 2L)

db$commit(
  test_no_keys,
  "no_keys_test",
  method = "upsert"
)
expect_equal(db$row_count("no_keys_test"), 4L) # Should append

# Test 35: Print method
expect_stdout(
  print(db),
  "Public Methods:|Active Bindings:|Format|Compression"
)

# Test 36: Metadata preservation on overwrite
test_data_with_attrs <- data.table::data.table(
  id = 1:3,
  name = c("a", "b", "c"),
  value = c(1.1, 2.2, 3.3)
)
data.table::setkeyv(test_data_with_attrs, "id") # don't want this to come back
data.table::setattr(test_data_with_attrs, "custom_attr", "test_value")
data.table::setattr(test_data_with_attrs, "numeric_attr", 42)
data.table::setattr(test_data_with_attrs, "logical_attr", TRUE)

db$commit(
  method = "overwrite",
  test_data_with_attrs,
  "test_metadata"
)

retrieved <- db$fetch("test_metadata")
expect_false(data.table::haskey(retrieved))
expect_equal(attr(retrieved, "custom_attr"), "test_value")
expect_equal(attr(retrieved, "numeric_attr"), 42)
expect_equal(attr(retrieved, "logical_attr"), TRUE)

# Test 38: Metadata override warning on append
# TODO should this become a warning, just in the other direction that nothing is done?
test_data_diff_attrs <- data.table::data.table(
  id = 6L,
  name = "f",
  value = 6.6
)
data.table::setattr(test_data_diff_attrs, "custom_attr", "different_value")
data.table::setattr(test_data_diff_attrs, "new_attr", "new")

expect_silent(
  db$commit(
    test_data_diff_attrs,
    "test_metadata",
    method = "append"
  )
)

retrieved <- db$fetch("test_metadata")
# Original attributes preserved, new ones ignored
expect_equal(attr(retrieved, "custom_attr"), "test_value") # original preserved
expect_equal(attr(retrieved, "new_attr"), "new")

# Test 39: Metadata preservation on upsert
test_data_upsert_base <- data.table::data.table(
  id = 1:3,
  name = c("whatevs", "i don't need", "your pity help"),
  value = c(10, 20, 30)
)
data.table::setattr(test_data_upsert_base, "version", "1.0")
data.table::setattr(test_data_upsert_base, "key_cols", "id")

db$commit(
  method = "upsert",
  test_data_upsert_base,
  "test_metadata"
)

retrieved <- db$fetch("test_metadata")
expect_equal(nrow(retrieved), 4L)
expect_equal(attr(retrieved, "version"), 1.0)

# Test 40: Metadata preservation on delete_from
db$delete_from("test_metadata", where = "id <= 2")
retrieved <- db$fetch("test_metadata")
expect_equal(nrow(retrieved), 2L)
expect_equal(attr(retrieved, "version"), 1.0)

# Test 41: Partitioned write (overwrite)
test_part_1 <- data.table::data.table(
  id = 1:6,
  group = rep(c("A", "B"), each = 3),
  value = rnorm(6)
)
# Ensure columns are ordered for comparison later
data.table::setcolorder(test_part_1, c("id", "value", "group"))
data.table::setattr(test_part_1, "partition_cols", "group")

expect_silent(
  db$commit(
    method = "overwrite",
    test_part_1,
    "test_partitioned"
  )
)

expect_true(dir.exists(file.path(test_dir, "test_partitioned.parquet")))
# Check partition directories exist (standard hive partitioning)
expect_true(
  length(list.files(file.path(test_dir, "test_partitioned.parquet"), pattern = "group=")) == 2
)

retrieved <- db$fetch("test_partitioned")
data.table::setorder(retrieved, id)
data.table::setcolorder(retrieved, c("id", "value", "group"))

expect_equal(retrieved, test_part_1)

# Test 42: Partitioned append
test_part_2 <- data.table::data.table(
  id = 7:8,
  group = c("A", "C"),
  value = rnorm(2)
)
data.table::setattr(test_part_2, "partition_cols", "group")
expect_silent(
  db$commit(
    test_part_2,
    "test_partitioned",
    method = "append"
  )
)

# New partition created
expect_length(
  list.files(file.path(test_dir, "test_partitioned.parquet"), pattern = "group="),
  3
)
expect_equal(db$row_count("test_partitioned"), 8L)

retrieved <- db$fetch("test_partitioned")
expect_equal(nrow(retrieved), 8L)
expect_true("C" %in% retrieved$group)

# Test 43: Partitioned upsert
test_part_3 <- data.table::data.table(
  id = c(1L, 9L), # 1 exists in A (update), 9 is new in B (insert)
  group = c("A", "B"),
  value = c(99.9, 88.8)
)
data.table::setattr(test_part_3, "partition_cols", "group")
data.table::setattr(test_part_3, "key_cols", "id")

expect_equal(db$row_count("test_partitioned"), 8L)
# 1 updated + 1 inserted + 6 unchanged but touched by partition overwrite = 8 total
expect_equal(db$commit(test_part_3, "test_partitioned", method = "upsert"), 8L)
expect_equal(db$row_count("test_partitioned"), 9L)
# 2 updated + 0 inserted + 6 unchanged but touched by partition overwrite = 8 total
expect_equal(db$commit(test_part_3, "test_partitioned", method = "upsert"), 8L)
expect_equal(db$row_count("test_partitioned"), 9L)
# repeat to ensure idempotency
expect_equal(db$commit(test_part_3, "test_partitioned", method = "upsert"), 8L)
expect_equal(db$row_count("test_partitioned"), 9L)

retrieved <- db$fetch("test_partitioned")
data.table::setorder(retrieved, id)

expect_equal(retrieved[id == 1]$value, 99.9)
expect_equal(retrieved[id == 9]$value, 88.8)

expect_length(
  list.files(file.path(test_dir, "test_partitioned.parquet"), recursive = TRUE),
  3L
)

# Test 44: Metadata with partitioning
test_part_meta <- data.table::data.table(
  id = 1:2,
  g = c("x", "y"),
  val = 1:2
)
data.table::setattr(test_part_meta, "my_meta", "exists")
data.table::setattr(test_part_meta, "partition_cols", "g")

db$commit(
  method = "overwrite",
  test_part_meta,
  "test_part_meta"
)

retrieved <- db$fetch("test_part_meta")
expect_equal(attr(retrieved, "my_meta"), "exists")
expect_equal(attr(retrieved, "partition_cols"), "g")

# Test 45: Alternate key columns in upsert
test_alt_key <- data.table::data.table(
  id = 1:3,
  name = c("a", "b", "c"),
  value = c(10, 20, 30)
)
data.table::setattr(test_alt_key, "key_cols", "id")
data.table::setattr(test_alt_key, "alternate_key_cols", "name")
expect_silent(
  db$commit(
    method = "overwrite",
    test_alt_key,
    "test_alt_key"
  )
)
expect_error(
  db$commit(
    data.table::data.table(
      id = 1,
      name = c("b", "b", "c"),
      value = NA_real_
    ),
    "test_alt_key",
    method = "upsert"
  ),
  "Constraint Error: Duplicate key"
)
expect_equal(
  db$commit(
    test_data_1[3:5],
    "test_alt_key",
    method = "upsert"
  ),
  5L # 1 updated + 2 inserted + 2 unchanged but touched and written = 5 total
)
expect_equal(
  db$fetch("test_alt_key")[order(id), value],
  c(10, 20, 30.3, 40.4, 50.5)
)
