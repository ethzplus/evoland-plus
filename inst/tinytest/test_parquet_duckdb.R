# Test generic parquet_db functionality
library(tinytest)

# Create temporary directory for testing
test_dir <- tempfile("parquet_db_test_")
on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

# Test 1: Initialization
expect_silent(
  db <- parquet_db$new(
    path = test_dir
  )
)
expect_true(inherits(db, "parquet_db"))
expect_true(dir.exists(test_dir))
expect_true(!is.null(db$connection))
expect_true(inherits(db$connection, "duckdb_connection"))

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
expect_silent(
  db$commit(test_data_3, "test_table_3", key_cols = "id_key", method = "upsert")
)
expect_true("test_table_3" %in% db$list_tables())
expect_equal(db$row_count("test_table_3"), 3L)

# Test 11: commit w/ upsert updates existing rows and inserts new ones
test_data_3b <- data.table::data.table(
  id_key = c(2L, 3L, 4L),
  name = c("b_updated", "c_updated", "d"),
  value = c(22.2, 33.3, 44.4)
)
expect_silent(
  db$commit(test_data_3b, "test_table_3", key_cols = "id_key", method = "upsert")
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

# Test 20: Auto-increment on overwrite (new table)
test_autoinc_1 <- data.table::data.table(
  name = c("item_a", "item_b", "item_c"),
  value = c(10, 20, 30)
)
db$commit(
  method = "overwrite",
  test_autoinc_1,
  "test_autoinc_1",
  autoincrement_cols = "id"
)
result <- db$fetch("test_autoinc_1")
expect_equal(result$id, 1:3)
expect_equal(result$name, c("item_a", "item_b", "item_c"))

# Test 21: Auto-increment on append
test_autoinc_2 <- data.table::data.table(
  name = c("item_d", "item_e"),
  value = c(40, 50)
)
db$commit(
  test_autoinc_2,
  "test_autoinc_1",
  autoincrement_cols = "id",
  method = "append"
)
result <- db$fetch("test_autoinc_1")
expect_equal(nrow(result), 5L)
expect_equal(result$id, 1:5)
expect_equal(result$name[4:5], c("item_d", "item_e"))

# Test 22: Auto-increment on upsert with new rows
test_autoinc_3 <- data.table::data.table(
  name = c("item_f", "item_g"),
  value = c(60, 70)
)
db$commit(
  test_autoinc_3,
  "test_autoinc_1",
  autoincrement_cols = "id",
  method = "upsert"
)
result <- db$fetch("test_autoinc_1")
expect_equal(nrow(result), 7L)
expect_equal(result$id, 1:7)

# Test 23: Auto-increment warning when overriding existing IDs
test_autoinc_with_ids <- data.table::data.table(
  id = c(NA, 100L, NA),
  name = c("new_a", "existing", "new_b"),
  value = c(1, 2, 3)
)
expect_warning(
  db$commit(
    method = "overwrite",
    test_autoinc_with_ids,
    "test_autoinc_2",
    autoincrement_cols = "id"
  ),
  "Overriding existing IDs"
)
result <- db$fetch("test_autoinc_2")
expect_equal(result$id, 1:3)

# Test 24: Multiple auto-increment columns
test_multi_autoinc <- data.table::data.table(
  name = c("item1", "item2"),
  value = c(10, 20)
)
db$commit(
  method = "overwrite",
  test_multi_autoinc,
  "test_multi_autoinc",
  autoincrement_cols = c("id_a", "id_b")
)
result <- db$fetch("test_multi_autoinc")
expect_equal(result$id_a, 1:2)
expect_equal(result$id_b, 1:2)

# Test 25: Auto-increment continues from max
test_continue_a <- data.table::data.table(
  id_seq = c(5L, 10L, 15L),
  value = c(100, 200, 300)
)
db$commit(
  method = "overwrite",
  test_continue_a,
  "test_continue"
)

test_continue_b <- data.table::data.table(
  value = c(400, 500)
)
db$commit(
  test_continue_b,
  "test_continue",
  autoincrement_cols = "id_seq",
  method = "append"
)
result <- db$fetch("test_continue")
expect_equal(nrow(result), 5L)
expect_equal(result$id_seq[4:5], c(16L, 17L))
expect_equal(result$value[4:5], c(400, 500))

# Test 26: attach_table and detach_table
db$commit(
  method = "overwrite",
  test_data_1,
  "test_attach"
)
expect_silent(db$attach_table("test_attach"))
# Verify table is attached by querying it directly
result <- db$get_query("SELECT COUNT(*) as n FROM test_attach")
expect_equal(result$n, 5L)
expect_silent(db$detach_table("test_attach"))
# After detach, table should not be accessible
expect_error(
  db$get_query("SELECT COUNT(*) as n FROM test_attach"),
  "test_attach"
)

# Test 27: attach_table with column selection
db$attach_table("test_attach", columns = c("id", "name"))
result <- db$get_query("SELECT * FROM test_attach")
expect_equal(ncol(result), 2L)
expect_true(all(c("id", "name") %in% names(result)))
expect_false("value" %in% names(result))
db$detach_table("test_attach")

# Test 28: attach_table with WHERE clause
db$attach_table("test_attach", where = "id > 3")
result <- db$get_query("SELECT * FROM test_attach")
expect_equal(nrow(result), 2L)
expect_true(all(result$id > 3))
db$detach_table("test_attach")

# Test 29: execute() method
db$attach_table("test_attach")
rows_affected <- db$execute("DELETE FROM test_attach WHERE id = 1")
expect_true(rows_affected >= 0) # DuckDB returns number of affected rows
db$detach_table("test_attach")

# Test 30: get_query() method
db$attach_table("test_attach")
result <- db$get_query("SELECT MAX(id) as max_id FROM test_attach")
expect_true(inherits(result, "data.table"))
expect_true("max_id" %in% names(result))
db$detach_table("test_attach")

# Test 31: Extension loading
test_dir_ext <- tempfile("parquet_db_ext_")
on.exit(unlink(test_dir_ext, recursive = TRUE), add = TRUE)

db_ext <- parquet_db$new(
  path = test_dir_ext,
  extensions = "spatial"
)
# Verify spatial extension is loaded by using a spatial function
expect_silent(
  db_ext$get_query("SELECT ST_Point(0, 0) as geom")
)

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
  key_cols = character(0),
  method = "upsert"
)
expect_equal(db$row_count("no_keys_test"), 4L) # Should append

# Test 35: Print method
expect_stdout(
  print(db),
  "Public methods:|Active bindings:|Format|Compression"
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
test_data_diff_attrs <- data.table::data.table(
  id = 6L,
  name = "f",
  value = 6.6
)
data.table::setattr(test_data_diff_attrs, "custom_attr", "different_value")
data.table::setattr(test_data_diff_attrs, "new_attr", "new")

expect_warning(
  db$commit(
    test_data_diff_attrs,
    "test_metadata",
    method = "append"
  ),
  "Overriding existing metadata"
)

retrieved <- db$fetch("test_metadata")
expect_equal(attr(retrieved, "custom_attr"), "different_value")
expect_equal(attr(retrieved, "new_attr"), "new")

# Test 39: Metadata preservation on upsert
test_data_upsert_base <- data.table::data.table(
  id = 1:3,
  name = c("whatevs", "i don't need", "your pity help"),
  value = c(10, 20, 30)
)
data.table::setattr(test_data_upsert_base, "version", "1.0")

db$commit(
  method = "upsert",
  test_data_upsert_base,
  "test_metadata",
  key_cols = "id"
)

retrieved <- db$fetch("test_metadata")
expect_equal(nrow(retrieved), 4L)
expect_equal(attr(retrieved, "version"), 1.0)

# Test 40: Metadata preservation on delete_from
db$delete_from("test_metadata", where = "id <= 2")
retrieved <- db$fetch("test_metadata")
expect_equal(nrow(retrieved), 2L)
expect_equal(attr(retrieved, "version"), 1.0)
