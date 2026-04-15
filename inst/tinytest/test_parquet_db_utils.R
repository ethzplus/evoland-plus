library(tinytest)

# ---- as_parquet_db_t: minimal usage (defaults only) ----
x_min <- as_parquet_db_t(data.frame(id = 1:3, val = letters[1:3]))
expect_true(data.table::is.data.table(x_min))
expect_inherits(x_min, "parquet_db_t")
expect_identical(attr(x_min, "key_cols"), NULL)
expect_identical(attr(x_min, "alternate_key_cols"), NULL)
expect_identical(attr(x_min, "map_cols"), NULL)
expect_identical(attr(x_min, "partition_cols"), NULL)

# validator error (false input) for minimal usage: mixed classes in list column
expect_error(
  as_parquet_db_t(data.table::data.table(id = 1:2, bad_list = list(1L, "x"))),
  "must have the same class"
)

# ---- as_parquet_db_t: class_name only ----
x_class <- as_parquet_db_t(data.frame(id = 1:2), class_name = "demo_t")
expect_inherits(x_class, c("demo_t", "parquet_db_t"))

# validator error (false input) for class_name-only object: non-atomic attribute
x_class_bad <- as_parquet_db_t(data.frame(id = 1:2), class_name = "demo_t")
data.table::setattr(x_class_bad, "bad_attr", list(1))
expect_error(validate(x_class_bad), "all attributes need to be atomic")

# ---- as_parquet_db_t: all optional attrs set ----
x_all <- as_parquet_db_t(
  data.table::data.table(
    id = c("a", "b"),
    alt_id = c(10L, 20L),
    map_col = list(
      list(k = 1L),
      list(k = 2L)
    ),
    part = c("p1", "p2")
  ),
  class_name = "full_t",
  key_cols = "id",
  alternate_key_cols = "alt_id",
  map_cols = "map_col",
  partition_cols = "part"
)
expect_identical(class(x_all)[1], "full_t")
expect_identical(attr(x_all, "key_cols"), "id")
expect_identical(attr(x_all, "alternate_key_cols"), "alt_id")
expect_identical(attr(x_all, "map_cols"), "map_col")
expect_identical(attr(x_all, "partition_cols"), "part")
expect_identical(data.table::key(x_all), "id")

# validator error (false input) for all-attrs case: invalid map_cols payload
expect_error(
  as_parquet_db_t(
    data.table::data.table(
      id = c("a", "b"),
      alt_id = c(1L, 2L),
      map_col = list(
        list(k = 1L),
        list(2:4)
      ),
      part = c("p1", "p2")
    ),
    class_name = "full_t",
    key_cols = "id",
    alternate_key_cols = "alt_id",
    map_cols = "map_col",
    partition_cols = "part"
  ),
  "Column 'map_col' specified as map_cols must be a list of named lists with atomic values"
)

# ---- resolve_cols / resolve_partition_clause ----
x_resolve <- as_parquet_db_t(
  data.table::data.table(id = 1L, part = "p"),
  key_cols = "id",
  partition_cols = "part"
)
expect_identical(
  evoland:::resolve_cols(x_resolve, metadata = list(), attr = "key_cols"),
  "id"
)
expect_warning(
  id_alt <- evoland:::resolve_cols(
    x_resolve,
    metadata = list(key_cols = "id_alt"),
    attr = "key_cols"
  ),
  "key_cols on disk (id_alt) takes precedence over attributes (id)",
  fixed = TRUE
)
expect_identical(id_alt, "id_alt")

expect_match(
  evoland:::resolve_partition_clause(x_resolve),
  ', partition_by ( "part" )',
  fixed = TRUE
)

# ---- resolve_metadata_clause ----
x_meta <- as_parquet_db_t(data.table::data.table(id = 1L))
data.table::setattr(x_meta, "custom_attr", c("a", "b"))
expect_match(
  evoland:::resolve_metadata_clause(x_meta, metadata = list(existing = "keep")),
  r"[existing: '"keep"',
  custom_attr: '"a", "b"',
  parquet_db_t_class: '"parquet_db_t"']",
  fixed = TRUE
)

# ---- convert_list_cols ----
x_conv <- data.table::data.table(map_col = list(list(a = 1, b = "x"), list(a = 2, b = "y")))
expect_identical(
  evoland:::convert_list_cols(x_conv, cols = "map_col", fn = evoland:::list_to_kv_df)[[1]][[1]],
  data.frame(key = c("a", "b"), value = c("1", "x"))
)

# ---- list_to_kv_df / kv_df_to_list ----
expect_identical(
  namedlist <- list(a = 1L, b = TRUE, c = "x"),
  evoland:::kv_df_to_list(evoland:::list_to_kv_df(namedlist))
)

# ---- create_table_binding: lexical body substitution ----
expect_match(
  evoland:::create_table_binding("coords_t", mode = "append") |>
    body() |>
    deparse() |>
    paste(collapse = "\n"),
  'tbl <- "coords_t"\n    md <- "append"',
  fixed = TRUE
)

# ---- create_method_binding ----
pure_fun <- function(a, b = 1, self, private = NULL, super = NULL) {
  list(
    sum = a + b,
    self_ok = !is.null(self),
    private_ok = !is.null(private),
    super_ok = !is.null(super)
  )
}
wrapper <- function(...) {
  self <- list(tag = "self")
  private <- list(tag = "private")
  super <- list(tag = "super")
  evoland:::create_method_binding(pure_fun, with_private = TRUE)
}
expect_equal(
  wrapper(a = 2, b = 3),
  list(sum = 5, self_ok = TRUE, private_ok = TRUE, super_ok = FALSE)
)

# ---- cols_to_select_expr ----
expect_identical(evoland:::cols_to_select_expr(c("a", "b")), '"a", "b"')
expect_identical(
  evoland:::cols_to_select_expr(c("a", "b"), "tbl"),
  '"tbl"."a", "tbl"."b"'
)
