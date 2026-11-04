library(tinytest)

# ---- as_ducklake_db_t: minimal usage (defaults only) ----
x_min <- as_ducklake_db_t(data.frame(id = 1:3, val = letters[1:3]))
expect_true(data.table::is.data.table(x_min))
expect_inherits(x_min, "ducklake_db_t")
expect_identical(attr(x_min, "key_cols"), NULL)
expect_identical(attr(x_min, "alternate_key_cols"), NULL)
expect_identical(attr(x_min, "map_cols"), NULL)
expect_identical(attr(x_min, "partition_cols"), NULL)

# validator error (false input) for minimal usage: mixed classes in list column
expect_error(
  as_ducklake_db_t(data.table::data.table(id = 1:2, bad_list = list(1L, "x"))),
  "must have the same class"
)

# ---- as_ducklake_db_t: class_name only ----
x_class <- as_ducklake_db_t(data.frame(id = 1:2), class_name = "demo_t")
expect_inherits(x_class, c("demo_t", "ducklake_db_t"))

# validator error (false input) for class_name-only object: non-atomic attribute
x_class_bad <- as_ducklake_db_t(data.frame(id = 1:2), class_name = "demo_t")
data.table::setattr(x_class_bad, "bad_attr", list(1))
expect_error(validate(x_class_bad), "all attributes need to be atomic")

# ---- as_ducklake_db_t: all optional attrs set ----
x_all <- as_ducklake_db_t(
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
  as_ducklake_db_t(
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

# ---- serialize_metadata / deserialize_metadata ----
expect_identical(
  evoland.plus:::serialize_metadata(list(existing = "keep", custom_attr = c("a", "b"))),
  'existing: "keep"\ncustom_attr: "a", "b"'
)
expect_identical(evoland.plus:::serialize_metadata(list()), "")

# values are type-converted, so numbers do not come back as strings
expect_identical(
  evoland.plus:::deserialize_metadata('existing: "keep"\nnum: "42"\nflag: "TRUE"'),
  list(existing = "keep", num = 42L, flag = TRUE)
)
expect_identical(evoland.plus:::deserialize_metadata(NA_character_), list())
expect_identical(evoland.plus:::deserialize_metadata(""), list())

# a metadata list survives the round-trip unchanged
roundtrip <- list(epsg = 2056L, resolution = 100.5, key_cols = c("id_run", "id_coord"))
expect_identical(
  evoland.plus:::deserialize_metadata(evoland.plus:::serialize_metadata(roundtrip)),
  roundtrip
)

# ---- convert_list_cols ----
x_conv <- data.table::data.table(map_col = list(list(a = 1, b = "x"), list(a = 2, b = "y")))
expect_identical(
  evoland.plus:::convert_list_cols(
    x_conv,
    cols = "map_col",
    fn = evoland.plus:::list_to_kv_df
  )[[1]][[1]],
  data.frame(key = c("a", "b"), value = c("1", "x"))
)

# ---- list_to_kv_df / kv_df_to_list ----
expect_identical(
  namedlist <- list(a = 1L, b = TRUE, c = "x"),
  evoland.plus:::kv_df_to_list(evoland.plus:::list_to_kv_df(namedlist))
)

# ---- create_table_binding: lexical body substitution ----
expect_match(
  evoland.plus:::create_table_binding("coords_t", mode = "append") |>
    body() |>
    deparse() |>
    paste(collapse = "\n"),
  'tbl <- "coords_t"(.|\n)*?md <- "append"',
  perl = TRUE
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
  evoland.plus:::create_method_binding(pure_fun, with_private = TRUE)
}
expect_equal(
  wrapper(a = 2, b = 3),
  list(sum = 5, self_ok = TRUE, private_ok = TRUE, super_ok = FALSE)
)

# create_method_binding() forwards only the arguments a caller supplies, so a default in the
# R6 signature never reaches the implementation: the defaults must be the same in both (#101)

# the arguments a caller passes: the binding supplies self, private and super
caller_formals <- function(fun) {
  args <- as.list(formals(fun))
  args <- args[setdiff(names(args), c("self", "private", "super"))]
  # a method without arguments: an empty list either way, without a names attribute
  if (length(args) == 0L) list() else args
}
first_match <- function(text, pattern) {
  regmatches(text, regexpr(pattern, text, perl = TRUE))
}
bound_methods <- c(
  evoland.plus::evoland_db$public_methods,
  evoland.plus::ducklake_db$public_methods
)
for (method in names(bound_methods)) {
  implementation <-
    bound_methods[[method]] |>
    body() |>
    deparse() |>
    paste(collapse = " ") |>
    first_match("(?<=create_method_binding\\()[A-Za-z_.]+")
  # methods that don't delegate, such as initialize()
  if (length(implementation) == 0L) {
    next
  }
  expect_identical(
    caller_formals(bound_methods[[method]]),
    implementation |>
      get(envir = asNamespace("evoland.plus")) |>
      caller_formals(),
    info = paste0("defaults of db$", method, "()")
  )
}
