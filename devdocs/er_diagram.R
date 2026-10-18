# Regenerates the ER diagram in devdocs/design/database.md from the table constructors.
#
#   Rscript devdocs/er_diagram.R
#
# Every exported `as_*_t()` called without arguments returns an empty table carrying its
# columns, types and key attributes; the diagram is derived from those. A column is a
# foreign key if it is named after another table's id: `id_<entity>` for the table
# `<entity>_t`, `<entity>s_t` or `<entity>_meta_t`, optionally with a role prefix or suffix
# (`parent_id_run`, `id_lulc_anterior`, `id_period_post`). CI fails if the committed
# diagram differs from the generated one, see .github/workflows/er-diagram.yaml.

if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(quiet = TRUE)
} else {
  library(evoland)
}

target_file <- file.path("devdocs", "design", "database.md")
begin_marker <- paste(
  "<!-- BEGIN GENERATED ER DIAGRAM:",
  "run `Rscript devdocs/er_diagram.R`, do not edit -->"
)
end_marker <- "<!-- END GENERATED ER DIAGRAM -->"

constructor_names <- setdiff(
  grep("^as_.*_t$", getNamespaceExports("evoland"), value = TRUE),
  "as_ducklake_db_t"
)
tables <- lapply(sort(constructor_names), \(fn) getExportedValue("evoland", fn)())
names(tables) <- sub("^as_", "", sort(constructor_names))

column_type <- function(column, column_name, map_cols) {
  if (column_name %in% map_cols) {
    return("map")
  }
  switch(
    class(column)[1],
    integer = "int",
    numeric = "float",
    character = "string",
    logical = "bool",
    Date = "date",
    factor = "factor",
    list = "list",
    class(column)[1]
  )
}

# id column -> table it identifies, from the naming convention described above
id_owner <- list()
for (table_name in names(tables)) {
  base <- sub("_t$", "", table_name)
  entities <- unique(c(base, sub("_meta$", "", base), sub("s$", "", base)))
  key_cols <- c(
    attr(tables[[table_name]], "key_cols"),
    attr(tables[[table_name]], "alternate_key_cols")
  )
  for (id_col in intersect(paste0("id_", entities), key_cols)) {
    id_owner[[id_col]] <- table_name
  }
}

referenced_table <- function(column_name, column) {
  if (!is.integer(column)) {
    return(NA_character_)
  }
  for (id_col in names(id_owner)) {
    pattern <- paste0("^([a-z]+_)?", id_col, "(_[a-z]+)?$")
    if (grepl(pattern, column_name)) {
      return(id_owner[[id_col]])
    }
  }
  NA_character_
}

entity_lines <- character()
relation_lines <- character()
for (table_name in names(tables)) {
  x <- tables[[table_name]]
  key_cols <- attr(x, "key_cols")
  alternate_key_cols <- attr(x, "alternate_key_cols")
  map_cols <- attr(x, "map_cols")

  entity_lines <- c(entity_lines, paste0("    ", table_name, " {"))
  for (column_name in names(x)) {
    parent <- referenced_table(column_name, x[[column_name]])
    is_foreign <- !is.na(parent) && !(parent == table_name && column_name %in% names(id_owner))
    markers <- c(
      if (column_name %in% key_cols) "PK",
      if (column_name %in% alternate_key_cols) "UK",
      if (is_foreign) "FK"
    )
    entity_lines <- c(
      entity_lines,
      paste(
        "       ",
        column_type(x[[column_name]], column_name, map_cols),
        column_name,
        paste(markers, collapse = ", ")
      ) |>
        trimws(which = "right")
    )
    if (is_foreign) {
      cardinality <- if (startsWith(column_name, "parent_")) "}o--o|" else "}o--||"
      relation_lines <- c(
        relation_lines,
        glue::glue('    {table_name} {cardinality} {parent} : "{column_name}"')
      )
    }
  }
  entity_lines <- c(entity_lines, "    }")
}

diagram <- c(
  begin_marker,
  "```mermaid",
  "erDiagram",
  entity_lines,
  "",
  relation_lines,
  "```",
  end_marker
)

document <- readLines(target_file)
begin <- which(document == begin_marker)
end <- which(document == end_marker)
stopifnot(
  "database.md must contain the begin and end markers exactly once, in order" =
    length(begin) == 1L && length(end) == 1L && begin < end
)
writeLines(
  c(document[seq_len(begin - 1L)], diagram, document[-seq_len(end)]),
  target_file
)
