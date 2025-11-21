#' R6 Class for Folder-Based Data Storage Interface
#'
#' @description
#' An R6 class that provides an interface to a folder-based data storage system
#' for the evoland package. Each table is stored as a parquet (or CSV) file.
#' This class uses DuckDB for in-memory SQL operations while persisting data
#' to disk in parquet format for better compression.
#'
#' @import R6 duckdb
#' @export

evoland_db <- R6::R6Class(
  classname = "evoland_db",

  ## Public Methods ----
  public = list(
    #' @field connection DBI connection object to an in-memory DuckDB database
    connection = NULL,

    #' @field path Character string path to the data folder
    path = NULL,

    #' @field default_format Default file format for new tables
    default_format = NULL,

    #' @field writeopts Default write options for DuckDB, see
    writeopts = NULL,

    #' @description
    #' Initialize a new evoland_db object
    #' @param path Character string. Path to the data folder.
    #' @param default_format Character. Default file format ("parquet" or "csv").
    #' Default is "parquet".
    #' @param ... passed on to `set_report`
    #'
    #' @return A new `evoland_db` object
    initialize = function(
      path,
      default_format = c("parquet", "csv"),
      ...
    ) {
      # Create folder if it doesn't exist
      self$path <- ensure_dir(path)

      # set format / writeopts
      self$default_format <- match.arg(default_format)
      self$writeopts <- switch(
        self$default_format,
        parquet = "FORMAT parquet, COMPRESSION zstd",
        csv = "FORMAT csv",
        stop(glue::glue("Unsupported format: {format}"))
      )

      # Create in-memory connection for SQL operations
      self$connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
      self$execute("INSTALL spatial; LOAD spatial;")

      self$set_report(...)

      invisible(self)
    },

    ### DB methods ----
    #' @description
    #' Commit data in overwrite mode
    #' @param x Data frame to commit
    #' @param table_name Character string table name
    #' @param autoincrement_cols Character vector of column names to auto-increment
    commit_overwrite = function(x, table_name, autoincrement_cols = character(0)) {
      file_path <- file.path(self$path, paste0(table_name, ".", self$default_format))

      duckdb::duckdb_register(self$connection, "new_data_v", x)
      on.exit(duckdb::duckdb_unregister(self$connection, "new_data_v"))

      # if there are any of these, first get existing max values
      if (length(intersect(autoincrement_cols, names(x))) > 0) {
        warning(glue::glue(
          "Overriding existing IDs ({toString(autoincrement_cols)}) with row numbers;\n",
          "Assign these IDs manually and do not pass any autoincrement_cols to avoid this warning"
        ))
      }

      select_expr <- compose_select_expr(
        all_cols = names(x),
        row_num_cols = autoincrement_cols
      )

      self$execute(glue::glue(
        r"{
        copy (
          select {select_expr}
          from new_data_v
        ) to '{file_path}' ({self$writeopts})
        }"
      ))
    },

    #' @description
    #' Commit data in append mode
    #' @param x Data frame to commit
    #' @param table_name Character string table name
    #' @param autoincrement_cols Character vector of column names to auto-increment
    commit_append = function(x, table_name, autoincrement_cols = character(0)) {
      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        self$commit_overwrite(x, table_name, autoincrement_cols)
      }

      self$attach_table(table_name)
      on.exit(self$detach_table(table_name))
      private$set_autoincrement_vars(table_name, autoincrement_cols)

      duckdb::duckdb_register(conn = self$connection, "new_data_v", df = x)
      on.exit(duckdb::duckdb_unregister(self$connection, "new_data_v"), add = TRUE)

      select_new <- compose_select_expr(names(x), from_max_cols = autoincrement_cols)

      # concatenation using union all; the "by name" option inserts NULLs if a column is
      # missing in one of the queries. also makes it robust against differing col orders.
      self$execute(glue::glue(
        r"{
          copy (
            select
              *
            from
              {table_name}
            union all by name
            select
              {select_new}
            from
              new_data_v
          )
          to '{file_info$path}' ({self$writeopts})
          }"
      ))
    },

    #' @description
    #' Commit data in upsert mode
    #' @param x Data frame to commit
    #' @param table_name Character string table name
    #' @param key_cols Identify unique columns - heuristic: if prefixed with
    #' id_, the set of all columns designates a uniqueness condition
    #' @param autoincrement_cols Character vector of column names to auto-increment
    commit_upsert = function(
      x,
      table_name,
      key_cols = grep("^id_", names(x), value = TRUE),
      autoincrement_cols = character(0)
    ) {
      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        return(self$commit_overwrite(x, table_name, autoincrement_cols))
      } else if (length(key_cols) == 0) {
        return(self$commit_append(x, table_name, autoincrement_cols))
      }

      self$attach_table(table_name)
      on.exit(self$detach_table(table_name))

      private$set_autoincrement_vars(table_name, autoincrement_cols)

      duckdb::duckdb_register(conn = self$connection, "new_data_v", df = x)
      on.exit(duckdb::duckdb_unregister(self$connection, "new_data_v"), add = TRUE)

      # Update existing data without touching key_cols or autoincrement_cols
      ordinary_cols <- setdiff(names(x), union(key_cols, autoincrement_cols))
      update_select_expr <- glue::glue_collapse(
        glue::glue("{ordinary_cols} = new_data_v.{ordinary_cols}"),
        sep = ",\n "
      )
      update_join_condition <- glue::glue_collapse(
        glue::glue("{table_name}.{key_cols} = new_data_v.{key_cols}"),
        sep = "\nand "
      )

      self$execute(glue::glue(
        r"{
        update {table_name} set
          {update_select_expr}
        from new_data_v
        where 
          {update_join_condition};
        }"
      ))

      # Insert new data with incrementing autoincrement_cols
      insert_select_expr <- glue::glue_collapse(
        c(
          glue::glue(
            "row_number() over () + getvariable('max_{autoincrement_cols}') as {autoincrement_cols}"
          ),
          glue::glue("new_data_v.{setdiff(names(x), autoincrement_cols)}")
        ),
        sep = ",\n "
      )
      null_condition <- glue::glue_collapse(
        glue::glue("{table_name}.{key_cols} is null"),
        sep = "\nand "
      )

      self$execute(glue::glue(
        r"{
        insert into {table_name}
        select
          {insert_select_expr}
        from 
          new_data_v
        left join 
          {table_name}
        on 
          {update_join_condition}
        where
          {null_condition}
        ;
        }"
      ))

      self$execute(glue::glue("copy {table_name} to '{file_info$path}' ({self$writeopts})"))
    },

    #' @description
    #' Fetch data from storage
    #' @param table_name Character string. Name of the table to query.
    #' @param where Character string. Optional WHERE clause for the SQL query.
    #' @param limit integerish, limit the amount of rows to return
    #'
    #' @return A data.table
    fetch = function(table_name, where = NULL, limit = NULL) {
      # Check if this is a view that needs special handling
      view_result <- switch(
        table_name,
        lulc_meta_long_v = private$fetch_lulc_meta_long_v(where, limit),
        pred_sources_v = private$fetch_pred_sources_v(where, limit),
        NULL
      )

      if (!is.null(view_result)) {
        return(view_result)
      }
      # Get file path and check if it exists
      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        # Return empty data.table with proper structure
        return(private$get_empty_table(table_name))
      }

      # Build SQL query
      sql <- glue::glue("SELECT * FROM read_{file_info$format}('{file_info$path}')")

      if (!is.null(where)) {
        sql <- glue::glue("{sql} WHERE {where}")
      }
      if (!is.null(limit)) {
        sql <- glue::glue("{sql} LIMIT {limit}")
      }

      DBI::dbGetQuery(self$connection, sql) |>
        data.table::as.data.table()
    },

    #' @description
    #' List all tables (files) in storage
    #' @return Character vector of table names
    list_tables = function() {
      list.files(self$path, pattern = "\\.(parquet|csv)$", full.names = FALSE) |>
        tools::file_path_sans_ext() |>
        unique() |>
        sort()
    },

    #' @description
    #' Execute statement
    #' @param statement A SQL statement
    #' @return No. of rows affected by statement
    execute = function(statement) {
      DBI::dbExecute(self$connection, statement)
    },

    #' @description
    #' Get Query
    #' @param statement A SQL statement
    #' @return No. of rows affected by statement
    get_query = function(statement) {
      DBI::dbGetQuery(self$connection, statement) |>
        data.table::as.data.table()
    },

    #' @description
    #' Attach one or more tables from the database folder as temporary tables in DuckDB.
    #' This is useful for working with multiple tables in SQL queries without loading
    #' them into R memory.
    #' @param table_name Character vector. Names of table to attach.
    #' @param columns Character vector. Optional sql column selection, defaults to "*"
    attach_table = function(table_name, columns = "*") {
      file_info <- private$get_file_path(table_name)

      # Build and execute SQL
      self$execute(glue::glue(
        "CREATE TEMP TABLE {table_name} AS ",
        "SELECT {paste(columns, collapse = ', ')} ",
        "FROM read_{file_info$format}('{file_info$path}')"
      ))
    },

    #' @description
    #' Detach one or more tables from the database.
    #' @param table_name Character. Name of table to drop.
    detach_table = function(table_name) {
      self$execute(paste0("drop table ", table_name, ";"))
    },

    #' @description
    #' Get table row count
    #' @param table_name Character string. Name of the table to query.
    #' @return No. of rows
    row_count = function(table_name) {
      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        return(0L)
      }

      DBI::dbGetQuery(
        self$connection,
        glue::glue("SELECT COUNT(*) as n FROM read_{file_info$format}('{file_info$path}')")
      )[[1]]
    },

    #' @description
    #' Delete rows from a table
    #' @param table_name Character string. Name of the table to delete from.
    #' @param where Character string, defaults to NULL: delete everything in table.
    #' @return No. of rows affected
    delete_from = function(table_name, where = NULL) {
      file_info <- private$get_file_path(table_name)

      if (!file_info$exists) {
        return(0L)
      }

      count_before <- self$row_count(table_name)

      if (is.null(where)) {
        file.remove(file_info$path)
        return(count_before)
      }

      self$execute(glue::glue(
        r"{
        COPY (
          SELECT * FROM read_{file_info$format}('{file_info$path}')
          WHERE NOT ({where})
        )
        TO '{file_info$path}' ({self$writeopts})
        }"
      ))

      count_after <- self$row_count(table_name)

      return(count_before - count_after)
    },

    ### Setter methods ----
    #' @description
    #' Set reporting metadata
    #' @param ... each named argument is entered into the table with the argument name
    #' as its key
    set_report = function(...) {
      params <- list(...)
      if (self$row_count("reporting_t") == 0L) {
        params[["report_name"]] <- "evoland_scenario"
        params[["report_name_pretty"]] <- "Default Evoland Scenario"
        params[["report_include_date"]] <- "TRUE"
        params[["creator_username"]] <- Sys.getenv("USER", unset = "unknown")
      }
      params[["last_opened"]] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      params[["last_opened_username"]] <- Sys.getenv("USER", unset = "unknown")

      self$commit_upsert(
        data.table::as.data.table(list(
          key = names(params), # cannot name a column "key" in data.table()
          value = unlist(params)
        )),
        table_name = "reporting_t",
        key_cols = "key"
      )
    },

    #' @description
    #' Set coordinates for DB. Cannot overwrite existing table (would mean cascading deletion)
    #' @param type string; which type of coordinates to set, see [coords_t]
    #' @param ... named arguments are passed to the appropriate coordinate creator function
    set_coords = function(type = c("square"), ...) {
      if (self$row_count("coords_t") > 0L) {
        warning("coords_t is not empty! Refusing to overwrite; start with fresh DB")
        return(invisible(NULL))
      }
      create_fun <- switch(
        type,
        square = create_coords_t_square,
        function(x) stop("Unsupported coordinate type specified.")
      )

      self$commit_overwrite(as_coords_t(create_fun(...)), "coords_t")
    },

    #' @description
    #' Set periods for DB. See [`periods_t`]
    #' @param period_length_str ISO 8601 duration string specifying the length of each
    #' period (currently only accepting years, e.g., "P5Y" for 5 years)
    #' @param start_observed Start date of the observed data (YYYY-MM-DD)
    #' @param end_observed End date of the observed data (YYYY-MM-DD)
    #' @param end_extrapolated End date for extrapolation time range (YYYY-MM-DD)
    set_periods = function(
      period_length_str = "P10Y",
      start_observed = "1985-01-01",
      end_observed = "2020-01-01",
      end_extrapolated = "2060-01-01"
    ) {
      if (self$row_count("periods_t") > 0L) {
        warning("periods_t is not empty! Refusing to overwrite; start with fresh DB")
        return(invisible(NULL))
      }

      self$commit_append(
        do.call(create_periods_t, as.list(environment())),
        "periods_t"
      )
    },

    ### Adder methods ----
    #' @description
    #' Add a predictor to the database
    #' @param pred_spec List of predictor specification; see [create_pred_meta_t()]
    #' @param pred_data An object that can be coerced to [`pred_data_t`], but doesn't have an
    #' `id_pred`
    #' @param pred_type Passed to [as_pred_data_t()]; one of float, int, bool
    add_predictor = function(pred_spec, pred_data, pred_type) {
      stopifnot(length(pred_spec) == 1)

      self$pred_meta_t <- create_pred_meta_t(pred_spec)

      existing_meta <- self$fetch(
        "pred_meta_t",
        where = glue::glue("name = '{names(pred_spec)}'")
      )

      data.table::set(pred_data, j = "id_pred", value = existing_meta[["id_pred"]])
      data.table::setcolorder(pred_data, c("id_pred", "id_coord", "id_period", "value"))

      self$commit_upsert(
        as_pred_data_t(pred_data, pred_type),
        paste0("pred_data_t_", pred_type)
      )
    }
  ),

  ## Active Bindings ----
  active = list(
    ### Bindings for tables ----
    #' @field coords_t A `coords_t` instance; see [create_coords_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    coords_t = function(x) {
      create_active_binding(self, "coords_t", as_coords_t)(x)
    },

    #' @field periods_t A `periods_t` instance; see [create_periods_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    periods_t = function(x) {
      create_active_binding(self, "periods_t", as_periods_t)(x)
    },

    #' @field lulc_meta_t A `lulc_meta_t` instance; see [create_lulc_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    lulc_meta_t = function(x) {
      create_active_binding(self, "lulc_meta_t", as_lulc_meta_t)(x)
    },

    #' @field lulc_meta_long_v Return a `lulc_meta_long_v` instance, i.e. unrolled `lulc_meta_t`.
    lulc_meta_long_v = function() {
      private$fetch_lulc_meta_long_v() |>
        new_evoland_table("lulc_meta_long_v", keycols = NULL)
    },

    #' @field lulc_data_t A `lulc_data_t` instance; see [as_lulc_data_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    lulc_data_t = function(x) {
      create_active_binding(self, "lulc_data_t", as_lulc_data_t)(x)
    },

    #' @field pred_data_t_float A `pred_data_t_float` instance; see
    #' [create_pred_data_t()] for the type of object to assign. Assigning is an
    #' upsert operation.
    pred_data_t_float = function(x) {
      create_active_binding(
        self,
        "pred_data_t_float",
        as_pred_data_t,
        type = "float"
      )(x)
    },

    #' @field pred_data_t_int A `pred_data_t_int` instance; see
    #' [create_pred_data_t()] for the type of object to assign. Assigning is an
    #' upsert operation.
    pred_data_t_int = function(x) {
      create_active_binding(
        self,
        "pred_data_t_int",
        as_pred_data_t,
        type = "int"
      )(x)
    },

    #' @field pred_data_t_bool A `pred_data_t_bool` instance; see
    #' [create_pred_data_t()] for the type of object to assign. Assigning is an
    #' upsert operation.
    pred_data_t_bool = function(x) {
      create_active_binding(
        self,
        "pred_data_t_bool",
        as_pred_data_t,
        type = "bool"
      )(x)
    },

    ### Bindings for descriptions, views, etc. ----
    #' @field extent Return a terra SpatExtent based on coords_t
    extent = function() {
      file_info <- private$get_file_path("coords_t")

      self$get_query(glue::glue(
        r"{
        SELECT
          min(lon) as xmin,
          max(lon) as xmax,
          min(lat) as ymin,
          max(lat) as ymax
        FROM
          read_{file_info$format}('{file_info$path}')
        }"
      )) |>
        unlist() |>
        terra::ext()
    },

    #' @field coords_minimal data.table with only (id_coord, lon, lat)
    coords_minimal = function() {
      file_info <- private$get_file_path("coords_t")

      if (!file_info$exists) {
        return(data.table::data.table(
          id_coord = integer(0),
          lon = numeric(0),
          lat = numeric(0)
        ))
      }

      sql <- glue::glue(
        "SELECT id_coord, lon, lat FROM read_{file_info$format}('{file_info$path}')"
      )

      DBI::dbGetQuery(self$connection, sql) |>
        data.table::as.data.table() |>
        cast_dt_col("id_coord", as.integer)
    },

    #' @field pred_meta_t A `pred_meta_t` instance; see [create_pred_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    pred_meta_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("pred_meta_t")
        return(as_pred_meta_t(x))
      }
      stopifnot(inherits(x, "pred_meta_t"))
      self$commit_upsert(
        x,
        table_name = "pred_meta_t",
        key_cols = "name",
        autoincrement_cols = "id_pred"
      )
    },

    #' @field pred_sources_v Retrieve a table of distinct predictor urls and their
    #' md5sum
    pred_sources_v = function() {
      private$fetch_pred_sources_v() |>
        new_evoland_table("pred_sources_v", keycols = NULL)
    },

    #' @field trans_meta_t A `trans_meta_t` instance; see [create_trans_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    trans_meta_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("trans_meta_t")
        return(as_trans_meta_t(x))
      }
      stopifnot(inherits(x, "trans_meta_t"))

      # Custom upsert for trans_meta_t to maintain unique constraint
      duckdb::duckdb_register(
        conn = self$connection,
        "temporary_trans_data",
        df = x
      )

      on.exit(duckdb::duckdb_unregister(
        self$connection,
        "temporary_trans_data"
      ))

      # Load existing data if it exists
      file_info <- private$get_file_path("trans_meta_t")

      if (file_info$exists) {
        sql <- glue::glue(
          "
          CREATE TEMP TABLE existing_trans_meta_t AS
          SELECT * FROM read_{file_info$format}('{file_info$path}');

          -- Delete rows that will be replaced (based on unique constraint)
          DELETE FROM existing_trans_meta_t
          WHERE (id_lulc_anterior, id_lulc_posterior) IN (
            SELECT id_lulc_anterior, id_lulc_posterior FROM temporary_trans_data
          );

          -- Insert new/updated rows
          INSERT INTO existing_trans_meta_t
          SELECT * FROM temporary_trans_data;

          COPY existing_trans_meta_t
          TO '{file_info$path}' ({self$writeopts});

          DROP TABLE existing_trans_meta_t;
          "
        )
      } else {
        sql <- glue::glue(
          "
          COPY temporary_trans_data
          TO '{file_info$path}' ({self$writeopts})
          "
        )
      }

      DBI::dbExecute(self$connection, sql)
    },

    #' @field trans_preds_t A `trans_preds_t` instance; see [create_trans_preds_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    trans_preds_t = function(x) {
      create_active_binding(self, "trans_preds_t", as_trans_preds_t)(x)
    },

    #' @field intrv_meta_t A `intrv_meta_t` instance; see [create_intrv_meta_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    intrv_meta_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("intrv_meta_t")

        x[["params"]] <- lapply(
          x[["params"]],
          kv_df_to_list
        )

        return(as_intrv_meta_t(x))
      }
      stopifnot(inherits(x, "intrv_meta_t"))

      # Convert params list to data.frame format for DuckDB MAP conversion
      x_copy <- data.table::copy(x)
      x_copy[["params"]] <- lapply(
        x_copy[["params"]],
        list_to_kv_df
      )

      duckdb::duckdb_register(
        conn = self$connection,
        name = "tmp_table",
        df = x_copy
      )

      on.exit(duckdb::duckdb_unregister(self$connection, "tmp_table"))

      # Get file info
      file_info <- private$get_file_path("intrv_meta_t")

      # Load existing data and merge
      if (file_info$exists) {
        sql <- glue::glue(
          "
          CREATE TEMP TABLE existing_intrv_meta_t AS
          SELECT * FROM read_{file_info$format}('{file_info$path}');

          DELETE FROM existing_intrv_meta_t
          WHERE id_intrv IN (SELECT id_intrv FROM tmp_table);

          INSERT INTO existing_intrv_meta_t
          SELECT
            id_intrv, id_period_list, id_trans_list, pre_allocation,
            name, pretty_name, description, sources,
            map_from_entries(params) as params
          FROM tmp_table;

          COPY existing_intrv_meta_t
          TO '{file_info$path}' ({self$writeopts});

          DROP TABLE existing_intrv_meta_t;
          "
        )
      } else {
        sql <- glue::glue(
          "
          CREATE TEMP TABLE new_intrv_meta_t AS
          SELECT
            id_intrv, id_period_list, id_trans_list, pre_allocation,
            name, pretty_name, description, sources,
            map_from_entries(params) as params
          FROM tmp_table;

          COPY new_intrv_meta_t
          TO '{file_info$path}' ({self$writeopts});

          DROP TABLE new_intrv_meta_t;
          "
        )
      }

      DBI::dbExecute(self$connection, sql)
    },

    #' @field intrv_masks_t A `intrv_masks_t` instance; see [as_intrv_masks_t()] for the type of
    #' object to assign. Assigning is an upsert operation.
    intrv_masks_t = function(x) {
      create_active_binding(self, "intrv_masks_t", as_intrv_masks_t)(x)
    },

    #' @field trans_models_t A `trans_models_t` instance; see [create_trans_models_t()] for the type
    #' of object to assign. Assigning is an upsert operation.
    trans_models_t = function(x) {
      if (missing(x)) {
        x <- self$fetch("trans_models_t")

        x[["model_params"]] <- lapply(
          x[["model_params"]],
          kv_df_to_list
        )
        x[["goodness_of_fit"]] <- lapply(
          x[["goodness_of_fit"]],
          kv_df_to_list
        )

        return(as_trans_models_t(x))
      }
      stopifnot(inherits(x, "trans_models_t"))

      # Convert lists to data.frame format for DuckDB MAP conversion
      x_copy <- data.table::copy(x)
      x_copy[["model_params"]] <- lapply(
        x_copy[["model_params"]],
        list_to_kv_df
      )
      x_copy[["goodness_of_fit"]] <- lapply(
        x_copy[["goodness_of_fit"]],
        list_to_kv_df
      )

      duckdb::duckdb_register(
        conn = self$connection,
        name = "tmp_table",
        df = x_copy
      )

      on.exit(duckdb::duckdb_unregister(self$connection, "tmp_table"))

      file_info <- private$get_file_path("trans_models_t")

      # Load existing data and merge
      if (file_info$exists) {
        sql <- glue::glue(
          "
          CREATE TEMP TABLE existing_trans_models_t AS
          SELECT * FROM read_{file_info$format}('{file_info$path}');

          DELETE FROM existing_trans_models_t
          WHERE (id_trans, id_period) IN (
            SELECT id_trans, id_period FROM tmp_table
          );

          INSERT INTO existing_trans_models_t
          SELECT
            id_trans, id_period, model_family,
            map_from_entries(model_params) as model_params,
            map_from_entries(goodness_of_fit) as goodness_of_fit,
            model_obj_part, model_obj_full
          FROM tmp_table;

          COPY existing_trans_models_t
          TO '{file_info$path}' ({self$writeopts});

          DROP TABLE existing_trans_models_t;
          "
        )
      } else {
        sql <- glue::glue(
          "
          CREATE TEMP TABLE new_trans_models_t AS
          SELECT
            id_trans, id_period, model_family,
            map_from_entries(model_params) as model_params,
            map_from_entries(goodness_of_fit) as goodness_of_fit,
            model_obj_part, model_obj_full
          FROM tmp_table;

          COPY new_trans_models_t
          TO '{file_info$path}' ({self$writeopts});

          DROP TABLE new_trans_models_t;
          "
        )
      }

      DBI::dbExecute(self$connection, sql)
    },

    #' @field alloc_params_t A `alloc_params_t` instance; see [as_alloc_params_t()] for the type
    #' of object to assign. Assigning is an upsert operation.
    alloc_params_t = function(x) {
      if (missing(x)) {
        x <-
          self$fetch("alloc_params_t") |>
          data.table::as.data.table()

        x[["alloc_params"]] <- lapply(
          x[["alloc_params"]],
          kv_df_to_list
        )
        x[["goodness_of_fit"]] <- lapply(
          x[["goodness_of_fit"]],
          kv_df_to_list
        )

        return(as_alloc_params_t(x))
      }
      stopifnot(inherits(x, "alloc_params_t"))

      # Convert lists to data.frame format for DuckDB MAP conversion
      x_copy <- data.table::copy(x)
      x_copy[["alloc_params"]] <- lapply(
        x_copy[["alloc_params"]],
        list_to_kv_df
      )
      x_copy[["goodness_of_fit"]] <- lapply(
        x_copy[["goodness_of_fit"]],
        list_to_kv_df
      )

      duckdb::duckdb_register(
        conn = self$connection,
        name = "tmp_table",
        df = x_copy
      )

      on.exit(duckdb::duckdb_unregister(self$connection, "tmp_table"))

      file_info <- private$get_file_path("alloc_params_t")

      # Load existing data and merge
      if (file_info$exists) {
        sql <- glue::glue(
          "
          CREATE TEMP TABLE existing_alloc_params_t AS
          SELECT * FROM read_{file_info$format}('{file_info$path}');

          DELETE FROM existing_alloc_params_t
          WHERE (id_trans, id_period) IN (
            SELECT id_trans, id_period FROM tmp_table
          );

          INSERT INTO existing_alloc_params_t
          SELECT
            id_trans, id_period,
            map_from_entries(alloc_params) as alloc_params,
            map_from_entries(goodness_of_fit) as goodness_of_fit
          FROM tmp_table;

          COPY existing_alloc_params_t
          TO '{file_info$path}' ({self$writeopts});

          DROP TABLE existing_alloc_params_t;
          "
        )
      } else {
        sql <- glue::glue(
          "
          CREATE TEMP TABLE new_alloc_params_t AS
          SELECT
            id_trans, id_period,
            map_from_entries(alloc_params) as alloc_params,
            map_from_entries(goodness_of_fit) as goodness_of_fit
          FROM tmp_table;

          COPY new_alloc_params_t
          TO '{file_info$path}' ({self$writeopts});

          DROP TABLE new_alloc_params_t;
          "
        )
      }

      DBI::dbExecute(self$connection, sql)
    }
  ),

  ## Private Methods ----
  private = list(
    # r6 hook called on gc(); Close the database connection
    #
    # return NULL (called for side effects)
    finalize = function() {
      if (!is.null(self$connection)) {
        DBI::dbDisconnect(self$connection)
        self$connection <- NULL
      }
    },

    # Get file path and format for a table
    #
    # param table_name Character string table name
    # return List with path, format, and exists flag
    get_file_path = function(table_name) {
      # Check for parquet first, then csv
      parquet_path <- file.path(self$path, paste0(table_name, ".parquet"))
      csv_path <- file.path(self$path, paste0(table_name, ".csv"))

      if (file.exists(parquet_path)) {
        return(list(path = parquet_path, format = "parquet", exists = TRUE))
      } else if (file.exists(csv_path)) {
        return(list(path = csv_path, format = "csv", exists = TRUE))
      } else {
        # Return default format for new file
        default_path <- file.path(
          self$path,
          paste0(table_name, ".", self$default_format)
        )
        return(list(
          path = default_path,
          format = self$default_format,
          exists = FALSE
        ))
      }
    },

    # Write data to file
    #
    # param x Data frame to write
    # param table_name Character string table name
    write_file = function(x, table_name) {
      file_path <- file.path(self$path, paste0(table_name, ".", self$default_format))

      duckdb::duckdb_register(self$connection, "temp_write_table", x)
      on.exit(duckdb::duckdb_unregister(self$connection, "temp_write_table"))

      sql <- glue::glue("COPY temp_write_table TO '{file_path}' ({self$writeopts})")
      DBI::dbExecute(self$connection, sql)
    },

    # Set one duckdb variable name max_{colname} to the maximum found for each
    # autoincrement_col in table_name. if column is missing from table_name, set to 0.
    set_autoincrement_vars = function(table_name, autoincrement_cols) {
      if (length(autoincrement_cols) == 0L) {
        return(NULL)
      }
      if (!DBI::dbExistsTable(self$connection, table_name)) {
        # attach / detach unless it was already there
        self$attach_table(table_name)
        on.exit(self$detach_table(table_name))
      }

      existing_cols <-
        glue::glue("select column_name from (describe {table_name})") |>
        self$get_query() |>
        purrr::pluck(1) |>
        intersect(autoincrement_cols)

      missing_cols <- setdiff(autoincrement_cols, existing_cols)

      set_exprs <- glue::glue_collapse(
        c(
          glue::glue(
            "set variable max_{existing_cols} = (select coalesce(max({existing_cols}), 0) from {table_name});"
          ),
          glue::glue(
            "set variable max_{missing_cols} = 0;"
          )
        ),
        sep = "\n"
      )

      self$execute(set_exprs)
    },

    # Fetch lulc_meta_long_v view
    fetch_lulc_meta_long_v = function(where = NULL, limit = NULL) {
      file_info <- private$get_file_path("lulc_meta_t")

      if (!file_info$exists) {
        return(data.table::data.table(
          id_lulc = integer(0),
          name = character(0),
          src_class = integer(0)
        ))
      }

      sql <- glue::glue(
        "
        SELECT
          id_lulc,
          name,
          unnest(src_classes) as src_class
        FROM read_{file_info$format}('{file_info$path}')
      "
      )

      if (!is.null(where)) {
        sql <- glue::glue("{sql} WHERE {where}")
      }
      if (!is.null(limit)) {
        sql <- glue::glue("{sql} LIMIT {limit}")
      }

      DBI::dbGetQuery(self$connection, sql) |>
        data.table::as.data.table()
    },

    # Fetch pred_sources_v view
    fetch_pred_sources_v = function(where = NULL, limit = NULL) {
      file_info <- private$get_file_path("pred_meta_t")

      if (!file_info$exists) {
        return(data.table::data.table(
          url = character(0),
          md5sum = character(0)
        ))
      }

      sql <- glue::glue(
        "
        SELECT DISTINCT
          unnest(sources).url AS url,
          unnest(sources).md5sum AS md5sum
        FROM read_{file_info$format}('{file_info$path}')
        WHERE sources IS NOT NULL
      "
      )

      if (!is.null(where)) {
        sql <- glue::glue("SELECT * FROM ({sql}) subq WHERE {where}")
      }
      if (!is.null(limit)) {
        sql <- glue::glue("{sql} LIMIT {limit}")
      }

      DBI::dbGetQuery(self$connection, sql) |>
        data.table::as.data.table()
    },

    # Get empty table with proper structure
    #
    # param table_name Character string table name
    # return Empty data.table with correct columns
    get_empty_table = function(table_name) {
      # Define empty table structures
      empty_tables <- list(
        reporting_t = suppressWarnings(data.table::data.table(
          key = character(0),
          value = character(0)
        )),
        coords_t = as_coords_t(),
        periods_t = as_periods_t(),
        lulc_meta_t = as_lulc_meta_t(),
        lulc_data_t = as_lulc_data_t(),
        pred_meta_t = as_pred_meta_t(),
        pred_data_t_float = as_pred_data_t(type = "float"),
        pred_data_t_int = as_pred_data_t(type = "int"),
        pred_data_t_bool = as_pred_data_t(type = "bool"),
        trans_meta_t = as_trans_meta_t(),
        trans_preds_t = as_trans_preds_t(),
        intrv_meta_t = as_intrv_meta_t(),
        intrv_masks_t = as_intrv_masks_t(),
        trans_models_t = as_trans_models_t(),
        alloc_params_t = as_alloc_params_t()
      )

      if (table_name %in% names(empty_tables)) {
        return(empty_tables[[table_name]])
      }

      # Default: return empty data.table
      data.table::data.table()
    },

    # Get maximum ID values for auto-increment columns
    #
    # param file_info List with path and format information
    # param id_cols Character vector of ID column names
    # return Named integer vector of max values for each column
    get_max_ids = function(file_info, id_cols) {
      # First check which columns exist in the file

      existing_cols <-
        glue::glue("SELECT * FROM read_{file_info$format}('{file_info$path}') LIMIT 0") |>
        DBI::dbGetQuery(self$connection, statement = _) |>
        names()

      # Filter to only columns that exist
      cols_to_query <- intersect(id_cols, existing_cols)

      # Initialize result with 0L for all requested columns
      max_vals <- stats::setNames(rep(0L, length(id_cols)), id_cols)

      # Only query for columns that exist
      if (length(cols_to_query) > 0) {
        max_exprs <- sprintf("MAX(%s) as %s", cols_to_query, cols_to_query)
        sql <- glue::glue(
          "SELECT {paste(max_exprs, collapse = ', ')}
           FROM read_{file_info$format}('{file_info$path}')"
        )

        result <- DBI::dbGetQuery(self$connection, sql)

        # Update max_vals for columns that exist
        for (col in cols_to_query) {
          val <- result[[col]]
          max_vals[[col]] <- if (is.na(val)) 0L else as.integer(val)
        }
      }

      return(max_vals)
    }
  )
)

# Utility function to assign auto-increment IDs to a data.table by reference
#
# param x data.table to update by reference
# param autoincrement_cols Character vector of column names to auto-increment
# param existing_max_ids Optional named integer vector of max IDs from existing table
# return x invisibly (modified by reference)
assign_autoincrement_ids <- function(x, autoincrement_cols, existing_max_ids = NULL) {
  if (length(autoincrement_cols) == 0L) {
    return(invisible(x))
  }

  stopifnot(inherits(x, "data.table")) # better safe than sorry

  for (col in autoincrement_cols) {
    # Determine starting point from existing table (i.e. read from file)
    if (!is.null(existing_max_ids) && col %in% names(existing_max_ids)) {
      existing_max <- existing_max_ids[[col]]
    } else {
      existing_max <- 0L
    }

    # Get max from current data (i.e. object x, if column exists and has non-NA values)
    if (hasName(x, col)) {
      data_max <- suppressWarnings(max(x[[col]], na.rm = TRUE))
      if (!is.finite(data_max)) data_max <- 0L
    } else {
      data_max <- 0L
      # Add column if it doesn't exist - use set() for by-reference modification
      data.table::set(x, j = col, value = NA_integer_)
    }

    # Start from the higher of the two
    start_id <- max(existing_max, data_max) + 1L

    # Count how many NAs need to be filled
    na_count <- sum(is.na(x[[col]]))

    if (na_count > 0L) {
      new_seq <- seq.int(from = start_id, length.out = na_count)
      data.table::set(x, i = which(is.na(x[[col]])), j = col, value = new_seq)
    }
  }

  invisible(x)
}

# Helper function to create simple Bindings with standard fetch/commit pattern
# param self The R6 instance (self).
# param table_name Character string. Name of the table.
# param as_fn Function to convert fetched data to the appropriate type.
# param ... Additional arguments passed to as_fn.
# return A function suitable for use as an active binding.
create_active_binding <- function(self, table_name, as_fn, ...) {
  extra_args <- list(...)
  function(x) {
    if (missing(x)) {
      x <- self$fetch(table_name)
      return(do.call(as_fn, c(list(x), extra_args)))
    }
    stopifnot(inherits(x, table_name))
    self$commit_upsert(x, table_name)
  }
}


# Helper functions for converting between list and data.frame formats for DuckDB MAPs
list_to_kv_df <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(data.frame(
      key = character(0),
      value = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    key = names(x),
    value = as.character(unlist(x)),
    stringsAsFactors = FALSE
  )
}

kv_df_to_list <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(NULL)
  }

  out <- list()

  for (row in seq_len(nrow(x))) {
    key <- x$key[row]
    val <- x$value[row]

    # Try numeric conversion
    num_val <- suppressWarnings(as.numeric(val))
    if (!is.na(num_val)) {
      out[[key]] <- num_val
    } else {
      out[[key]] <- val
    }
  }

  out
}

# Compose a SQL select expression for use in commits
# param all_cols: char vector of all column names
# param row_num_cols: char vector of those columns that should be set to their row number
# param from_max_cols: char vector of those columns that should increment from a
# previously set max_{colname} duckdb variable
compose_select_expr <- function(
  all_cols,
  row_num_cols = character(),
  from_max_cols = character()
) {
  # find those columns that aren't auto-incrementing
  ordinary_cols <- setdiff(
    all_cols,
    union(row_num_cols, from_max_cols) # assuming these aren't overlapping
  )
  glue::glue_collapse(
    c(
      glue::glue("row_number() over () as {row_num_cols}"),
      glue::glue("row_number() over () + getvariable('max_{from_max_cols}') as {from_max_cols}"),
      ordinary_cols
    ),
    sep = ",\n "
  )
}
