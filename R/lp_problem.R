#' R6 Class for Sparse Linear Programs
#'
#' @description
#' Collects the constraints of a linear program in long form and hands them to
#' [lpSolve::lp()] in triplet form, which keeps memory linear in the number of non-zero
#' coefficients. Constraints are tagged with the block they belong to, so a program can be
#' assembled once and solved over a subset of its blocks.
#'
#' @details
#' Two tables describe a program.
#'
#' `variables` has one row per decision variable: an `id_var` giving its column in the
#' program, a `block` naming the group it belongs to, and whatever further key columns the
#' caller needs to identify it. Nothing about the meaning of a variable is known here; the
#' subclass supplies it.
#'
#' `constraints` has one row per non-zero coefficient: `block`, `id_row`, `id_var`,
#' `coefficient`, plus the `dir` and `rhs` of the row it belongs to, which are constant
#' within an `id_row`. Coefficients are summed over duplicated `(id_row, id_var)` pairs and
#' exact zeroes are dropped, since [lpSolve::lp()] matches constraint rows to `dir`/`rhs` by
#' the order of the row indices it is given, and cannot represent an empty row.
#'
#' All variables are non-negative: [lpSolve::lp()] has no notion of variable bounds, so a
#' quantity that may take either sign has to be split into two variables.
#'
#' `lpSolve` is a suggested dependency, since most of the package does not solve linear
#' programs. It has to be installed before a program can be constructed.
#'
#' @seealso [trans_rate_lp]
#' @export
lp_problem <- R6::R6Class(
  classname = "lp_problem",

  public = list(
    #' @description Initialize a program over a fixed set of decision variables. Fails if
    #' the suggested `lpSolve` package is not installed, since nothing could be solved.
    #' @param variables A data.table with `id_var` (`1:n`, in order) and `block`, plus any
    #' key columns identifying each variable.
    #' @return A new `lp_problem` object
    initialize = function(variables) {
      require_suggested("lpSolve", "build a linear program")

      variables <- data.table::as.data.table(variables)
      stopifnot(
        "variables needs id_var and block columns" = all(
          c("id_var", "block") %in% names(variables)
        ),
        "id_var must be 1:n in order" = identical(
          variables[["id_var"]],
          seq_len(nrow(variables))
        )
      )

      private$.variables <- variables
      private$.constraints <- data.table::data.table(
        block = character(0),
        id_row = integer(0),
        id_var = integer(0),
        coefficient = numeric(0),
        dir = character(0),
        rhs = numeric(0)
      )
      invisible(self)
    },

    #' @description Add one block of constraint rows.
    #' @param block Name of the block, used to subset the program when solving.
    #' @param constraints A data.table with `id_row`, `id_var`, `coefficient`, `dir` and
    #' `rhs`. `id_row` only has to be unique within this call; it is renumbered to stay
    #' unique across blocks. `dir` and `rhs` must be constant within an `id_row`.
    #' @return The `lp_problem` object, invisibly
    add_constraints = function(block, constraints) {
      constraints <- data.table::as.data.table(constraints)
      stopifnot(
        "block must be a single name" = is.character(block) && length(block) == 1L,
        "block was already added" = !(block %chin% private$.constraints[["block"]]),
        "constraints needs id_row, id_var, coefficient, dir and rhs" = all(
          c("id_row", "id_var", "coefficient", "dir", "rhs") %in% names(constraints)
        ),
        "dir must be one of <=, >= or =" = all(constraints[["dir"]] %chin% c("<=", ">=", "=")),
        "constraints reference unknown variables" = all(
          constraints[["id_var"]] %in% private$.variables[["id_var"]]
        ),
        "dir and rhs must be constant within a row" = nrow(unique(
          constraints[, .(id_row, dir, rhs)]
        )) ==
          data.table::uniqueN(constraints[["id_row"]])
      )

      collapsed <-
        constraints[,
          .(coefficient = sum(coefficient)),
          by = .(id_row, id_var, dir, rhs)
        ][
          coefficient != 0
        ]
      stopifnot(
        "a constraint row has no non-zero coefficient" = data.table::uniqueN(
          collapsed[["id_row"]]
        ) ==
          data.table::uniqueN(constraints[["id_row"]])
      )

      row_offset <- private$last_id_row()
      collapsed[, id_row := .GRP + row_offset, by = id_row]
      data.table::set(collapsed, j = "block", value = block)

      private$.constraints <- rbind(
        private$.constraints,
        collapsed[, .(block, id_row, id_var, coefficient, dir, rhs)]
      )
      invisible(self)
    },

    #' @description Solve the program and store the solution.
    #' @param objective A data.table with `id_var` and `coefficient`; variables it does not
    #' mention do not enter the objective.
    #' @param direction `"min"` or `"max"`.
    #' @param blocks Constraint blocks to include; all of them when `NULL`.
    #' @return The `lp_problem` object, invisibly
    solve = function(objective, direction = "min", blocks = NULL) {
      solution <- private$run(objective, direction, blocks)
      private$.status <- solution[["status"]]
      private$.objective <- solution[["objval"]]
      private$.values <- data.table::data.table(
        id_var = private$.variables[["id_var"]],
        value = solution[["solution"]]
      )
      invisible(self)
    },

    #' @description Print a summary of the program.
    #' @param ... Ignored.
    print = function(...) {
      cat(glue::glue(
        "<{class(self)[1]}>\n",
        "{nrow(private$.variables)} variables, {self$n_row} constraint rows, ",
        "{nrow(private$.constraints)} non-zero coefficients\n",
        "status: {private$.status %||% 'unsolved'}\n\n"
      ))
      print(self$block_summary)
      invisible(self)
    }
  ),

  active = list(
    #' @field variables The decision variables, one row each.
    variables = function() private$.variables[],

    #' @field constraints The constraint coefficients, one row per non-zero entry.
    constraints = function() private$.constraints[],

    #' @field block_summary Rows and coefficients per constraint block.
    block_summary = function() {
      private$.constraints[,
        .(n_row = data.table::uniqueN(id_row), n_coefficient = .N),
        by = block
      ]
    },

    #' @field n_var Number of decision variables.
    n_var = function() nrow(private$.variables),

    #' @field n_row Number of constraint rows.
    n_row = function() data.table::uniqueN(private$.constraints[["id_row"]]),

    #' @field status The [lpSolve::lp()] status of the last solve; 0 is success.
    status = function() private$.status,

    #' @field objective The objective value of the last solve.
    objective = function() private$.objective,

    #' @field values The solved value of every variable, joined to its keys.
    values = function() {
      stopifnot("the program has not been solved" = !is.null(private$.values))
      private$.variables[private$.values, on = "id_var"]
    }
  ),

  private = list(
    .variables = NULL,
    .constraints = NULL,
    .values = NULL,
    .status = NULL,
    .objective = NULL,

    # highest row index handed out so far, so blocks stay disjoint
    last_id_row = function() {
      if (nrow(private$.constraints) == 0L) 0L else max(private$.constraints[["id_row"]])
    },

    # solve without storing, so that a subclass can run auxiliary programs over a subset of
    # the blocks without discarding the solution of the main one
    run = function(objective, direction = "min", blocks = NULL) {
      constraints <- private$.constraints
      if (!is.null(blocks)) {
        constraints <- constraints[block %chin% blocks]
      }
      stopifnot("no constraints to solve" = nrow(constraints) > 0L)
      data.table::setorder(constraints, id_row, id_var)

      rows <- unique(constraints, by = "id_row")
      coefficients <- numeric(nrow(private$.variables))
      coefficients[objective[["id_var"]]] <- objective[["coefficient"]]

      lpSolve::lp(
        direction = direction,
        objective.in = coefficients,
        const.dir = rows[["dir"]],
        const.rhs = rows[["rhs"]],
        # lpSolve numbers its rows 1:n, so a block left out has to close the gap it leaves
        dense.const = as.matrix(constraints[,
          .(id_row = data.table::rleid(id_row), id_var, coefficient)
        ])
      )
    }
  )
)
