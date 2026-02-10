#' Dinamica Utility Functions
#'
#' Interact with Dinamica from R, see **Functions** section below.
#'
#' @name util_dinamica
NULL

#' @describeIn util_dinamica Execute a Dinamica .ego file using `DinamicaConsole`
#' @param model_path Path to the .ego model file to run. Any submodels must be included
#' in a directory of the exact form `basename(modelpath)_ego_Submodels`, [see
#' wiki](https://csr.ufmg.br/dinamica/dokuwiki/doku.php?id=submodels)
#' @param disable_parallel Whether to disable parallel steps (default TRUE)
#' @param log_level Logging level (1-7, default NULL)
#' @param additional_args Additional arguments to pass to DinamicaConsole, see
#' `DinamicaConsole -help`
#' @param write_logfile bool, write stdout&stderr to a file?
#' @param echo bool, direct echo to console?
#'
#' @export

exec_dinamica <- function(
  model_path,
  disable_parallel = TRUE,
  log_level = NULL,
  additional_args = NULL,
  write_logfile = TRUE,
  echo = FALSE
) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop(
      "Package 'processx' is required for this function. ",
      "Please install it with: install.packages('processx')",
      call. = FALSE
    )
  }

  if (Sys.which("DinamicaConsole") == "") {
    stop(
      "DinamicaConsole not found on PATH. ",
      "Please ensure Dinamica EGO is installed and DinamicaConsole is available."
    )
  }
  args <- character()
  if (disable_parallel) {
    args <- c(args, "-disable-parallel-steps")
  }
  if (!is.null(log_level)) {
    args <- c(args, paste0("-log-level ", log_level))
  }
  if (!is.null(additional_args)) {
    args <- c(args, additional_args)
  }
  args <- c(args, model_path)

  if (write_logfile) {
    logfile_path <- file.path(
      dirname(model_path),
      format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss_dinamica.log")
    )
    message("Logging to ", logfile_path)

    # Use bash process substitution with sed to strip ANSI codes and tee to logfile
    res <- processx::run(
      command = "bash",
      args = c(
        "-c",
        sprintf(
          paste(
            "set -o pipefail;", # propagate dinamica fails to end of pipe
            "stdbuf -oL", # force flush - dinamica does not always flush its buffer before crashing
            "DinamicaConsole %s 2>&1 |", # redirect stderr to stdout
            "sed 's/\\x1b\\[[0-9;]*m//g' |", # strip ansi codes
            "tee '%s';", # write to stdout and file
            "exit ${PIPESTATUS[0]}"
          ),
          paste(shQuote(args), collapse = " "),
          logfile_path
        )
      ),
      error_on_status = FALSE,
      echo = echo,
      spinner = TRUE,
      env = c(
        "current",
        DINAMICA_HOME = dirname(model_path)
      )
    )
  } else {
    res <- processx::run(
      # If called directly, DinamicaConsole does not flush its buffer upon SIGTERM.
      # stdbuf -oL forces flushing the stdout buffer after every line.
      command = "stdbuf",
      args = c(
        "-oL",
        "DinamicaConsole", # assume that $PATH is complete
        args
      ),
      error_on_status = FALSE,
      echo = echo,
      spinner = TRUE,
      env = c(
        "current",
        DINAMICA_HOME = dirname(model_path)
      )
    )
  }

  if (
    res[["status"]] != 0L ||
      grepl("Dinamica EGO exited with an error", res[["stdout"]])
  ) {
    stop(
      "Dinamica registered an error. \n",
      "Rerun with echo = TRUE or check logfile to see what went wrong."
    )
  }

  invisible(res)
}

#' @describeIn util_dinamica Set up evoland-specific Dinamica EGO files; execute using
#' [exec_dinamica()]
#' @param run_modelprechecks bool, Validate that everything's in place for a model run.
#' Will never be run if calibration.
#' @param config List of config params
#' @param calibration bool, Is this a calibration run?
#' @param work_dir Working dir, where to place ego files and control table
#' @param ... passed to [exec_dinamica()]
#' @export
run_alloc_dinamica <- function(
  work_dir = format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"),
  ...
) {
  # find raw ego files with decoded R/Python code chunks
  decoded_files <-
    list.files(
      path = system.file("dinamica_models", package = "evoland"),
      full.names = TRUE,
      recursive = TRUE
    ) |>
    grep(pattern = "allocation.*\\.ego-decoded$", value = TRUE)

  lapply(decoded_files, function(decoded_file) {
    # Determine relative path and new output path with .ego extension
    base_dir <- system.file("dinamica_models", package = "evoland")
    rel_path <- substring(decoded_file, nchar(base_dir) + 2)
    out_path <- sub("\\.ego-decoded$", ".ego", file.path(work_dir, rel_path))
    ensure_dir(dirname(out_path))
    process_dinamica_script(decoded_file, out_path)
  })

  message("Starting to run model with Dinamica EGO")
  exec_dinamica(
    model_path = file.path(work_dir, "allocation.ego"),
    ...
  )
}

#' @describeIn util_dinamica Encode or decode raw R and Python code chunks in .ego
#' files and their submodels to/from base64
#' @param infile Input file path. Treated as input if passed AsIs using `base::I()`
#' @param outfile Output file path (optional)
#' @param mode Character, either "encode" or "decode"
#' @param check Default TRUE, simple check to ensure that you're handling what you're expecting

process_dinamica_script <- function(infile, outfile, mode = "encode", check = TRUE) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop(
      "Package 'base64enc' is required for this function. ",
      "Please install it with: install.packages('base64enc')",
      call. = FALSE
    )
  }

  mode <- match.arg(mode, c("encode", "decode"))
  if (inherits(infile, "AsIs")) {
    file_text <- unclass(infile)
  } else {
    # read the input file as a single string
    file_text <- readChar(infile, file.info(infile)$size)
  }

  # match the Calculate R or Python Expression blocks - guesswork involved
  pattern <- r'(:= Calculate(?:Python|R)Expression "(\X*?)" (?:\.no )?\{\{)'
  match_positions <- gregexpr(pattern, file_text, perl = TRUE)[[1]]
  if (match_positions[1] == -1) {
    # none found (position -1)
    matches <- character(0)
  } else {
    # extracts first the full match as char vect and then the captured group
    full_matches <- regmatches(file_text, match_positions)
    # Extract the first capture group for each match
    all_matches <- lapply(full_matches, function(m) {
      cap <- regmatches(m, regexec(pattern, m, perl = TRUE))[[1]]
      cap[2]
    })
    matches <- do.call(c, all_matches)
  }

  if (check) {
    non_base64_chars_present <- grepl("[^A-Za-z0-9+=\\n/]", matches)
    if (mode == "encode" && any(!non_base64_chars_present)) {
      stop(
        "There are no non-base64 chars in one of the matched patterns, which seems ",
        "unlikely for an unencoded code chunk. Override this check with ",
        "check = FALSE if you're sure that this is an unencoded file."
      )
    } else if (mode == "decode" && any(non_base64_chars_present)) {
      stop(
        "There are non-base64 chars in one of the matched patterns, which seems ",
        "unlikely for an encoded code chunk. Override this check with ",
        "check = FALSE if you're sure that this is an unencoded file."
      )
    }
  }

  if (length(matches) > 0) {
    encoder_decoder <- if (mode == "encode") {
      function(code) base64enc::base64encode(charToRaw(code))
    } else {
      function(code) rawToChar(base64enc::base64decode(code))
    }
    # matches contains the captured R/python code OR base64-encoded code
    encoded_vec <- vapply(matches, encoder_decoder, character(1), USE.NAMES = FALSE)
    # replace each original code with its base64 encoded version
    for (i in seq_along(encoded_vec)) {
      file_text <- sub(
        pattern = matches[i],
        replacement = encoded_vec[i],
        x = file_text,
        fixed = TRUE
      )
    }
  }

  # Write to outfile if specified, otherwise return the substituted string
  if (!missing(outfile)) {
    writeChar(file_text, outfile, eos = NULL)
    invisible(outfile)
  } else {
    file_text
  }
}
