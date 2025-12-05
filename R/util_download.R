#' Utilities to download and manage files
#'
#' In order to maintain a clean set of raw data files, these functions help with
#' retrieving the raw data from an online source and caching it.
#'
#' @name util_download
NULL

#' @describeIn util_download Download a set of files and check their integrity against
#'    their md5 checksum
#' @param df_in Data frame with 'url' and 'md5sum' columns
#' @param target_dir Target directory for downloads, defaults to option
#'    `evoland.cachedir`
#' @param overwrite Whether to overwrite existing files (default: FALSE)
#'
#' @return df_out, Same as df_in but with added column "local_filename", the full path
#' of which can be constructed
#' @export

download_and_verify <- function(
  df_in,
  target_dir = getOption("evoland.cachedir"),
  overwrite = FALSE
) {
  check_missing_names(df_in, c("url", "md5sum"))
  ensure_dir(target_dir)

  # makes a copy while ensuring DT semantics
  df_in <- data.table::as.data.table(df_in)
  # find pre-existing files
  all_dt <- df_in[,
    .(
      url,
      md5sum,
      found_files = lapply(file.path(target_dir, md5sum), list.files, full.names = TRUE)
    )
  ]

  # can only handle one file per md5 folder
  all_dt[, no_found_files := vapply(found_files, length, integer(1))]
  if (nrow(too_many_files <- all_dt[no_found_files > 1])) {
    stop(
      "Investigate: found more than one file for \n ",
      too_many_files[, "url"]
    )
  }

  # only where no file is found should we download, unless overwrite
  if (overwrite) {
    all_dt[, to_download := TRUE]
  } else {
    all_dt[, to_download := (no_found_files == 0)]
  }

  # if download, fetch to temp file in target_dir, then move to md5 folder once known
  # if not download, just return details on existing file, not rechecking md5sum
  downloaded_dt <- data.table::rbindlist(.mapply(
    dots = all_dt,
    MoreArgs = NULL,
    FUN = function(url, found_files, to_download, md5sum, ...) {
      if (to_download) {
        message("Downloading: ", url)
        temp_file <- tempfile(tmpdir = target_dir)
        res <- curl::curl_fetch_disk(url, temp_file)
        md5sum_actual <- tools::md5sum(temp_file)

        # the content-disposition header might contain a filename; else use basename
        filename_final <- filename_cd_header(res) %||% basename(url)
        md5dir <- ensure_dir(file.path(target_dir, md5sum_actual))
        target_path <- file.path(md5dir, filename_final)

        # move the file into place
        file.rename(temp_file, target_path)
      } else {
        message("Found ", found_files, "\nfor ", url)
        # we already checked that found_files is length 1
        filename_final <- basename(found_files)
        target_path <- found_files
        md5sum_actual <- md5sum
      }

      list(
        url = url,
        md5sum_actual = md5sum_actual,
        local_filename = filename_final,
        local_path = target_path
      )
    }
  ))

  # join initial assumptions about url/md5 to actual data
  df_out <- downloaded_dt[
    all_dt,
    .(
      url,
      md5sum,
      md5sum_actual,
      local_filename,
      local_path
    ),
    on = "url"
  ]

  # throw warning on mismatch (most likely file has changed but is still usable)
  if (nrow(mismatches <- df_out[md5sum != md5sum_actual])) {
    o <- options(datatable.prettyprint.char = 32L)
    on.exit(options(o))
    warning(
      "Hash mismatch for \n",
      paste(
        utils::capture.output(print_rowwise_yaml(mismatches)),
        collapse = "\n"
      )
    )
  }

  df_in[df_out, on = c("url", "md5sum")]
}

# parse a curl::curl_fetch_disk result and return a filename indicated by
# content-disposition or NULL
filename_cd_header <- function(res) {
  # pilfered from https://regex101.com/r/hJ7tS6/179
  filename_match <- r"{filename[^;=\n]*=(?:(\\?['"])(.*?)\1|(?:[^\s]+'.*?')?([^;\n]*))}"

  cd <-
    res[["headers"]] |>
    curl::parse_headers() |>
    stringi::stri_subset_fixed("Content-Disposition:") |>
    stringi::stri_match_all_regex(filename_match)

  if (length(cd) == 0L) {
    return(NULL)
  }

  # first row: first match. fourth column: third capture group.
  out <- cd[[1L]][1, 4]

  if (is.na(out)) {
    # second capture group in case the filename is quoted
    out <- cd[[1L]][1, 3]
    if (is.na(out)) {
      return(NULL)
    }
  }

  out
}
