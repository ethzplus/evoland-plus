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
  df_out <- df_in
  df_out[["local_filename"]] <- NA_character_

  for (i in seq_len(nrow(df_in))) {
    url <- df_in[["url"]][i]
    expected_md5 <- df_in[["md5sum"]][i]

    md5dir <- ensure_dir(file.path(target_dir, expected_md5))
    present <- list.files(md5dir)

    if (length(present) > 1L) {
      stop("Investigate: found more than one file in ", md5dir)
    } else if (length(present) == 1L && !overwrite) {
      message(glue::glue(
        "Found file `{present}`
        for {url}
         - skipping download"
      ))
      filename_final <- df_out[["local_filename"]][i] <-
        present
    } else {
      message("Downloading: ", url)
      temp_file <- tempfile(tmpdir = md5dir)
      res <- curl::curl_fetch_disk(url, temp_file)
      filename_cd <- filename_cd_header(res)
      if (is.null(filename_cd)) {
        filename_final <- basename(url)
      } else {
        filename_final <- filename_cd
      }
      file.rename(
        temp_file,
        file.path(md5dir, filename_final)
      )

      df_out[["local_filename"]][i] <- filename_final
    }

    # Verify MD5
    filepath <- df_out[["local_path"]][i] <-
      file.path(md5dir, filename_final)
    actual_md5 <- tools::md5sum(filepath)
    if (actual_md5 != expected_md5) {
      stop(glue::glue(
        "Hash mismatch for {url}
        Expected: {expected_md5},
        Got: {actual_md5}
        Stopping for safety, please verify file integrity at
        {filepath}"
      ))
    }
  }

  return(df_out)
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
