# Test cases for evoland:::filename_cd_header function
library(tinytest)

# Helper function to create mock curl response
create_mock_response <- function(content_disposition) {
  list(
    headers = charToRaw(paste0(
      "HTTP/1.1 200 OK\r\n",
      "Content-Type: application/octet-stream\r\n",
      "Content-Disposition: ",
      content_disposition,
      "\r\n",
      "\r\n"
    ))
  )
}

# Test cases with expected filenames
test_cases <- list(
  list(
    cd = "attachment; filename=content.txt",
    expected = "content.txt"
  ),
  list(
    cd = "attachment; filename*=UTF-8''filename.txt",
    expected = "filename.txt"
  ),
  list(
    cd = "attachment; filename=\"EURO rates\"; filename*=utf-8''%e2%82%ac%20rates",
    expected = "EURO rates"
  ),
  list(
    cd = "attachment; filename=EURO rates; filename*=utf-8''%e2%82%ac%20rates",
    expected = "EURO rates"
  ),
  list(
    cd = "attachment; filename=\"omÃ¡Ã¨ka.jpg\"",
    expected = "omÃ¡Ã¨ka.jpg"
  ),
  list(
    cd = "attachment; filename=\\\"omÃ¡Ã¨ka.jpg\\\"",
    expected = "omÃ¡Ã¨ka.jpg"
  ),
  list(
    cd = "attachment; filename*=iso-8859-1'en'%A3%20rates",
    expected = "%A3%20rates"
  ),
  list(
    cd = "attachment; filename=EXAMPLE- I'm ößä.dat; filename*=iso-8859-1''EXAMPLE-%20I%27m%20%F6%DF%E4.dat", # nolint
    expected = "EXAMPLE- I'm ößä.dat"
  ),
  list(
    cd = "attachment; filename*=UTF-8''0004-1-002-01-datasheet.pdf;",
    expected = "0004-1-002-01-datasheet.pdf"
  ),
  list(
    cd = "attachment; filename=ag-b-00.03-37-area-all-csv-APPENDIX.zip",
    expected = "ag-b-00.03-37-area-all-csv-APPENDIX.zip"
  )
)

# Run tests in a loop
for (i in seq_along(test_cases)) {
  test_case <- test_cases[[i]]

  # Create mock response
  mock_response <- create_mock_response(test_case$cd)

  # Test the function
  result <- evoland:::filename_cd_header(mock_response)

  # Check if result matches expected
  expect_equal(
    result,
    test_case$expected,
    info = paste("Test case", i, ":", test_case$cd)
  )
}

# Test case for no Content-Disposition header
mock_response_no_cd <- list(
  headers = charToRaw("HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\n\r\n")
)

expect_null(
  evoland:::filename_cd_header(mock_response_no_cd),
  info = "Should return NULL when no Content-Disposition header"
)

# Test case for Content-Disposition without filename
mock_response_no_filename <- create_mock_response("inline")
expect_null(
  evoland:::filename_cd_header(mock_response_no_filename),
  info = "Should return NULL when Content-Disposition has no filename"
)

# test actual transfer
dir <- tempdir()
df_in <- data.frame(
  url = file.path("file:/", system.file("config.yaml", package = "evoland")),
  md5sum = tools::md5sum(system.file("config.yaml", package = "evoland"))
)
expect_message(
  df_out1 <- download_and_verify(df_in = df_in, target_dir = dir),
  "^Downloading:"
)
expect_message(
  df_out2 <- download_and_verify(df_in = df_in, target_dir = dir),
  "^Found file"
)
expect_equal(df_out1, df_out2)
unlink(
  file.path(dir, df_out2$local_path),
  recursive = TRUE
)

# test sources getter
expect_silent(
  sources_dt <-
    system.file("config.yaml", package = "evoland") |>
    read_evoland_config() |>
    get_sources_dt()
)

expect_equal(
  sources_dt[1, ],
  data.table::data.table(
    url = "https://dam-api.bfs.admin.ch/hub/api/dam/assets/32376216/appendix",
    md5sum = "c32937eb4a11fc9c5c58c66e9830360a",
    conf_key = "lulc_data"
  )
)
