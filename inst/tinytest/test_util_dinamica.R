library(tinytest)

# nolint start
sample_dinamica_script_encoded <- '
@charset = UTF-8
@date = 2023-Oct-13 15:04:52
@version = 8.3
Script {{
    realValue1652 := RealValue 2;

    @collapsed = no
    calculateRexpression1653 := CalculateRExpression "b3V0cHV0IDwtIHYxKjIKb3V0cHV0RG91YmxlKCJvdXRwdXRfbnVtYmVyIiwgb3V0cHV0KQ==" .no {{
        NumberValue realValue1652 1;
    }};

    @viewer.number = yes
    _ := ExtractStructNumber calculateRexpression1653 $"(output_number)";
}};
'

sample_dinamica_script_decoded <- '
@charset = UTF-8
@date = 2023-Oct-13 15:04:52
@version = 8.3
Script {{
    realValue1652 := RealValue 2;

    @collapsed = no
    calculateRexpression1653 := CalculateRExpression "stop("runcible spoon")" .no {{
        NumberValue realValue1652 1;
    }};

    @viewer.number = yes
    _ := ExtractStructNumber calculateRexpression1653 $"(output_number)";
}};
'
# nolint end

# Test: evoland:::process_dinamica_script encodes correctly
expect_error(
  evoland:::process_dinamica_script(
    I(sample_dinamica_script_encoded),
    mode = "encode"
  ),
  pattern = "seems unlikely for an unencoded code chunk"
)

expect_match(
  evoland:::process_dinamica_script(
    I(sample_dinamica_script_encoded),
    mode = "decode"
  ),
  'output <- v1\\*2\\noutputDouble\\("output_number", output\\)'
)

# Test: evoland:::process_dinamica_script decodes correctly
expect_error(
  evoland:::process_dinamica_script(
    I(sample_dinamica_script_decoded),
    mode = "decode"
  ),
  pattern = "seems unlikely for an encoded code chunk"
)

expect_match(
  evoland:::process_dinamica_script(
    I(sample_dinamica_script_decoded),
    mode = "encode"
  ),
  "c3RvcCgicnVuY2libGUgc3Bvb24iKQ=="
)

# Test: evoland:::process_dinamica_script is idempotent
expect_equal(
  {
    sample_dinamica_script_encoded |>
      I() |>
      evoland:::process_dinamica_script(mode = "decode") |>
      I() |>
      evoland:::process_dinamica_script(mode = "encode")
  },
  sample_dinamica_script_encoded
)

expect_equal(
  {
    sample_dinamica_script_decoded |>
      I() |>
      evoland:::process_dinamica_script(mode = "encode") |>
      I() |>
      evoland:::process_dinamica_script(mode = "decode")
  },
  sample_dinamica_script_decoded
)

if (Sys.which("DinamicaConsole") != "") {
  stopifnot(
    !is.na(Sys.getenv("DINAMICA_EGO_8_INSTALLATION_DIRECTORY", unset = NA)),
    !is.na(Sys.getenv("DINAMICA_EGO_CLI", unset = NA)),
    !is.na(Sys.getenv("DINAMICA_EGO_8_HOME", unset = NA))
  )
  Sys.setenv(
    "DINAMICA_EGO_8_TEMP_DIR" = tempdir()
  )
  # Test: exec_dinamica works
  tmpfile_ego <- tempfile(fileext = ".ego")
  writeChar(
    sample_dinamica_script_encoded,
    tmpfile_ego,
    eos = NULL
  )
  expect_message(
    expect_length(
      exec_dinamica(tmpfile_ego),
      4 # list of status, stdout, stderr, timeout
    ),
    "Logging to"
  )
  unlink(tmpfile_ego)

  # Test: exec_dinamica with echo
  tmpfile_ego <- tempfile(fileext = ".ego")
  writeChar(
    sample_dinamica_script_encoded,
    tmpfile_ego,
    eos = NULL
  )
  expect_stdout(
    exec_dinamica(tmpfile_ego, echo = TRUE, write_logfile = FALSE),
    "Running model script"
  )
  unlink(tmpfile_ego)

  # Test: exec_dinamica fails
  tmpfile_ego <- tempfile(fileext = ".ego")
  evoland:::process_dinamica_script(I(sample_dinamica_script_decoded), tmpfile_ego)

  # capture the R error for the Dinamica CalculateRExpression (via stdout)
  expect_stdout(
    # silence the logging message within this calling process
    expect_message(
      # capture the R error within this calling process
      expect_error(
        exec_dinamica(tmpfile_ego, echo = TRUE),
        "Dinamica registered an error"
      )
    ),
    pattern = "Error caught in R execution: 'runcible spoon'"
  )
  unlink(tmpfile_ego)
}
