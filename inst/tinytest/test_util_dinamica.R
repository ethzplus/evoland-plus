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

# Test: process_dinamica_script encodes correctly
expect_error(
  process_dinamica_script(
    I(sample_dinamica_script_encoded),
    mode = "encode"
  ),
  pattern = "seems unlikely for an unencoded code chunk"
)

expect_match(
  process_dinamica_script(
    I(sample_dinamica_script_encoded),
    mode = "decode"
  ),
  'output <- v1\\*2\\noutputDouble\\("output_number", output\\)'
)

# Test: process_dinamica_script decodes correctly
expect_error(
  process_dinamica_script(
    I(sample_dinamica_script_decoded),
    mode = "decode"
  ),
  pattern = "seems unlikely for an encoded code chunk"
)

expect_match(
  process_dinamica_script(
    I(sample_dinamica_script_decoded),
    mode = "encode"
  ),
  "c3RvcCgicnVuY2libGUgc3Bvb24iKQ=="
)

# Test: process_dinamica_script is idempotent
expect_equal(
  {
    sample_dinamica_script_encoded |>
      I() |>
      process_dinamica_script(mode = "decode") |>
      I() |>
      process_dinamica_script(mode = "encode")
  },
  sample_dinamica_script_encoded
)

expect_equal(
  {
    sample_dinamica_script_decoded |>
      I() |>
      process_dinamica_script(mode = "encode") |>
      I() |>
      process_dinamica_script(mode = "decode")
  },
  sample_dinamica_script_decoded
)

# Test: exec_dinamica works
tmpfile_ego <- tempfile(fileext = ".ego")
writeChar(
  sample_dinamica_script_encoded,
  tmpfile_ego,
  eos = NULL
)
expect_identical(
  exec_dinamica(tmpfile_ego)[["status"]],
  0L
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
process_dinamica_script(I(sample_dinamica_script_decoded), tmpfile_ego)
expect_error(
  exec_dinamica(tmpfile_ego, echo = TRUE),
  pattern = "runcible spoon"
)
unlink(tmpfile_ego)
