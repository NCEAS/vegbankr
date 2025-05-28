test_that("default vb_base_url is used", {
  local_base_url(NULL)
  expect_identical(get_vb_base_url(), "https://api.vegbank.org")
  suppressMessages(set_vb_base_url(""))
  expect_identical(get_vb_base_url(), "https://api.vegbank.org")
})
test_that("vb_base_url option set", {
  local_base_url(NULL)
  suppressMessages(set_vb_base_url("http://localhost", 8080))
  expect_identical(get_vb_base_url(), "http://localhost:8080")
})
test_that("set_vb_base_url(NULL) reports default URL", {
  local_base_url(NULL)
  expect_message(set_vb_base_url(NULL),
                 "Using https://api.vegbank.org as base URL")
})

test_that("canonicalize_names() works", {
  # Input with all names matched in the package lookup table
  input_df <- data.frame(
    stratum_ID = integer(),
    stratummethodname = character()
  )
  output_df <- canonicalize_names(input_df)
  expect_s3_class(output_df, "data.frame")
  expect_named(
    output_df,
    c("stratum_id", "stratum_method_name"),
    ignore.order = TRUE
  )
  # Input with a name missing from the package lookup table
  input_df$unexpectedname <- character()
  expect_warning(
     canonicalize_names(input_df),
     "^Unmatched names: unexpectedname$"
  )
  # Pass in custom name lookup table
  input_df <- data.frame(customfield = character())
  lookup_df <- data.frame(
    lower = 'customfield',
    snake = 'custom_field'
  )
  output_df <- canonicalize_names(input_df, lookup_df)
  expect_s3_class(output_df, "data.frame")
  expect_named(output_df, "custom_field")
})
